package riscv.integration.pipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.cache.CacheConfig
import riscv.pipeline.Pipeline3
import java.io.{File, PrintWriter}
import scala.collection.mutable

/** Custo do FENCE.I visto pelo programa: le `mcycle` antes e depois do FENCE.I.
  *
  * O numero medido inclui tudo o que o programa paga: o flush das linhas sujas
  * da DCache, a invalidacao da ICache, a parada do pipeline e a busca de novo
  * da instrucao seguinte (ICache vazia). Para isolar o custo do FENCE.I do
  * custo das instrucoes da janela, cada latencia tem uma execucao de controle,
  * igual mas com um NOP no lugar do FENCE.I.
  *
  * Checagens exatas: a inclinacao por linha suja e (latencia + 2), igual a da
  * previsao do cache (ver CacheMemoryLatencyTest); o custo fixo e so informado.
  */
class Pipeline3FenceICostSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 (custo do FENCE.I por mcycle)"

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def sw(rs2: Int, rs1: Int, imm: Int): Long =
    (((imm >> 5) & 0x7f).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (0x2L << 12) | ((imm & 0x1f).toLong << 7) | 0x23L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private val FenceI = 0x0000100fL
  private val NOP = 0x00000013L
  private val jal0 = 0x0000006fL // jal x0, 0
  private val Mcycle = 0xb00

  private val Cfg = CacheConfig(capacityBytes = 1024, ways = 4, lineBytes = 16)
  private val MemWords = 512 // 2 KiB: codigo em 0x000.., dados em 0x400..0x7FF
  private val DataBase = 0x400
  private val Latencies = Seq(0, 1, 3, 6)
  private val Dirties =
    Seq(0, 1, 2, Cfg.numLines) // zero, uma, duas e cache cheia

  /** `dirty` stores em linhas distintas (todas sujas e sem despejo), depois a
    * janela medida: csrr x5 ; FENCE.I (ou NOP) ; csrr x6. A janela tem sempre o
    * mesmo alinhamento (FENCE.I no inicio de uma linha da ICache).
    */
  private def program(dirty: Int, withFence: Boolean): Seq[Long] = {
    val setup = Seq(addi(2, 0, DataBase), addi(3, 0, 0x55))
    val stores = Seq.fill(dirty)(Seq(sw(3, 2, 0), addi(2, 2, 16))).flatten
    val head = setup ++ stores
    val pad = Seq
      .fill((3 - head.size % 4 + 4) % 4)(NOP) // csrr x5 em indice = 3 (mod 4)
    head ++ pad ++ Seq(
      csrrs(5, Mcycle, 0), // x5 = mcycle antes
      if (withFence) FenceI else NOP,
      csrrs(6, Mcycle, 0), // x6 = mcycle depois
      jal0
    )
  }

  /** Devolve x6 - x5: ciclos da janela (leitura de mcycle no ID). */
  private def window(dirty: Int, withFence: Boolean, latency: Int): Int = {
    val w = mutable.Map.empty[Int, BigInt]
    test(
      new Pipeline3(
        initialProgram = program(dirty, withFence),
        memoryWords = MemWords,
        memoryLatency = latency,
        cacheConfig = Cfg
      )
    ) { dut =>
      dut.clock.setTimeout(0) // so ha peeks: o limite padrao e 1000 ciclos
      var cycles = 0
      while (!(w.contains(5) && w.contains(6))) {
        require(
          cycles < 12000,
          s"timeout (dirty=$dirty, fence=$withFence, L=$latency)"
        )
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          if ((rd == 5 || rd == 6) && !w.contains(rd))
            w += rd -> dut.io.writebackData.peekInt()
        }
        dut.clock.step(1)
        cycles += 1
      }
    }
    (w(6) - w(5)).toInt
  }

  private case class Row(
      latency: Int,
      dirty: Int,
      total: Int, // janela com FENCE.I (o que o programa le em mcycle)
      control: Int, // mesma janela com NOP no lugar
      fence: Int // total - control: custo do FENCE.I
  )
  private val rows = mutable.ListBuffer[Row]()

  for (latency <- Latencies) {
    it should s"medir o custo total do FENCE.I e conferir a inclinacao L+2 por linha suja (latencia $latency)" in {
      val control = window(0, withFence = false, latency)
      val totals =
        Dirties.map(d => d -> window(d, withFence = true, latency)).toMap
      val failures = mutable.ListBuffer[String]()
      val fixed = Cfg.numSets + 4 // varredura + handshake, sem linhas sujas

      for (d <- Dirties) {
        val row = Row(latency, d, totals(d), control, totals(d) - control)
        rows += row
        println(
          f"[fence.i/pipeline] latencia=$latency%d sujas=$d%3d mcycle: janela=${row.total}%5d controle=$control%4d custo do FENCE.I=${row.fence}%5d (flush previsto ${fixed + d * (latency + 2)}%5d)"
        )
        if (d > 0) {
          val extra = totals(d) - totals(0)
          if (extra != d * (latency + 2))
            failures += s"L=$latency sujas=$d: acrescimo $extra, esperado ${d * (latency + 2)}"
        }
      }
      // O custo de D=0 tem de cobrir ao menos a varredura e o handshake; o que
      // sobra e o custo do pipeline (parada + busca da instrucao seguinte).
      val restart = (totals(0) - control) - fixed
      println(
        s"[fence.i/pipeline] latencia=$latency: custo fixo do pipeline alem do flush = $restart ciclos"
      )
      if (restart < 0)
        failures += s"L=$latency: custo com 0 linhas sujas (${totals(0) - control}) menor que a varredura ($fixed)"
      assert(failures.isEmpty, failures.mkString("\n"))
    }
  }

  it should "gravar a medicao em target/fence-i-pipeline.csv" in {
    val file = new File("target/fence-i-pipeline.csv")
    file.getParentFile.mkdirs()
    val out = new PrintWriter(file)
    try {
      out.println(
        "latencia,linhas_sujas,janela_com_fence,janela_controle,custo_fence"
      )
      rows.foreach(r =>
        out.println(
          s"${r.latency},${r.dirty},${r.total},${r.control},${r.fence}"
        )
      )
    } finally out.close()
    assert(file.exists())
  }
}
