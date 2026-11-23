package riscv.integration.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import riscv.pipeline.Pipeline3

/** mcycle/minstret e os contadores das caches lidos como CSRs.
  *
  * Um unico programa (uma compilacao do Pipeline3) le os contadores com CSRRS e
  * o teste confere os valores no log de writeback. A memoria tem latencia 2,
  * entao as caches frias dao varios ciclos de espera, que nao podem contar
  * instrucoes a mais em minstret.
  */
class Pipeline3CounterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Contadores como CSRs"

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x03L
  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x1L << 12) | (rd.toLong << 7) | 0x73L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private def csrrwi(rd: Int, csr: Int, zimm: Int): Long =
    (csr.toLong << 20) | (zimm.toLong << 15) | (0x5L << 12) | (rd.toLong << 7) | 0x73L
  private val NOP = 0x00000013L
  private val jal0 = 0x0000006fL // jal x0, 0

  private val MCYCLE = 0xb00
  private val MINSTRET = 0xb02
  private val MINSTRETH = 0xb82
  private val MCOUNTEREN = 0x306
  private val INSTRET = 0xc02 // espelho de usuario de minstret
  private val HPM_ICACHE_MISSES = 0xb04 // mhpmcounter4
  private val HPM_DCACHE_HITS = 0xb06 // mhpmcounter6
  private val HPM_DCACHE_MISSES = 0xb07 // mhpmcounter7
  private val HPM_DCACHE_MISSES_USER = 0xc07 // hpmcounter7 (espelho)

  private val program: Seq[Long] =
    Seq(csrrs(10, MCYCLE, 0)) ++ // x10 = mcycle (c1)
      Seq.fill(20)(addi(5, 5, 1)) ++
      Seq(csrrs(11, MCYCLE, 0)) ++ // x11 = mcycle (c2)
      Seq(csrrs(12, MINSTRET, 0)) ++ // x12 = minstret (i1)
      Seq.fill(10)(addi(6, 6, 1)) ++
      Seq(
        csrrs(13, MINSTRET, 0), // x13 = minstret (i2): i2 - i1 == 11
        csrrs(14, INSTRET, 0), // x14 = instret (espelho) == i2 + 1
        csrrs(15, MINSTRETH, 0), // x15 = metade alta == 0
        csrrwi(0, MCOUNTEREN, 7), // mcounteren = 7
        csrrs(21, MCOUNTEREN, 0), // x21 == 7
        addi(2, 0, 0x300),
        lw(17, 2, 0), // miss de DCache
        lw(18, 2, 4), // hit (mesma linha)
        NOP, // deixa os contadores assentarem (o hit conta no fim do ciclo)
        NOP,
        csrrs(19, HPM_DCACHE_MISSES, 0), // x19 == 1
        csrrs(20, HPM_DCACHE_HITS, 0), // x20 == 1
        csrrs(22, HPM_DCACHE_MISSES_USER, 0), // x22 == 1
        csrrs(23, HPM_ICACHE_MISSES, 0), // x23 >= 1
        csrrw(0, MCYCLE, 0), // mcycle = 0
        NOP,
        csrrs(16, MCYCLE, 0), // x16 pequeno, < x11
        addi(9, 0, 1), // marcador de fim
        jal0
      )

  private case class Run(
      regs: Map[Int, BigInt],
      iMisses: BigInt,
      dMisses: BigInt,
      dHits: BigInt
  )

  // Try memoriza a falha: se a simulacao quebrar, ela nao roda de novo.
  private lazy val run: scala.util.Try[Run] = scala.util.Try {
    val first = mutable.Map.empty[Int, BigInt] // 1a escrita de cada rd
    var result: Run = null
    test(
      new Pipeline3(
        initialProgram = program,
        memoryWords = 256,
        memoryLatency = 2
      )
    ) { dut =>
      dut.io.perfClear.poke(false.B)
      var cycle = 0
      while (cycle < 3000 && !first.contains(9)) {
        val rd = dut.io.writebackRd.peekInt().toInt
        if (
          dut.io.writebackEnable.peekBoolean() && rd != 0 && !first.contains(rd)
        )
          first(rd) = dut.io.writebackData.peekInt()
        dut.clock.step(1)
        cycle += 1
      }
      dut.clock.step(2)
      result = Run(
        first.toMap,
        dut.io.iPerf.misses.peekInt(),
        dut.io.dPerf.misses.peekInt(),
        dut.io.dPerf.hits.peekInt()
      )
    }
    require(result.regs.contains(9), "o programa nao chegou ao marcador de fim")
    result
  }

  private def reg(n: Int): BigInt =
    run.get.regs.getOrElse(n, fail(s"x$n nunca foi escrito"))

  it should "fazer mcycle crescer a cada ciclo (e valer menos apos escrever 0)" in {
    assert(reg(11) > reg(10) + 20, "21 instrucoes entre as duas leituras")
    assert(reg(16) < reg(11), "escrever 0 em mcycle reinicia a contagem")
    assert(reg(16) < 40)
  }

  it should "contar em minstret cada instrucao concluida exatamente uma vez" in {
    // Entre as duas leituras concluem o proprio CSRRS (i1) + 10 ADDI = 11,
    // mesmo com varios ciclos de espera por miss de ICache (latencia 2).
    assert(reg(13) - reg(12) == 11, s"minstret: ${reg(12)} -> ${reg(13)}")
    assert(reg(15) == 0, "metade alta de minstret")
  }

  it should "espelhar minstret em instret (somente leitura)" in {
    assert(reg(14) == reg(13) + 1)
  }

  it should "escrever e ler mcounteren" in {
    assert(reg(21) == 7)
  }

  it should "expor hits e misses das caches nos CSRs hpm, iguais as saidas do Pipeline3" in {
    assert(reg(19) == 1, "DCache: 1 miss (primeiro lw)")
    assert(reg(20) == 1, "DCache: 1 hit (segundo lw, mesma linha)")
    assert(reg(22) == 1, "hpmcounter7 espelha mhpmcounter7")
    assert(reg(23) >= 1, "ICache: ao menos um miss")
    assert(run.get.dMisses == 1 && run.get.dHits == 1)
    assert(run.get.iMisses >= 1)
  }
}
