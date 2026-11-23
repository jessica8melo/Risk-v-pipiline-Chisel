package riscv.integration.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import riscv.pipeline.Pipeline3

/** DIAGNOSTICO (temporario): onde o minstret deixa de contar uma instrucao?
  *
  * Le o minstret depois de CADA addi, com o mesmo prefixo (mesmo alinhamento)
  * do Pipeline3CounterSpec. Cada leitura conta as instrucoes mais antigas que
  * ela, entao a diferenca entre leituras consecutivas deve ser exatamente 2 (o
  * csrrs anterior + o addi). O teste imprime cada passo com o endereco do csrrs
  * e o deslocamento dentro da linha de 16 B da ICache.
  */
class Pipeline3MinstretDiagSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "minstret (diagnostico)"

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private val jal0 = 0x0000006fL

  private val MCYCLE = 0xb00
  private val MINSTRET = 0xb02
  private val Reads = 12

  private val prefix: Seq[Long] =
    Seq(csrrs(10, MCYCLE, 0)) ++ Seq.fill(20)(addi(5, 5, 1)) ++
      Seq(csrrs(11, MCYCLE, 0))

  private val program: Seq[Long] =
    prefix ++
      (0 until Reads).flatMap(k =>
        Seq(addi(6, 6, 1), csrrs(12 + k, MINSTRET, 0))
      ) ++
      Seq(addi(9, 0, 1), jal0)

  for (latency <- Seq(0, 2, 4)) {
    it should s"contar +2 entre leituras consecutivas (latencia $latency)" in {
      val first = mutable.Map.empty[Int, BigInt]
      test(
        new Pipeline3(
          initialProgram = program,
          memoryWords = 256,
          memoryLatency = latency
        )
      ) { dut =>
        dut.io.perfClear.poke(false.B)
        var cycle = 0
        while (cycle < 3000 && !first.contains(9)) {
          val rd = dut.io.writebackRd.peekInt().toInt
          if (
            dut.io.writebackEnable.peekBoolean() && rd != 0 &&
            !first.contains(rd)
          ) first(rd) = dut.io.writebackData.peekInt()
          dut.clock.step(1)
          cycle += 1
        }
        dut.clock.step(2)
      }
      assert(first.contains(9), "o programa nao chegou ao marcador de fim")

      val base = prefix.length // indice (em palavras) do primeiro addi
      val lines = (0 until Reads).map { k =>
        val addr = 4 * (base + 2 * k + 1) // endereco do csrrs k
        val value = first.get(12 + k)
        val delta =
          if (k == 0) None
          else for (a <- first.get(11 + k); b <- value) yield b - a
        f"k=$k%2d csrrs@0x$addr%03x (+${addr % 16}%2d na linha)  minstret=${value
            .map(_.toString)
            .getOrElse("?")}%5s  delta=${delta.map(_.toString).getOrElse("-")}"
      }
      println(s"--- minstret por passo (latencia $latency) ---")
      lines.foreach(println)

      val bad = (1 until Reads).filter { k =>
        (for (a <- first.get(11 + k); b <- first.get(12 + k))
          yield b - a) != Some(BigInt(2))
      }
      assert(bad.isEmpty, s"delta != 2 nos passos k = ${bad.mkString(", ")}")
    }
  }
}
