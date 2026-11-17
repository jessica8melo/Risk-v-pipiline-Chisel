package riscv.integration.rv32c

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import riscv.integration.support.Rv32iCode
import riscv.pipeline.Pipeline3

class CompressedPipelineSpec extends AnyFlatSpec with ChiselScalatestTester {
  import Rv32iCode._

  private def pack(instructions: (Long, Int)*): Seq[Long] = {
    val bytes = instructions.flatMap { case (instr, size) =>
      (0 until size).map(i => (instr >> (8 * i)) & 0xffL)
    }
    bytes
      .grouped(4)
      .map(
        _.zipWithIndex
          .map { case (byte, i) =>
            byte << (8 * i)
          }
          .foldLeft(0L)(_ | _)
      )
      .toSeq
  }

  behavior of "Pipeline3 com RVC"

  it should "executar instrucoes misturadas sem repetir a instrucao anterior nas bolhas" in {
    val program = pack(
      (0x4095L, 2), // c.li x1, 5
      (addi(2, 1, 3), 4),
      (r(0, rd = 3, rs1 = 1, rs2 = 2), 4),
      (0x0185L, 2), // c.addi x3, 1
      (j(0, 0), 4)
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      val writes = ArrayBuffer.empty[(Int, BigInt)]
      for (_ <- 0 until 80) {
        if (
          dut.io.writebackEnable
            .peekBoolean() && dut.io.writebackRd.peekInt() != 0
        )
          writes += ((
            dut.io.writebackRd.peekInt().toInt,
            dut.io.writebackData.peekInt()
          ))
        dut.clock.step()
      }
      assert(
        writes.toSeq == Seq(
          1 -> BigInt(5),
          2 -> BigInt(8),
          3 -> BigInt(13),
          3 -> BigInt(14)
        )
      )
    }
  }

  it should "gravar PC+2 em C.JAL e C.JALR e descartar o caminho errado" in {
    val program = pack(
      (0x2019L, 2), // PC 0: c.jal +6, ra = 2
      (addi(10, 0, 99), 4), // PC 2: descartada
      (0x42a5L, 2), // PC 6: c.li x5, 9
      (0x4139L, 2), // PC 8: c.li x2, 14
      (0x9102L, 2), // PC 10: c.jalr x2, ra = 12
      (0x459dL, 2), // PC 12: c.li x11, 7, descartada
      (0x4329L, 2), // PC 14: c.li x6, 10
      (j(0, 0), 4)
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      val writes = ArrayBuffer.empty[(Int, BigInt)]
      for (_ <- 0 until 80) {
        if (
          dut.io.writebackEnable
            .peekBoolean() && dut.io.writebackRd.peekInt() != 0
        )
          writes += ((
            dut.io.writebackRd.peekInt().toInt,
            dut.io.writebackData.peekInt()
          ))
        dut.clock.step()
      }
      assert(
        writes.toSeq == Seq(
          1 -> BigInt(2),
          5 -> BigInt(9),
          2 -> BigInt(14),
          1 -> BigInt(12),
          6 -> BigInt(10)
        )
      )
    }
  }

  it should "manter load-use e forwarding quando a instrucao dependente e comprimida" in {
    val program = pack(
      (addi(1, 0, 64), 4),
      (addi(2, 0, 42), 4),
      (s(2, 1, 2), 4),
      (load(2, 3, 1), 4),
      (0x0185L, 2), // c.addi x3, 1
      (addi(4, 3, 2), 4),
      (j(0, 0), 4)
    )
    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      val results = ArrayBuffer.empty[BigInt]
      var stalls = 0
      for (_ <- 0 until 80) {
        if (dut.io.stalled.peekBoolean()) stalls += 1
        if (
          dut.io.writebackEnable
            .peekBoolean() && dut.io.writebackRd.peekInt() == 4
        )
          results += dut.io.writebackData.peekInt()
        dut.clock.step()
      }
      assert(stalls == 1)
      assert(results.toSeq == Seq(BigInt(45)))
    }
  }
}
