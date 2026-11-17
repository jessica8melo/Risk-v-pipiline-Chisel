package riscv.integration.blackbox.rv32i

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.integration.support.Rv32iBlackBox
import scala.collection.mutable.ArrayBuffer

class Rv32iPipelineBlackBoxSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Processador RV32I pela interface pública"

  it should "executar um programa e expor os resultados arquiteturais" in {
    val program = Seq(
      0x00500093L, // addi x1, x0, 5
      0x00308113L, // addi x2, x1, 3
      0x002081b3L, // add x3, x1, x2
      0x00000013L // nop
    )
    val expectedWrites = Seq(
      1 -> BigInt(5),
      2 -> BigInt(8),
      3 -> BigInt(13)
    )

    test(new Rv32iBlackBox(program, words = 32)) { dut =>
      val writes = ArrayBuffer.empty[(Int, BigInt)]
      var cycles = 0

      while (writes.size < expectedWrites.size && cycles < 120) {
        if (
          dut.io.writebackEnable
            .peekBoolean() && dut.io.writebackRd.peekInt() != 0
        )
          writes += ((
            dut.io.writebackRd.peekInt().toInt,
            dut.io.writebackData.peekInt()
          ))
        dut.clock.step()
        cycles += 1
      }

      assert(writes.toSeq == expectedWrites, s"escritas observadas: $writes")
    }
  }
}
