package riscv.integration.support

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.pipeline.Pipeline3
import scala.collection.mutable.ArrayBuffer

/** Binding point for the core under test; keep this module's public interface
  * stable.
  */
class Rv32iBlackBox(program: Seq[Long], words: Int = 64)
    extends Pipeline3(initialProgram = program, memoryWords = words)

/** Codifica instruções RV32I para montar programas pequenos nos testes. */
object Rv32iCode {
  val nop: Long = 0x00000013L

  def r(f3: Int, f7: Int = 0, rd: Int = 5, rs1: Int = 1, rs2: Int = 2): Long =
    (f7.toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) | (f3.toLong << 12) | (rd.toLong << 7) | 0x33L

  def i(op: Int, f3: Int, rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (f3.toLong << 12) | (rd.toLong << 7) | op.toLong

  def s(f3: Int, rs1: Int, rs2: Int, imm: Int = 0): Long =
    (((imm >> 5) & 0x7f).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (f3.toLong << 12) | ((imm & 31).toLong << 7) | 0x23L

  def b(f3: Int, rs1: Int, rs2: Int, imm: Int = 8): Long =
    (((imm >> 12) & 1).toLong << 31) | (((imm >> 5) & 63).toLong << 25) |
      (rs2.toLong << 20) | (rs1.toLong << 15) | (f3.toLong << 12) |
      (((imm >> 1) & 15).toLong << 8) | (((imm >> 11) & 1).toLong << 7) | 0x63L

  def u(op: Int, rd: Int, imm: Int): Long =
    (imm.toLong & 0xfffff000L) | (rd.toLong << 7) | op.toLong

  def j(rd: Int, imm: Int): Long =
    (((imm >> 20) & 1).toLong << 31) | (((imm >> 1) & 1023).toLong << 21) |
      (((imm >> 11) & 1).toLong << 20) | (((imm >> 12) & 255).toLong << 12) |
      (rd.toLong << 7) | 0x6fL

  def addi(rd: Int, rs1: Int, imm: Int): Long = i(0x13, 0, rd, rs1, imm)
  def load(f3: Int, rd: Int, rs1: Int, imm: Int = 0): Long =
    i(0x03, f3, rd, rs1, imm)
}

/** Integração observável pela saída pública, sem inspeção de estágios ou
  * stalls.
  */
trait Rv32iBench extends AnyFlatSpec with ChiselScalatestTester {
  import Rv32iCode._

  protected def check(
      program: Seq[Long],
      expected: Map[Int, BigInt],
      absent: Set[Int] = Set.empty
  ): Unit = {
    test(new Rv32iBlackBox(program)) { dut =>
      val writes = ArrayBuffer.empty[(Int, BigInt)]
      val maxCycles = math.max(120, program.size * 12 + 60)
      for (_ <- 0 until maxCycles) {
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

      expected.foreach { case (rd, value) =>
        assert(
          writes.collect { case (`rd`, data) => data }.toSeq == Seq(
            value & BigInt(0xffffffffL)
          ),
          s"x$rd esperado 0x${value.toString(16)} uma vez; escritas: $writes"
        )
      }
      absent.foreach(rd =>
        assert(!writes.exists(_._1 == rd), s"x$rd não deveria ser escrito")
      )
    }
  }

  protected def registers(target: Long): Seq[Long] =
    Seq(addi(1, 0, -8), addi(2, 0, 3), target, nop)
}
