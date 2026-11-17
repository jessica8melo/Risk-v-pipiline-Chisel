package riscv.unit.decode

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.ImmGen

class ImmGenTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "ImmGen"

  private def u32(x: BigInt): BigInt = x & BigInt("FFFFFFFF", 16)

  private def signExtend(value: BigInt, bits: Int): BigInt = {
    val mask = (BigInt(1) << bits) - 1
    val v = value & mask
    val signBit = BigInt(1) << (bits - 1)
    if ((v & signBit) != 0) v - (BigInt(1) << bits) else v
  }

  private def mkI(imm12: Int, opcode: Int): BigInt = {
    (BigInt(imm12 & 0xfff) << 20) | BigInt(opcode & 0x7f)
  }

  private def mkS(imm12: Int, opcode: Int): BigInt = {
    val imm = imm12 & 0xfff
    (BigInt((imm >> 5) & 0x7f) << 25) |
      (BigInt(imm & 0x1f) << 7) |
      BigInt(opcode & 0x7f)
  }

  private def mkB(imm13: Int, opcode: Int): BigInt = {
    val imm = imm13 & 0x1fff
    (BigInt((imm >> 12) & 0x1) << 31) |
      (BigInt((imm >> 11) & 0x1) << 7) |
      (BigInt((imm >> 5) & 0x3f) << 25) |
      (BigInt((imm >> 1) & 0xf) << 8) |
      BigInt(opcode & 0x7f)
  }

  private def mkU(imm20: Int, opcode: Int): BigInt = {
    (BigInt(imm20 & 0xfffff) << 12) | BigInt(opcode & 0x7f)
  }

  private def mkJ(imm21: Int, opcode: Int): BigInt = {
    val imm = imm21 & 0x1fffff
    (BigInt((imm >> 20) & 0x1) << 31) |
      (BigInt((imm >> 12) & 0xff) << 12) |
      (BigInt((imm >> 11) & 0x1) << 20) |
      (BigInt((imm >> 1) & 0x3ff) << 21) |
      BigInt(opcode & 0x7f)
  }

  private def runCase(dut: ImmGen, instr: BigInt, expected: BigInt): Unit = {
    dut.io.instr.poke(u32(instr).U(32.W))
    dut.clock.step(1)
    dut.io.imm.expect(u32(expected).U(32.W))
  }

  it should "gerar imediato correto para cada formato RV32I" in {
    test(new ImmGen) { dut =>
      // I-type: addi-like, imm = -1
      val iImm = 0xfff
      runCase(dut, mkI(iImm, 0x13), signExtend(iImm, 12))

      // S-type: store-like, imm = -16
      val sImm = 0xff0
      runCase(dut, mkS(sImm, 0x23), signExtend(sImm, 12))

      // B-type: branch-like, imm = +16
      val bImm = 0x010
      runCase(dut, mkB(bImm, 0x63), signExtend(bImm, 13))

      // U-type: lui-like
      val uImm = 0xabcde
      runCase(dut, mkU(uImm, 0x37), BigInt(uImm) << 12)

      // J-type: jal-like, imm = -4
      val jImm = 0x1ffffc
      runCase(dut, mkJ(jImm, 0x6f), signExtend(jImm, 21))
    }
  }

  it should "retornar zero para opcode sem imediato (tipo R)" in {
    test(new ImmGen) { dut =>
      // Opcode 0110011: tipo R (add/sub/and/or...), sem imediato.
      runCase(dut, BigInt(0x00000033L), 0)
    }
  }
}
