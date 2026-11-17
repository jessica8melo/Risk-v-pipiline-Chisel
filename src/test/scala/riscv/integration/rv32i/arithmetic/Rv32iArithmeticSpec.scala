package riscv.integration.rv32i.arithmetic

import riscv.integration.support.{Rv32iBench, Rv32iCode}

class Rv32iArithmeticSpec extends Rv32iBench {
  import Rv32iCode._
  behavior of "RV32I: aritmética e lógica"

  // Cada caso vira um teste independente com nome próprio no relatório do ScalaTest.
  private val registerCases = Seq(
    ("ADD", r(0), -5L),
    ("SUB", r(0, 0x20), -11L),
    ("SLL", r(1), -64L),
    ("SLT", r(2), 1L),
    ("SLTU", r(3), 0L),
    ("XOR", r(4), -5L),
    ("SRL", r(5), 0x1fffffffL),
    ("SRA", r(5, 0x20), -1L),
    ("OR", r(6), -5L),
    ("AND", r(7), 0L)
  )
  registerCases.foreach { case (name, instruction, result) =>
    it should s"executar $name" in check(
      registers(instruction),
      Map(5 -> BigInt(result))
    )
  }

  private val immediateCases = Seq(
    ("ADDI", 0, 5, -3L),
    ("SLTI", 2, -1, 1L),
    ("SLTIU", 3, 1, 0L),
    ("XORI", 4, 3, -5L),
    ("ORI", 6, 3, -5L),
    ("ANDI", 7, 3, 0L),
    ("SLLI", 1, 3, -64L),
    ("SRLI", 5, 3, 0x1fffffffL),
    ("SRAI", 5, 0x403, -1L)
  )
  immediateCases.foreach { case (name, funct3, imm, result) =>
    it should s"executar $name" in check(
      Seq(addi(1, 0, -8), i(0x13, funct3, 5, 1, imm), nop),
      Map(5 -> BigInt(result))
    )
  }

  it should "executar LUI" in check(
    Seq(u(0x37, 5, 0x12345000), nop),
    Map(5 -> BigInt(0x12345000L))
  )
  it should "executar AUIPC somando o PC" in
    check(
      Seq(addi(1, 0, 1), u(0x17, 5, 0x12345000), nop),
      Map(5 -> BigInt(0x12345004L))
    )
}
