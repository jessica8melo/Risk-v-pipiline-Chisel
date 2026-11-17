package riscv.integration.rv32i.control

import riscv.integration.support.{Rv32iBench, Rv32iCode}

class Rv32iControlSpec extends Rv32iBench {
  import Rv32iCode._
  behavior of "RV32I: desvios e fluxo"

  // Valores opostos exercitam comparações com e sem sinal; BEQ/BNE usam igualdade.
  private val branches = Seq(
    ("BEQ", 0, 5, 5, 1, 2),
    ("BNE", 1, -1, 1, 1, 1),
    ("BLT", 4, -1, 1, 1, 1),
    ("BGE", 5, 1, -1, -1, 1),
    ("BLTU", 6, 1, -1, 1, 1),
    ("BGEU", 7, -1, 1, 1, -1)
  )
  branches.foreach { case (name, funct3, a, bValue, falseA, falseB) =>
    val prefix = Seq(addi(1, 0, a), addi(2, 0, bValue))
    val taken =
      prefix ++ Seq(b(funct3, 1, 2), addi(10, 0, 99), addi(11, 0, 42), nop)
    it should s"executar $name tomado" in check(
      taken,
      Map(11 -> BigInt(42)),
      absent = Set(10)
    )

    val notTaken = Seq(
      addi(1, 0, falseA),
      addi(2, 0, falseB),
      b(funct3, 1, 2),
      addi(10, 0, 99),
      addi(11, 0, 42),
      nop
    )
    it should s"executar $name não tomado" in
      check(notTaken, Map(10 -> BigInt(99), 11 -> BigInt(42)))
  }

  it should "executar JAL e gravar PC+4" in
    check(
      Seq(j(5, 8), addi(10, 0, 99), addi(11, 0, 42), nop),
      Map(5 -> BigInt(4), 11 -> BigInt(42)),
      absent = Set(10)
    )

  it should "executar JALR, limpar bit zero do destino e gravar PC+4" in
    check(
      Seq(
        addi(1, 0, 17),
        i(0x67, 0, 5, 1, 0),
        addi(10, 0, 99),
        nop,
        addi(11, 0, 42),
        nop
      ),
      Map(5 -> BigInt(8), 11 -> BigInt(42)),
      absent = Set(10)
    )

  it should "executar FENCE como barreira sem alterar registradores" in
    check(
      Seq(addi(1, 0, 1), 0x0000000fL, addi(5, 1, 2), nop),
      Map(5 -> BigInt(3))
    )
}
