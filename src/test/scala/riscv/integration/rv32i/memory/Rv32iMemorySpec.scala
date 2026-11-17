package riscv.integration.rv32i.memory

import riscv.integration.support.{Rv32iBench, Rv32iCode}

class Rv32iMemorySpec extends Rv32iBench {
  import Rv32iCode._
  behavior of "RV32I: loads e stores"

  private val loads = Seq(
    ("LB", 0, 0, -128L),
    ("LH", 1, 1, -128L),
    ("LW", 2, 2, -128L),
    ("LBU", 4, 0, 128L),
    ("LHU", 5, 1, 0xff80L)
  )
  loads.foreach { case (name, loadSize, storeSize, result) =>
    it should s"executar $name com extensão correta" in {
      val program = Seq(
        addi(1, 0, 64),
        addi(2, 0, -128),
        s(storeSize, 1, 2),
        load(loadSize, 5, 1),
        nop
      )
      check(program, Map(5 -> BigInt(result)))
    }
  }

  private val stores =
    Seq(("SB", 0, 4, 128L, 1), ("SH", 1, 5, 0xff80L, 2), ("SW", 2, 2, -128L, 4))
  stores.foreach { case (name, storeSize, loadSize, result, nextByte) =>
    it should s"executar $name e permitir leitura posterior" in {
      val program = Seq(
        addi(1, 0, 64),
        addi(2, 0, -128),
        s(storeSize, 1, 2),
        load(loadSize, 5, 1),
        load(4, 6, 1, nextByte),
        nop
      )
      check(program, Map(5 -> BigInt(result), 6 -> BigInt(0)))
    }
  }

  it should "preservar o resultado em dependência imediata de LW" in {
    val program = Seq(
      addi(1, 0, 64),
      addi(2, 0, 42),
      s(2, 1, 2),
      load(2, 3, 1),
      r(0, rd = 5, rs1 = 3, rs2 = 2),
      nop
    )
    check(program, Map(5 -> BigInt(84)))
  }

  it should "não fazer stall quando o imediato coincide com o rd do LW" in {
    val program = Seq(addi(1, 0, 64), load(2, 3, 1), addi(5, 0, 3), nop)
    check(program, Map(5 -> BigInt(3)))
  }
}
