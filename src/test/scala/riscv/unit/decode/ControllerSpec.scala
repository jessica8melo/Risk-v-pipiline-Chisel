package riscv.unit.decode

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.elementosbasicos.{ALUOp, Controller, RV32I}

class ControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Controller"

  private def pokeBase(
      dut: Controller,
      opcode: UInt,
      funct3: UInt = 0.U,
      funct7: UInt = 0.U,
      rs2: UInt = 0.U
  ): Unit = {
    dut.io.opcode.poke(opcode)
    dut.io.funct3.poke(funct3)
    dut.io.funct7.poke(funct7)
    dut.io.rs2.poke(rs2)
  }

  // funct7 de uma instrucao atomica = funct5 | aq | rl
  private def pokeAtomic(
      dut: Controller,
      funct5: Int,
      rs2: Int = 0,
      aq: Boolean = false,
      rl: Boolean = false,
      funct3: Int = 2
  ): Unit = {
    val funct7 = (funct5 << 2) | (if (aq) 2 else 0) | (if (rl) 1 else 0)
    pokeBase(dut, RV32I.Opcode.ATOMIC, funct3.U, funct7.U, rs2.U)
  }

  it should "decodificar instrucao ADD corretamente (Tipo R)" in {
    test(new Controller) { dut =>
      pokeBase(dut, RV32I.Opcode.OP, "b000".U, "b0000000".U)

      dut.io.signals.regWrite.expect(true.B)
      dut.io.signals.operandBSel.expect(RV32I.OperandBSel.RS2)
      dut.io.signals.aluOp.expect(ALUOp.ADD)
      dut.io.signals.memWrite.expect(false.B)
      dut.io.signals.branchType.expect(RV32I.BranchType.NONE)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "decodificar instrucao SUB corretamente (Tipo R)" in {
    test(new Controller) { dut =>
      pokeBase(dut, RV32I.Opcode.OP, "b000".U, "b0100000".U)

      dut.io.signals.aluOp.expect(ALUOp.SUB)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "decodificar loads RV32I com tamanho e extensao corretos" in {
    test(new Controller) { dut =>
      pokeBase(dut, RV32I.Opcode.LOAD, "b010".U)

      dut.io.signals.regWrite.expect(true.B)
      dut.io.signals.operandBSel.expect(RV32I.OperandBSel.IMM)
      dut.io.signals.writebackSel.expect(RV32I.WritebackSel.MEM)
      dut.io.signals.memSize.expect(RV32I.MemorySize.WORD)
      dut.io.signals.memUnsigned.expect(false.B)
      dut.io.signals.aluOp.expect(ALUOp.ADD)

      pokeBase(dut, RV32I.Opcode.LOAD, "b100".U)
      dut.io.signals.memSize.expect(RV32I.MemorySize.BYTE)
      dut.io.signals.memUnsigned.expect(true.B)
    }
  }

  it should "decodificar branches RV32I explicitamente" in {
    test(new Controller) { dut =>
      pokeBase(dut, RV32I.Opcode.BRANCH, "b110".U)

      dut.io.signals.branchType.expect(RV32I.BranchType.BLTU)
      dut.io.signals.aluOp.expect(ALUOp.SLTU)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "marcar combinacoes invalidas como ilegais" in {
    test(new Controller) { dut =>
      pokeBase(dut, RV32I.Opcode.OP, "b001".U, "b0100000".U)

      dut.io.signals.illegal.expect(true.B)
      dut.io.signals.regWrite.expect(false.B)
    }
  }

  it should "decodificar LR.W com rs2 = 0" in {
    test(new Controller) { dut =>
      pokeAtomic(dut, funct5 = 0x02)

      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.isLR.expect(true.B)
      dut.io.signals.isSC.expect(false.B)
      dut.io.signals.isAMO.expect(false.B)
      dut.io.signals.regWrite.expect(true.B)
      dut.io.signals.memWrite.expect(false.B)
      dut.io.signals.writebackSel.expect(RV32I.WritebackSel.MEM)
    }
  }

  it should "marcar LR.W com rs2 diferente de zero como ilegal" in {
    test(new Controller) { dut =>
      for (rs2 <- Seq(1, 5, 31)) {
        pokeAtomic(dut, funct5 = 0x02, rs2 = rs2)
        dut.io.signals.illegal.expect(true.B)
      }
    }
  }

  it should "nao exigir rs2 = 0 em SC.W e AMOs" in {
    test(new Controller) { dut =>
      pokeAtomic(dut, funct5 = 0x03, rs2 = 7)
      dut.io.signals.illegal.expect(false.B)
      dut.io.signals.isSC.expect(true.B)
      dut.io.signals.isLR.expect(false.B)
      dut.io.signals.memWrite.expect(true.B)
      dut.io.signals.regWrite.expect(true.B)

      pokeAtomic(dut, funct5 = 0x00, rs2 = 7)
      dut.io.signals.illegal.expect(false.B)
    }
  }

  it should "decodificar todas as operacoes AMO.W" in {
    val amos = Seq(
      (0x01, "SWAP", RV32I.AMOOp.SWAP),
      (0x00, "ADD", RV32I.AMOOp.ADD),
      (0x0c, "AND", RV32I.AMOOp.AND),
      (0x08, "OR", RV32I.AMOOp.OR),
      (0x04, "XOR", RV32I.AMOOp.XOR),
      (0x14, "MAX", RV32I.AMOOp.MAX),
      (0x10, "MIN", RV32I.AMOOp.MIN),
      (0x16, "MAXU", RV32I.AMOOp.MAXU),
      (0x12, "MINU", RV32I.AMOOp.MINU)
    )

    test(new Controller) { dut =>
      for ((funct5, name, amoOp) <- amos) {
        pokeAtomic(dut, funct5 = funct5, rs2 = 3)
        withClue(s"AMO$name: ") {
          dut.io.signals.illegal.expect(false.B)
          dut.io.signals.isAMO.expect(true.B)
          dut.io.signals.isLR.expect(false.B)
          dut.io.signals.isSC.expect(false.B)
          dut.io.signals.amoOp.expect(amoOp)
          dut.io.signals.regWrite.expect(true.B)
          dut.io.signals.memWrite.expect(true.B)
          dut.io.signals.writebackSel.expect(RV32I.WritebackSel.MEM)
        }
      }
    }
  }

  it should "propagar os bits aq e rl das instrucoes atomicas" in {
    test(new Controller) { dut =>
      pokeAtomic(dut, funct5 = 0x00, aq = true, rl = false)
      dut.io.signals.amoAq.expect(true.B)
      dut.io.signals.amoRl.expect(false.B)

      pokeAtomic(dut, funct5 = 0x00, aq = false, rl = true)
      dut.io.signals.amoAq.expect(false.B)
      dut.io.signals.amoRl.expect(true.B)
    }
  }

  it should "marcar funct5 reservado e funct3 diferente de 010 em atomicas como ilegais" in {
    test(new Controller) { dut =>
      for (funct5 <- Seq(0x05, 0x06, 0x07, 0x09, 0x0d, 0x18, 0x1f)) {
        pokeAtomic(dut, funct5 = funct5)
        withClue(s"funct5=$funct5: ") { dut.io.signals.illegal.expect(true.B) }
      }
      for (funct3 <- Seq(0, 1, 3, 7)) {
        pokeAtomic(dut, funct5 = 0x00, funct3 = funct3)
        withClue(s"funct3=$funct3: ") { dut.io.signals.illegal.expect(true.B) }
      }
    }
  }
}
