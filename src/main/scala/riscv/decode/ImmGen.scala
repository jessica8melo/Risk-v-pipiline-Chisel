package riscv.decode

import chisel3._
import chisel3.util._

/** Gerador de imediatos para instrucoes RV32I.
  *
  * Recebe a instrucao de 32 bits e devolve o imediato de 32 bits no formato
  * correto (com extensao de sinal quando aplicavel).
  */
class ImmGen extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val imm = Output(UInt(32.W))
  })

  import RV32I.Opcode

  // Opcode: 7 bits menos significativos da instrucao.
  val opcode = io.instr(6, 0)

  // Extracao dos imediatos brutos.
  val immI = io.instr(31, 20)
  val immS = Cat(io.instr(31, 25), io.instr(11, 7))
  val immB =
    Cat(io.instr(31), io.instr(7), io.instr(30, 25), io.instr(11, 8), 0.U(1.W))
  val immU = Cat(io.instr(31, 12), 0.U(12.W))
  val immJ = Cat(
    io.instr(31),
    io.instr(19, 12),
    io.instr(20),
    io.instr(30, 21),
    0.U(1.W)
  )

  // Extensao de sinal para 32 bits.
  val signExtI = Cat(Fill(20, immI(11)), immI)
  val signExtS = Cat(Fill(20, immS(11)), immS)
  val signExtB = Cat(Fill(19, immB(12)), immB)
  val signExtJ = Cat(Fill(11, immJ(20)), immJ)

  io.imm := 0.U(32.W)

  switch(opcode) {
    // Tipo I: loads, arith-immediate, jalr
    is(Opcode.LOAD, Opcode.OP_IMM, Opcode.JALR) {
      io.imm := signExtI
    }

    // Tipo S: stores
    is(Opcode.STORE) {
      io.imm := signExtS
    }

    // Tipo B: branches
    is(Opcode.BRANCH) {
      io.imm := signExtB
    }

    // Tipo U: lui, auipc
    is(Opcode.LUI, Opcode.AUIPC) {
      io.imm := immU
    }

    // Tipo J: jal
    is(Opcode.JAL) {
      io.imm := signExtJ
    }
  }
}
