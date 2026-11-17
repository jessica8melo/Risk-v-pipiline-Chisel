package riscv.atomic

import chisel3._
import chisel3.util._
import riscv.decode.RV32I.AMOOp

/** ALU combinacional das operacoes AMO.W da extensao A.
  *
  * Recebe o valor antigo lido da memoria (`memData`) e o operando (`rs2`) e
  * devolve o valor que deve ser escrito de volta (`result`). O valor antigo
  * continua sendo o que vai para `rd`; isso e responsabilidade de quem
  * instancia o modulo.
  *
  *   - SWAP: operando
  *   - ADD/AND/OR/XOR: operacao bit a bit / aritmetica modulo 2^32
  *   - MAX/MIN: comparacao COM sinal
  *   - MAXU/MINU: comparacao SEM sinal
  *
  * Codigos de `op` fora da tabela (9..15) devolvem o operando, igual ao SWAP.
  */
class AmoAlu extends Module {
  val io = IO(new Bundle {
    val op = Input(UInt(4.W)) // AMOOp.*
    val memData = Input(UInt(32.W)) // valor atual na memoria
    val operand = Input(UInt(32.W)) // rs2
    val result = Output(UInt(32.W)) // valor a gravar na memoria
  })

  io.result := io.operand

  switch(io.op) {
    is(AMOOp.SWAP) { io.result := io.operand }
    is(AMOOp.ADD) { io.result := io.memData + io.operand }
    is(AMOOp.AND) { io.result := io.memData & io.operand }
    is(AMOOp.OR) { io.result := io.memData | io.operand }
    is(AMOOp.XOR) { io.result := io.memData ^ io.operand }
    is(AMOOp.MAX) {
      io.result := Mux(
        io.memData.asSInt > io.operand.asSInt,
        io.memData,
        io.operand
      )
    }
    is(AMOOp.MIN) {
      io.result := Mux(
        io.memData.asSInt < io.operand.asSInt,
        io.memData,
        io.operand
      )
    }
    is(AMOOp.MAXU) {
      io.result := Mux(io.memData > io.operand, io.memData, io.operand)
    }
    is(AMOOp.MINU) {
      io.result := Mux(io.memData < io.operand, io.memData, io.operand)
    }
  }
}
