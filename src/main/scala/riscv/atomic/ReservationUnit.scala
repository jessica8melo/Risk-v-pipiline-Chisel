package riscv.atomic

import chisel3._

/** Reserva de LR/SC (single hart, granularidade de palavra).
  *
  * Guarda um unico par (endereco, valido). Todas as entradas de evento devem
  * valer apenas no ciclo em que a instrucao correspondente efetiva (commit).
  * Ordem de prioridade (a ultima vence, como no `when` encadeado do Chisel):
  *   1. `lrCommit`: grava `addr` e marca a reserva como valida
  *   2. `scCommit`: consome a reserva (com sucesso ou nao)
  *   3. `amoCommit`: um AMO invalida a reserva
  *   4. `storeHit` em `addr` igual ao endereco reservado: qualquer escrita no
  *      endereco reservado invalida a reserva
  *   5. `invalidate`: trap/instrucao ilegal invalida a reserva
  *
  * `hasReservation` e combinacional: vale quando existe reserva valida para o
  * endereco `addr` apresentado AGORA (antes de aplicar os eventos do ciclo).
  */
class ReservationUnit extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W)) // endereco do acesso em EX
    val lrCommit = Input(Bool())
    val scCommit = Input(Bool())
    val amoCommit = Input(Bool())
    val storeHit = Input(Bool()) // escrita concluida em `addr` neste ciclo
    val invalidate = Input(Bool())
    val hasReservation = Output(Bool())
  })

  private val reservationAddr = RegInit(0.U(32.W))
  private val reservationValid = RegInit(false.B)

  io.hasReservation := reservationValid && (reservationAddr === io.addr)

  when(io.lrCommit) {
    reservationAddr := io.addr
    reservationValid := true.B
  }

  when(io.scCommit) {
    reservationValid := false.B
  }

  when(io.amoCommit) {
    reservationValid := false.B
  }

  when(io.storeHit && (io.addr === reservationAddr)) {
    reservationValid := false.B
  }

  when(io.invalidate) {
    reservationValid := false.B
  }
}
