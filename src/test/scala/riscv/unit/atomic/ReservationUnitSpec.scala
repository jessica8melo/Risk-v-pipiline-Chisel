package riscv.unit.atomic

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.atomic.ReservationUnit

class ReservationUnitSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ReservationUnit"

  private val addrA = 0x100
  private val addrB = 0x104

  private def idle(dut: ReservationUnit): Unit = {
    dut.io.lrCommit.poke(false.B)
    dut.io.scCommit.poke(false.B)
    dut.io.amoCommit.poke(false.B)
    dut.io.storeHit.poke(false.B)
    dut.io.invalidate.poke(false.B)
  }

  private def reserve(dut: ReservationUnit, addr: Int): Unit = {
    idle(dut)
    dut.io.addr.poke(addr.U)
    dut.io.lrCommit.poke(true.B)
    dut.clock.step(1)
    idle(dut)
  }

  private def hasReservation(dut: ReservationUnit, addr: Int): Boolean = {
    dut.io.addr.poke(addr.U)
    dut.io.hasReservation.peekBoolean()
  }

  it should "comecar sem reserva" in {
    test(new ReservationUnit) { dut =>
      idle(dut)
      assert(!hasReservation(dut, 0), "reset deveria deixar a reserva invalida (mesmo para o endereco 0)")
      assert(!hasReservation(dut, addrA))
    }
  }

  it should "reservar apenas o endereco do LR" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)
      assert(hasReservation(dut, addrA))
      assert(!hasReservation(dut, addrB))
      assert(hasReservation(dut, addrA), "consulta nao deve alterar o estado")
    }
  }

  it should "substituir a reserva anterior em um novo LR" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)
      reserve(dut, addrB)
      assert(!hasReservation(dut, addrA))
      assert(hasReservation(dut, addrB))
    }
  }

  it should "consumir a reserva no SC" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)
      dut.io.addr.poke(addrA.U)
      dut.io.scCommit.poke(true.B)
      dut.io.hasReservation.expect(true.B) // o SC ainda enxerga a reserva neste ciclo
      dut.clock.step(1)
      idle(dut)
      assert(!hasReservation(dut, addrA))
    }
  }

  it should "invalidar a reserva em um AMO" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)
      dut.io.addr.poke(addrA.U)
      dut.io.amoCommit.poke(true.B)
      dut.clock.step(1)
      idle(dut)
      assert(!hasReservation(dut, addrA))
    }
  }

  it should "invalidar a reserva apenas quando a escrita atinge o endereco reservado" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)

      dut.io.addr.poke(addrB.U) // escrita em outro endereco
      dut.io.storeHit.poke(true.B)
      dut.clock.step(1)
      idle(dut)
      assert(hasReservation(dut, addrA), "escrita em outro endereco nao deve invalidar")

      dut.io.addr.poke(addrA.U) // escrita no endereco reservado
      dut.io.storeHit.poke(true.B)
      dut.clock.step(1)
      idle(dut)
      assert(!hasReservation(dut, addrA))
    }
  }

  it should "invalidar a reserva por instrucao ilegal ou trap" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)
      dut.io.invalidate.poke(true.B)
      dut.clock.step(1)
      idle(dut)
      assert(!hasReservation(dut, addrA))
    }
  }

  it should "dar prioridade a invalidacao sobre um LR no mesmo ciclo" in {
    test(new ReservationUnit) { dut =>
      idle(dut)
      dut.io.addr.poke(addrA.U)
      dut.io.lrCommit.poke(true.B)
      dut.io.invalidate.poke(true.B)
      dut.clock.step(1)
      idle(dut)
      assert(!hasReservation(dut, addrA))
    }
  }

  it should "ignorar eventos que nao sao commit (entradas em zero)" in {
    test(new ReservationUnit) { dut =>
      reserve(dut, addrA)
      idle(dut)
      dut.clock.step(5)
      assert(hasReservation(dut, addrA))
    }
  }
}
