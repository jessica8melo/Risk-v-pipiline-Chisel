package riscv.unit.privileged

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.decode.RV32I
import riscv.privileged.CLINT

class CLINTSyncExceptionSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CLINT (excecao sincrona do pipeline)"

  it should "registrar mepc, mcause e mtval e saltar para mtvec" in {
    test(new CLINT) { dut =>
      dut.io.mtvec.poke(0x8000.U)
      dut.io.mstatus.poke(0x8.U) // MIE = 1
      dut.io.current_priv.poke(RV32I.PrivilegeMode.M)
      dut.io.exc_valid.poke(true.B)
      dut.io.exc_cause.poke(6.U)
      dut.io.exc_pc.poke(0x1234.U)
      dut.io.exc_tval.poke(0xdead0003L.U)
      dut.clock.step(0)
      dut.io.trap_assert.expect(true.B)
      dut.io.trap_address.expect(0x8000.U)
      dut.io.direct_write_enable.expect(true.B)
      dut.io.mcause_write_data.expect(6.U)
      dut.io.mepc_write_data.expect(0x1234.U)
      dut.io.mtval_write_data.expect(0xdead0003L.U)
    }
  }
}
