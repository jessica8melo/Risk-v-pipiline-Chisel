package riscv.unit.registers

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.registers.RegisterFile

class RegisterFileTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "RegisterFile"

  it should "escrever e ler corretamente em registrador valido" in {
    test(new RegisterFile) { dut =>
      // Escrita em x5.
      dut.io.regWrite.poke(true.B)
      dut.io.rd.poke(5.U)
      dut.io.writeData.poke(42.U)
      dut.clock.step(1)

      // Leitura do mesmo registrador nas duas portas.
      dut.io.rs1.poke(5.U)
      dut.io.rs2.poke(5.U)
      dut.clock.step(1)

      dut.io.readData1.expect(42.U)
      dut.io.readData2.expect(42.U)
    }
  }

  it should "manter x0 imutavel mesmo com tentativa de escrita" in {
    test(new RegisterFile) { dut =>
      // Tenta escrever 99 em x0 (deve ser ignorado).
      dut.io.regWrite.poke(true.B)
      dut.io.rd.poke(0.U)
      dut.io.writeData.poke(99.U)
      dut.clock.step(1)

      // Leitura de x0 deve continuar zero.
      dut.io.rs1.poke(0.U)
      dut.io.rs2.poke(0.U)
      dut.clock.step(1)

      dut.io.readData1.expect(0.U)
      dut.io.readData2.expect(0.U)
    }
  }

  it should "nao escrever quando regWrite estiver desativado" in {
    test(new RegisterFile) { dut =>
      // Tenta escrever em x10 com regWrite desabilitado.
      dut.io.regWrite.poke(false.B)
      dut.io.rd.poke(10.U)
      dut.io.writeData.poke(123.U)
      dut.clock.step(1)

      // Registrador deve permanecer no valor inicial (zero).
      dut.io.rs1.poke(10.U)
      dut.io.rs2.poke(10.U)
      dut.clock.step(1)

      dut.io.readData1.expect(0.U)
      dut.io.readData2.expect(0.U)
    }
  }
}
