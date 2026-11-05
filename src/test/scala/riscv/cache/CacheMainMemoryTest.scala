package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CacheMainMemoryTest extends AnyFlatSpec with ChiselScalatestTester {

  private def initialize(dut: CacheMainMemory): Unit = {
    dut.io.iMem.request.poke(false.B)
    dut.io.iMem.write.poke(false.B)
    dut.io.iMem.address.poke(0.U)
    dut.io.dMem.request.poke(false.B)
    dut.io.dMem.write.poke(false.B)
    dut.io.dMem.address.poke(0.U)
    for (word <- 0 until CacheParams.wordsPerLine) {
      dut.io.iMem.writeLine(word).poke(0.U)
      dut.io.dMem.writeLine(word).poke(0.U)
    }
  }

  behavior of "CacheMainMemory"

  it should "retornar uma linha inicializada pela porta de instrucoes" in {
    test(
      new CacheMainMemory(
        depthWords = 8,
        initialData = Seq(0x13L, 0x00100093L, 0x00200113L, 0x00300193L)
      )
    ) { dut =>
      initialize(dut)
      dut.io.iMem.request.poke(true.B)
      dut.io.iMem.address.poke(0.U)

      dut.io.iMem.ready.expect(true.B)
      dut.io.dMem.ready.expect(false.B)
      dut.io.iMem.readLine(0).expect("h00000013".U)
      dut.io.iMem.readLine(1).expect("h00100093".U)
      dut.io.iMem.readLine(2).expect("h00200113".U)
      dut.io.iMem.readLine(3).expect("h00300193".U)
    }
  }

  it should "compartilhar escritas da DCache com leituras da ICache" in {
    test(new CacheMainMemory(depthWords = 8)) { dut =>
      initialize(dut)
      dut.io.dMem.request.poke(true.B)
      dut.io.dMem.write.poke(true.B)
      dut.io.dMem.address.poke(16.U)
      dut.io.dMem.writeLine(0).poke("h11223344".U)
      dut.io.dMem.writeLine(1).poke("h55667788".U)
      dut.io.dMem.writeLine(2).poke("h99AABBCC".U)
      dut.io.dMem.writeLine(3).poke("hDDEEFF00".U)

      dut.io.dMem.ready.expect(true.B)
      dut.clock.step(1)

      dut.io.dMem.request.poke(false.B)
      dut.io.dMem.write.poke(false.B)
      dut.io.iMem.request.poke(true.B)
      dut.io.iMem.address.poke(16.U)
      dut.io.iMem.ready.expect(true.B)
      dut.io.iMem.readLine(0).expect("h11223344".U)
      dut.io.iMem.readLine(1).expect("h55667788".U)
      dut.io.iMem.readLine(2).expect("h99AABBCC".U)
      dut.io.iMem.readLine(3).expect("hDDEEFF00".U)
    }
  }

  it should "dar prioridade a DCache quando as duas portas pedem juntas" in {
    test(new CacheMainMemory(depthWords = 8)) { dut =>
      initialize(dut)
      dut.io.iMem.request.poke(true.B)
      dut.io.dMem.request.poke(true.B)

      dut.io.dMem.ready.expect(true.B)
      dut.io.iMem.ready.expect(false.B)

      dut.io.dMem.request.poke(false.B)
      dut.io.iMem.ready.expect(true.B)
    }
  }

  it should "nao aceitar endereco alem da capacidade nem deixar que ele retorne ao inicio" in {
    test(
      new CacheMainMemory(
        depthWords = 8,
        initialData = Seq(0x12345678L, 0x89ABCDEFL, 0x0BADF00DL, 0x55AA55AAL)
      )
    ) { dut =>
      initialize(dut)
      dut.io.dMem.request.poke(true.B)
      dut.io.dMem.address.poke(32.U) // capacidade: 8 palavras = 32 bytes

      dut.io.dMem.ready.expect(false.B)
      dut.io.iMem.ready.expect(false.B)
      // O pedido inválido não pode ser aceito nem retornar os dados do início.
      dut.io.dMem.write.poke(true.B)
      dut.io.dMem.writeLine(0).poke("hFFFFFFFF".U)
      dut.io.dMem.writeLine(1).poke("hFFFFFFFF".U)
      dut.io.dMem.writeLine(2).poke("hFFFFFFFF".U)
      dut.io.dMem.writeLine(3).poke("hFFFFFFFF".U)
      for (word <- 0 until CacheParams.wordsPerLine) {
        dut.io.dMem.readLine(word).expect(0.U)
      }
      dut.clock.step(1)

      dut.io.dMem.request.poke(false.B)
      dut.io.dMem.write.poke(false.B)
      dut.io.iMem.request.poke(true.B)
      dut.io.iMem.address.poke(0.U)
      dut.io.iMem.ready.expect(true.B)
      dut.io.iMem.readLine(0).expect("h12345678".U)
      dut.io.iMem.readLine(1).expect("h89ABCDEF".U)
      dut.io.iMem.readLine(2).expect("h0BADF00D".U)
      dut.io.iMem.readLine(3).expect("h55AA55AA".U)
    }
  }
}