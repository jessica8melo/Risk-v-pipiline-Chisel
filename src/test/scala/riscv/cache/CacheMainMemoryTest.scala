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
        initialData = Seq(0x12345678L, 0x89abcdefL, 0x0badf00dL, 0x55aa55aaL)
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

  // --- armazenamento em Mem + ROM de pre-carga ---------------------------------

  private def writeLine(
      dut: CacheMainMemory,
      address: Int,
      words: Seq[Long]
  ): Unit = {
    dut.io.dMem.request.poke(true.B)
    dut.io.dMem.write.poke(true.B)
    dut.io.dMem.address.poke(address.U)
    for (word <- words.indices)
      dut.io.dMem.writeLine(word).poke(words(word).U(32.W))
    dut.io.dMem.ready.expect(true.B)
    dut.clock.step(1)
    dut.io.dMem.request.poke(false.B)
    dut.io.dMem.write.poke(false.B)
  }

  private def readLine(dut: CacheMainMemory, address: Int): Seq[BigInt] = {
    dut.io.iMem.request.poke(true.B)
    dut.io.iMem.address.poke(address.U)
    dut.io.iMem.ready.expect(true.B)
    val words = (0 until CacheParams.wordsPerLine).map(w =>
      dut.io.iMem.readLine(w).peekInt()
    )
    dut.io.iMem.request.poke(false.B)
    words
  }

  it should "devolver zero fora da area pre-carregada e sem dados iniciais" in {
    test(
      new CacheMainMemory(
        depthWords = 16,
        initialData = Seq(1L, 2L, 3L, 4L, 5L)
      )
    ) { dut =>
      initialize(dut)
      assert(readLine(dut, 0) == Seq(1, 2, 3, 4).map(BigInt(_)))
      // linha 1 tem so a primeira palavra pre-carregada; o resto precisa ser zero
      assert(readLine(dut, 16) == Seq(5, 0, 0, 0).map(BigInt(_)))
      assert(readLine(dut, 32) == Seq.fill(4)(BigInt(0)))
      assert(readLine(dut, 48) == Seq.fill(4)(BigInt(0)))
    }
    test(new CacheMainMemory(depthWords = 16)) { dut =>
      initialize(dut)
      for (address <- Seq(0, 16, 32, 48)) {
        assert(
          readLine(dut, address) == Seq.fill(4)(BigInt(0)),
          s"linha em $address"
        )
      }
    }
  }

  it should "sobrescrever a area pre-carregada sem afetar o restante" in {
    test(
      new CacheMainMemory(
        depthWords = 16,
        initialData = Seq(1L, 2L, 3L, 4L, 5L, 6L)
      )
    ) { dut =>
      initialize(dut)
      writeLine(dut, 16, Seq(0xaaL, 0xbbL, 0xccL, 0xddL))

      assert(readLine(dut, 16) == Seq(0xaa, 0xbb, 0xcc, 0xdd).map(BigInt(_)))
      assert(
        readLine(dut, 0) == Seq(1, 2, 3, 4).map(BigInt(_)),
        "linha 0 nao pode mudar"
      )

      // escrever uma linha fora da ROM nao pode reviver o conteudo da ROM
      writeLine(dut, 32, Seq(9L, 8L, 7L, 6L))
      assert(readLine(dut, 32) == Seq(9, 8, 7, 6).map(BigInt(_)))
      assert(readLine(dut, 16) == Seq(0xaa, 0xbb, 0xcc, 0xdd).map(BigInt(_)))

      // reescrever a linha 0 com zeros: o valor novo vence o pre-carregado
      writeLine(dut, 0, Seq(0L, 0L, 0L, 0L))
      assert(readLine(dut, 0) == Seq.fill(4)(BigInt(0)))
    }
  }

  it should "funcionar quando initialData ocupa a memoria inteira" in {
    val data = (1L to 8L).toSeq
    test(new CacheMainMemory(depthWords = 8, initialData = data)) { dut =>
      initialize(dut)
      assert(readLine(dut, 0) == data.take(4).map(BigInt(_)))
      assert(readLine(dut, 16) == data.drop(4).map(BigInt(_)))
      writeLine(dut, 16, Seq(0x10L, 0x20L, 0x30L, 0x40L))
      assert(readLine(dut, 16) == Seq(0x10, 0x20, 0x30, 0x40).map(BigInt(_)))
      assert(readLine(dut, 0) == data.take(4).map(BigInt(_)))
    }
  }

  it should "preservar palavras com bit 31 em 1 na pre-carga" in {
    test(
      new CacheMainMemory(
        depthWords = 8,
        initialData = Seq(0xffffffffL, 0x80000000L, 0L, 1L)
      )
    ) { dut =>
      initialize(dut)
      assert(
        readLine(dut, 0) == Seq(
          BigInt(0xffffffffL),
          BigInt(0x80000000L),
          BigInt(0),
          BigInt(1)
        )
      )
    }
  }
}
