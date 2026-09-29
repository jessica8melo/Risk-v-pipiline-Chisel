package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** Wrapper minimo so para conseguir testar CacheAddress.fromUInt
  * (ele cria um Wire, entao precisa estar dentro de um Module).
  */
class CacheAddressWrapper extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val decoded = Output(new CacheAddress)
  })
  io.decoded := CacheAddress.fromUInt(io.addr)
}

class CacheTest extends AnyFlatSpec with ChiselScalatestTester {

  // ---------------------------------------------------------------------
  behavior of "CacheParams"

  it should "derivar a geometria esperada (8KiB, 4-way, linha de 16B)" in {
    assert(CacheParams.lineBytes == 16)
    assert(CacheParams.wordsPerLine == 4)
    assert(CacheParams.bytesPerCache == 8 * 1024)
    assert(CacheParams.numLines == 512)
    assert(CacheParams.numSets == 128)
    assert(CacheParams.offsetBits == 4)
    assert(CacheParams.indexBits == 7)
    assert(CacheParams.tagBits == 21)
  }

  it should "somar tag + index + offset em 32 bits" in {
    assert(
      CacheParams.tagBits + CacheParams.indexBits + CacheParams.offsetBits == 32
    )
  }

  // ---------------------------------------------------------------------
  behavior of "CacheAddress"

  private def expectedFields(addr: BigInt): (BigInt, BigInt, BigInt) = {
    val offset = addr & ((BigInt(1) << CacheParams.offsetBits) - 1)
    val index = (addr >> CacheParams.offsetBits) &
      ((BigInt(1) << CacheParams.indexBits) - 1)
    val tag = addr >> (CacheParams.offsetBits + CacheParams.indexBits)
    (tag, index, offset)
  }

  it should "decompor o endereco em tag/index/offset" in {
    test(new CacheAddressWrapper) { dut =>
      val addrs = Seq(
        BigInt(0x00000000L),
        BigInt(0x0000000fL),
        BigInt(0x00000010L),
        BigInt(0x12345678L),
        BigInt(0xdeadbeefL),
        BigInt(0xffffffffL)
      )

      for (a <- addrs) {
        val (tag, index, offset) = expectedFields(a)
        dut.io.addr.poke(a.U(32.W))
        dut.clock.step(1)
        dut.io.decoded.tag.expect(tag.U)
        dut.io.decoded.index.expect(index.U)
        dut.io.decoded.offset.expect(offset.U)
      }
    }
  }

  // ---------------------------------------------------------------------
  behavior of "Cache (esqueleto atual)"

  private def idle(dut: Cache): Unit = {
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.io.cpu.address.poke(0.U)
    dut.io.cpu.writeData.poke(0.U)
    dut.io.cpu.memSize.poke(0.U)
    dut.io.cpu.unsignedLoad.poke(false.B)
    dut.io.mem.ready.poke(false.B)
    for (i <- 0 until CacheParams.wordsPerLine)
      dut.io.mem.readLine(i).poke(0.U)
  }

  it should "nao dar stall nem hit sem pedido da CPU" in {
    test(new Cache) { dut =>
      idle(dut)
      dut.clock.step(1)

      dut.io.cpu.stall.expect(false.B)
      dut.io.cpu.hit.expect(false.B)
      dut.io.mem.request.expect(false.B)
    }
  }

  it should "dar stall e nunca hit quando ha pedido (sem logica real ainda)" in {
    test(new Cache) { dut =>
      idle(dut)
      dut.io.cpu.request.poke(true.B)
      dut.io.cpu.address.poke(0x1000.U)
      dut.clock.step(1)

      dut.io.cpu.stall.expect(true.B)
      dut.io.cpu.hit.expect(false.B)
    }
  }

  it should "nao disparar pedido a memoria por enquanto" in {
    test(new Cache) { dut =>
      idle(dut)
      dut.io.cpu.request.poke(true.B)
      dut.io.cpu.address.poke(0x2000.U)
      dut.clock.step(2)

      dut.io.mem.request.expect(false.B)
      dut.io.mem.write.expect(false.B)
    }
  }

  it should "aceitar leitura na ICache sem violar o assert" in {
    test(new Cache(isInstructionCache = true)) { dut =>
      idle(dut)
      dut.io.cpu.request.poke(true.B)
      dut.io.cpu.write.poke(false.B)
      dut.io.cpu.address.poke(0x0.U)
      dut.clock.step(2)

      dut.io.cpu.hit.expect(false.B)
    }
  }

  // ---------------------------------------------------------------------
  behavior of "L1Cache"

  private def idleL1(dut: L1Cache): Unit = {
    for (p <- Seq(dut.io.iCache, dut.io.dCache)) {
      p.request.poke(false.B)
      p.write.poke(false.B)
      p.address.poke(0.U)
      p.writeData.poke(0.U)
      p.memSize.poke(0.U)
      p.unsignedLoad.poke(false.B)
    }
    for (m <- Seq(dut.io.iMem, dut.io.dMem)) {
      m.ready.poke(false.B)
      for (i <- 0 until CacheParams.wordsPerLine) m.readLine(i).poke(0.U)
    }
  }

  it should "manter ICache e DCache independentes" in {
    test(new L1Cache) { dut =>
      idleL1(dut)
      dut.clock.step(1)
      dut.io.iCache.stall.expect(false.B)
      dut.io.dCache.stall.expect(false.B)

      // Pedido so na ICache: so ela deve ficar em stall.
      dut.io.iCache.request.poke(true.B)
      dut.io.iCache.address.poke(0x100.U)
      dut.clock.step(1)
      dut.io.iCache.stall.expect(true.B)
      dut.io.dCache.stall.expect(false.B)

      // Agora so na DCache.
      dut.io.iCache.request.poke(false.B)
      dut.io.dCache.request.poke(true.B)
      dut.io.dCache.address.poke(0x200.U)
      dut.io.dCache.write.poke(true.B)
      dut.io.dCache.writeData.poke("hCAFEBABE".U)
      dut.clock.step(1)
      dut.io.iCache.stall.expect(false.B)
      dut.io.dCache.stall.expect(true.B)
    }
  }

  it should "nao gerar hit em nenhuma das caches por enquanto" in {
    test(new L1Cache) { dut =>
      idleL1(dut)
      dut.io.iCache.request.poke(true.B)
      dut.io.dCache.request.poke(true.B)
      dut.clock.step(1)

      dut.io.iCache.hit.expect(false.B)
      dut.io.dCache.hit.expect(false.B)
    }
  }
}