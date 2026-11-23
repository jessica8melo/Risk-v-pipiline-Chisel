package riscv.cache

import chisel3._
import chisel3.stage.ChiselStage
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** O bit de sujeira so existe na DCache: na ICache (`hasDirty = false`) o
  * registrador `dirtyBits` nem e criado, em vez de ficar constante em zero e
  * depender da sintese para sumir.
  */
class CacheDirtyBitTest extends AnyFlatSpec with ChiselScalatestTester {

  private def chirrtl(gen: => RawModule): String =
    (new ChiselStage).emitChirrtl(gen)

  behavior of "Bit de sujeira (dirty)"

  it should "nao existir no circuito da ICache e existir no da DCache" in {
    val icache = chirrtl(new Cache(isInstructionCache = true))
    val dcache = chirrtl(new Cache(isInstructionCache = false))
    assert(
      !icache.contains("dirtyBits"),
      "a ICache nao deve ter armazenamento de dirty"
    )
    assert(dcache.contains("dirtyBits"), "a DCache precisa do bit de dirty")
  }

  private def initArrays(dut: CacheArrays): Unit = {
    dut.io.invalidateAll.poke(false.B)
    dut.io.readIndex.poke(0.U)
    dut.io.write.en.poke(false.B)
    dut.io.write.way.poke(0.U)
    dut.io.write.index.poke(0.U)
    dut.io.write.metaEn.poke(false.B)
    dut.io.write.tag.poke(0.U)
    dut.io.write.valid.poke(false.B)
    dut.io.write.dirty.poke(false.B)
    for (i <- 0 until CacheParams.wordsPerLine) {
      dut.io.write.wordMask(i).poke(true.B)
      dut.io.write.data(i).poke(0.U)
    }
  }

  /** Grava a linha (way 1, index 3) com tag 5, valid = 1 e o dirty dado. */
  private def writeLine(dut: CacheArrays, dirty: Boolean): Unit = {
    dut.io.write.en.poke(true.B)
    dut.io.write.way.poke(1.U)
    dut.io.write.index.poke(3.U)
    dut.io.write.metaEn.poke(true.B)
    dut.io.write.tag.poke(5.U)
    dut.io.write.valid.poke(true.B)
    dut.io.write.dirty.poke(dirty.B)
    dut.clock.step(1)
    dut.io.write.en.poke(false.B)
    dut.io.write.metaEn.poke(false.B)
    dut.io.readIndex.poke(3.U)
  }

  it should "ler sempre 0 e ignorar write.dirty em CacheArrays sem hasDirty" in {
    test(new CacheArrays(hasDirty = false)) { dut =>
      initArrays(dut)
      writeLine(dut, dirty = true) // o pedido de dirty e ignorado
      dut.io.read(1).valid.expect(true.B)
      dut.io.read(1).tag.expect(5.U)
      for (w <- 0 until CacheParams.ways) dut.io.read(w).dirty.expect(false.B)

      dut.io.invalidateAll.poke(true.B)
      dut.clock.step(1)
      dut.io.invalidateAll.poke(false.B)
      dut.io.read(1).valid.expect(false.B)
    }
  }

  it should "guardar e limpar o dirty em CacheArrays com hasDirty (padrao)" in {
    test(new CacheArrays) { dut =>
      initArrays(dut)
      writeLine(dut, dirty = true)
      dut.io.read(1).valid.expect(true.B)
      dut.io.read(1).dirty.expect(true.B)
      dut.io.read(0).dirty.expect(false.B)

      dut.io.invalidateAll.poke(true.B)
      dut.clock.step(1)
      dut.io.invalidateAll.poke(false.B)
      dut.io.read(1).valid.expect(false.B)
      dut.io.read(1).dirty.expect(false.B)
    }
  }
}
