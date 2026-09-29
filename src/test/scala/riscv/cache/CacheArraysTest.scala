package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CacheArraysTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CacheArrays"

  private def noWrite(dut: CacheArrays): Unit = {
    dut.io.write.en.poke(false.B)
    dut.io.write.metaEn.poke(false.B)
    for (i <- 0 until CacheParams.wordsPerLine) dut.io.write.wordMask(i).poke(false.B)
  }

  private def writeLine(
      dut: CacheArrays, way: Int, index: Int, tag: Int, words: Seq[Long],
      mask: Seq[Boolean], meta: Boolean, valid: Boolean, dirty: Boolean
  ): Unit = {
    dut.io.write.en.poke(true.B)
    dut.io.write.way.poke(way.U)
    dut.io.write.index.poke(index.U)
    dut.io.write.metaEn.poke(meta.B)
    dut.io.write.tag.poke(tag.U)
    dut.io.write.valid.poke(valid.B)
    dut.io.write.dirty.poke(dirty.B)
    for (i <- 0 until CacheParams.wordsPerLine) {
      dut.io.write.data(i).poke(words(i).U)
      dut.io.write.wordMask(i).poke(mask(i).B)
    }
    dut.clock.step(1)
    noWrite(dut)
  }

  it should "iniciar com todas as linhas invalidas e limpas" in {
    test(new CacheArrays) { dut =>
      noWrite(dut)
      for (idx <- Seq(0, 1, 63, CacheParams.numSets - 1)) {
        dut.io.readIndex.poke(idx.U)
        for (w <- 0 until CacheParams.ways) {
          dut.io.read(w).valid.expect(false.B)
          dut.io.read(w).dirty.expect(false.B)
        }
      }
    }
  }

  it should "gravar e ler de volta linha completa (leitura combinacional)" in {
    test(new CacheArrays) { dut =>
      noWrite(dut)
      val words = Seq(0x11111111L, 0x22222222L, 0x33333333L, 0x44444444L)
      writeLine(dut, way = 2, index = 5, tag = 0x1abcde, words, Seq.fill(4)(true),
        meta = true, valid = true, dirty = false)

      dut.io.readIndex.poke(5.U) // sem step: leitura combinacional
      dut.io.read(2).tag.expect(0x1abcde.U)
      dut.io.read(2).valid.expect(true.B)
      dut.io.read(2).dirty.expect(false.B)
      for (i <- 0 until 4) dut.io.read(2).data(i).expect(words(i).U)

      // outras vias e outros conjuntos nao sao afetados
      dut.io.read(0).valid.expect(false.B)
      dut.io.read(3).valid.expect(false.B)
      dut.io.readIndex.poke(6.U)
      dut.io.read(2).valid.expect(false.B)
    }
  }

  it should "gravar so a palavra mascarada e marcar dirty sem tocar nas demais" in {
    test(new CacheArrays) { dut =>
      noWrite(dut)
      val base = Seq(1L, 2L, 3L, 4L)
      writeLine(dut, 0, 9, 0x10, base, Seq.fill(4)(true), true, true, false)
      writeLine(dut, 0, 9, 0x10, Seq(0L, 0xdeadbeefL, 0L, 0L),
        Seq(false, true, false, false), true, true, true)

      dut.io.readIndex.poke(9.U)
      dut.io.read(0).data(0).expect(1.U)
      dut.io.read(0).data(1).expect(0xdeadbeefL.U)
      dut.io.read(0).data(2).expect(3.U)
      dut.io.read(0).data(3).expect(4.U)
      dut.io.read(0).dirty.expect(true.B)
    }
  }

  it should "nao alterar tag/valid/dirty quando metaEn = 0" in {
    test(new CacheArrays) { dut =>
      noWrite(dut)
      writeLine(dut, 1, 3, 0x7, Seq(9L, 9L, 9L, 9L), Seq.fill(4)(true),
        meta = false, valid = true, dirty = true)
      dut.io.readIndex.poke(3.U)
      dut.io.read(1).valid.expect(false.B)
      dut.io.read(1).dirty.expect(false.B)
      dut.io.read(1).data(0).expect(9.U)
    }
  }
}