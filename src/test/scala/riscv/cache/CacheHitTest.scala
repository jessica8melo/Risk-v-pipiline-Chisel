package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

/** Caminho de acerto (leitura e escrita) exercitado em TODAS as vias.
  *
  * As 4 linhas usadas mapeiam para o mesmo conjunto (index 5) com tags
  * diferentes; depois de preenchidas ocupam as 4 vias. Em vez de assumir a
  * politica de vitima, o teste le a via de acerto pela porta `io.dbg` e exige
  * que as 4 linhas acertem em 4 vias distintas cobrindo {0,1,2,3}.
  */
class CacheHitTest extends AnyFlatSpec with ChiselScalatestTester with CacheTestHelpers {
  behavior of "Cache (caminho de acerto)"

  private val Ways = CacheParams.ways
  private val Base = 5L * CacheParams.lineBytes // conjunto 5
  private val OtherBase = 6L * CacheParams.lineBytes // conjunto 6 (vizinho)
  private def lineAddr(base: Long, k: Int): Long = base + k.toLong * SetStride

  /** Conteudo original da palavra i da linha k (unico por linha/palavra). */
  private def orig(k: Int, i: Int): BigInt = BigInt(0x10000000L * (k + 1) + i * 0x101L + 0x5a)

  private def load(dut: Cache, mem: MemModel, base: Long, k: Int): Unit = {
    mem.preload(lineAddr(base, k), (0 until W).map(i => orig(k, i)))
    val r = access(dut, mem, lineAddr(base, k))
    assert(r.data == orig(k, 0))
  }

  /** Preenche as 4 vias do conjunto 5 (4 misses). */
  private def fillSet(dut: Cache, mem: MemModel): Unit = {
    val readsBefore = mem.reads
    for (k <- 0 until Ways) load(dut, mem, Base, k)
    assert(mem.reads - readsBefore == Ways && mem.writes == 0)
  }

  it should "acertar leitura em todas as vias e em todas as palavras da linha" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      fillSet(dut, mem)

      val wayOfLine = Array.fill(Ways)(-1)
      for (k <- 0 until Ways; i <- 0 until W) {
        val r = access(dut, mem, lineAddr(Base, k) + 4L * i)
        assert(r.data == orig(k, i), s"linha $k palavra $i")
        assert(r.cycles == 2, "hit = Idle + CompareTag")
        assert(!r.hitDirty, "leitura nao suja a linha")
        if (wayOfLine(k) == -1) wayOfLine(k) = r.hitWay
        assert(r.hitWay == wayOfLine(k), "todas as palavras da linha estao na mesma via")
      }
      assert(wayOfLine.toSet == (0 until Ways).toSet, s"vias usadas: ${wayOfLine.toSeq}")
      assert(mem.reads == Ways && mem.writes == 0, "hit nao acessa a memoria")
    }
  }

  it should "acertar alternando entre as vias (round-robin) sem interferencia" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      fillSet(dut, mem)
      for (round <- 0 until 3; k <- 0 until Ways) {
        val i = (round + k) % W
        assert(access(dut, mem, lineAddr(Base, k) + 4L * i).data == orig(k, i))
      }
      assert(mem.reads == Ways)
    }
  }

  it should "acertar escrita em todas as vias, marcando dirty so na linha escrita" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      load(dut, mem, OtherBase, 8) // linha vizinha (conjunto 6), nao pode ser afetada
      fillSet(dut, mem)
      val readsAfterFill = mem.reads

      def newVal(k: Int): BigInt = BigInt(0xdead0000L + k)

      // antes: todas limpas
      for (k <- 0 until Ways)
        assert(!access(dut, mem, lineAddr(Base, k)).hitDirty)

      // store hit: uma palavra diferente por via/linha
      val wayOfLine = Array.fill(Ways)(-1)
      for (k <- 0 until Ways) {
        val r = access(dut, mem, lineAddr(Base, k) + 4L * k, write = true, data = newVal(k))
        assert(r.cycles == 2, "store hit = Idle + CompareTag")
        assert(!r.hitDirty, "dirty observado antes da escrita ainda e 0")
        wayOfLine(k) = r.hitWay
      }
      assert(wayOfLine.toSet == (0 until Ways).toSet, s"vias usadas: ${wayOfLine.toSeq}")
      assert(mem.reads == readsAfterFill && mem.writes == 0, "store hit nao acessa a memoria")

      // depois: so a palavra escrita mudou; linha suja; via preservada
      for (k <- 0 until Ways; i <- 0 until W) {
        val r = access(dut, mem, lineAddr(Base, k) + 4L * i)
        val expected = if (i == k) newVal(k) else orig(k, i)
        assert(r.data == expected, s"linha $k palavra $i")
        assert(r.hitDirty, s"linha $k deveria estar suja")
        assert(r.hitWay == wayOfLine(k), "store nao muda a via da linha")
      }

      // vizinho intacto e limpo
      for (i <- 0 until W) {
        val r = access(dut, mem, lineAddr(OtherBase, 8) + 4L * i)
        assert(r.data == orig(8, i))
        assert(!r.hitDirty)
      }
      assert(mem.writes == 0)
    }
  }

  it should "acertar store de byte e half em todas as vias (merge na palavra)" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      fillSet(dut, mem)

      for (k <- 0 until Ways) {
        // sb no byte k da palavra 0
        val b = BigInt(0xa0 + k)
        access(dut, mem, lineAddr(Base, k) + k, write = true, data = b, size = 0)
        val expW0 = (orig(k, 0) & ~(BigInt(0xff) << (8 * k))) | (b << (8 * k))
        assert(access(dut, mem, lineAddr(Base, k)).data == expW0, s"sb via da linha $k")
        assert(access(dut, mem, lineAddr(Base, k) + k, size = 0, unsigned = true).data == b)
        assert(
          access(dut, mem, lineAddr(Base, k) + k, size = 0).data == BigInt(0xffffff00L | (0xa0 + k))
        )

        // sh na metade alta da palavra 1
        val h = BigInt(0xbe00 + k)
        access(dut, mem, lineAddr(Base, k) + 6, write = true, data = h, size = 1)
        val expW1 = (orig(k, 1) & BigInt(0xffff)) | (h << 16)
        assert(access(dut, mem, lineAddr(Base, k) + 4).data == expW1, s"sh via da linha $k")
        assert(access(dut, mem, lineAddr(Base, k) + 6, size = 1, unsigned = true).data == h)
      }
      assert(mem.reads == Ways && mem.writes == 0)
    }
  }

  it should "acertar na ICache em todas as vias, sempre devolvendo a palavra inteira" in {
    test(new Cache(isInstructionCache = true)) { dut =>
      initIO(dut)
      val mem = new MemModel
      fillSet(dut, mem)

      val wayOfLine = Array.fill(Ways)(-1)
      for (k <- 0 until Ways; i <- 0 until W) {
        // memSize = byte de proposito: a ICache deve ignora-lo
        val r = access(dut, mem, lineAddr(Base, k) + 4L * i, size = 0)
        assert(r.data == orig(k, i), s"linha $k palavra $i")
        assert(r.cycles == 2)
        if (wayOfLine(k) == -1) wayOfLine(k) = r.hitWay
      }
      assert(wayOfLine.toSet == (0 until Ways).toSet, s"vias usadas: ${wayOfLine.toSeq}")
      assert(mem.reads == Ways)
    }
  }
}