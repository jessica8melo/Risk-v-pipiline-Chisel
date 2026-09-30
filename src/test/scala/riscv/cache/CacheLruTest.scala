package riscv.cache

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

/** Marca testes demorados no Treadle. Pular: `-- -l Slow`; so eles: `-- -n Slow`. */
object Slow extends Tag("Slow")

/** Reposicao LRU (4-way) e write-back de linhas sujas em todas as vias. */
class CacheLruTest extends AnyFlatSpec with ChiselScalatestTester with CacheTestHelpers {
  behavior of "Cache (LRU e write-back)"

  private val Ways = CacheParams.ways
  private val Base = 5L * CacheParams.lineBytes // conjunto 5
  private val OtherBase = 6L * CacheParams.lineBytes // conjunto 6
  private def addrOf(base: Long, k: Int): Long = base + k.toLong * SetStride

  private def orig(k: Int, i: Int): BigInt = BigInt(0x10000000L * (k + 1) + i * 0x101L + 0x5a)

  private def preload(mem: MemModel, base: Long, n: Int): Unit =
    for (k <- 0 until n) mem.preload(addrOf(base, k), (0 until W).map(i => orig(k, i)))

  private def isHit(r: Result): Boolean = r.cycles == 2

  // Opcional: SIM=verilator sbt ... usa Verilator (bem mais rapido, requer instalacao).
  private val simAnnos =
    if (sys.env.get("SIM").contains("verilator")) Seq(VerilatorBackendAnnotation) else Seq()

  it should "despejar a menos recentemente usada, nao a mais antiga" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      preload(mem, Base, 6)

      // preenche a0..a3 (ordem de uso: a0 < a1 < a2 < a3) e guarda a via de cada uma
      val wayOf = (0 until Ways).map(k => access(dut, mem, addrOf(Base, k)).hitWay)
      assert(wayOf.toSet == (0 until Ways).toSet)

      // reusa a0: agora a menos recente e a1
      assert(isHit(access(dut, mem, addrOf(Base, 0))))

      // miss em a4: deve ocupar a via de a1
      val r4 = access(dut, mem, addrOf(Base, 4))
      assert(!isHit(r4))
      assert(r4.hitWay == wayOf(1), "vitima deveria ser a via de a1")
      assert(mem.reads == Ways + 1 && mem.writes == 0)

      // a2, a3, a0, a4 continuam na cache (hits, sem ir a memoria)
      for (k <- Seq(2, 3, 0, 4)) assert(isHit(access(dut, mem, addrOf(Base, k))), s"a$k")
      assert(mem.reads == Ways + 1)

      // a1 foi despejada; a ordem de uso agora e a2 < a3 < a0 < a4, logo a1 entra na via de a2
      val r1 = access(dut, mem, addrOf(Base, 1))
      assert(!isHit(r1))
      assert(r1.hitWay == wayOf(2), "vitima deveria ser a via de a2")
      assert(mem.reads == Ways + 2)
    }
  }

  it should "fazer write-back de linha suja em todas as vias, na ordem LRU" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      preload(mem, Base, 8)
      def newVal(k: Int): BigInt = BigInt(0xdead0000L + k)

      // suja as 4 vias: store na palavra k da linha k
      val wayOf = (0 until Ways).map { k =>
        access(dut, mem, addrOf(Base, k) + 4L * k, write = true, data = newVal(k)).hitWay
      }
      assert(wayOf.toSet == (0 until Ways).toSet)
      assert(mem.reads == Ways && mem.writes == 0)

      // 4 misses novos despejam a0..a3 nessa ordem, cada uma com write-back
      for (k <- 0 until Ways) {
        val r = access(dut, mem, addrOf(Base, 4 + k))
        assert(!isHit(r))
        assert(r.hitWay == wayOf(k), s"a${4 + k} deveria ocupar a via de a$k")
        assert(mem.writes == k + 1, s"write-back $k")
        val wb = mem.lines(BigInt(addrOf(Base, k)))
        for (i <- 0 until W)
          assert(wb(i) == (if (i == k) newVal(k) else orig(k, i)), s"linha $k palavra $i")
      }

      // as 4 novas linhas sao limpas: despeja-las nao gera write-back
      for (k <- 0 until Ways) {
        val r = access(dut, mem, addrOf(Base, k) + 4L * k) // recarrega a_k da memoria
        assert(!isHit(r))
        assert(r.data == newVal(k), s"dado da linha $k sobreviveu ao write-back")
      }
      assert(mem.writes == Ways, "despejo de linha limpa nao faz write-back")
    }
  }

  it should "manter o LRU independente por conjunto" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      preload(mem, Base, 5)
      preload(mem, OtherBase, 5)

      for (k <- 0 until Ways) access(dut, mem, addrOf(OtherBase, k)) // conjunto 6: b0 < b1 < b2 < b3
      for (k <- 0 until Ways) access(dut, mem, addrOf(Base, k)) // conjunto 5
      for (k <- Seq(3, 2, 1, 0, 3)) access(dut, mem, addrOf(Base, k)) // mexe so no conjunto 5
      val readsBefore = mem.reads

      // miss no conjunto 6 despeja b0, sem ser afetado pela atividade do conjunto 5
      assert(!isHit(access(dut, mem, addrOf(OtherBase, 4))))
      for (k <- Seq(1, 2, 3, 4)) assert(isHit(access(dut, mem, addrOf(OtherBase, k))), s"b$k")
      assert(mem.reads == readsBefore + 1)
      assert(!isHit(access(dut, mem, addrOf(OtherBase, 0))), "b0 foi despejada")
    }
  }

  it should "coincidir com um modelo LRU de referencia (hits, misses e write-backs)" taggedAs Slow in {
    test(new Cache).withAnnotations(simAnnos) { dut =>
      initIO(dut)
      val mem = new MemModel
      val nTags = 6 // mais tags que vias: forca despejos
      preload(mem, Base, nTags)

      val golden = mutable.Map[Long, BigInt]()
      for (k <- 0 until nTags; i <- 0 until W) golden(addrOf(Base, k) + 4L * i) = orig(k, i)

      var order = Vector.empty[Int] // tags residentes, MRU primeiro
      val dirty = mutable.Set[Int]()
      var expectedReads = 0
      var expectedWb = 0
      val rnd = new scala.util.Random(1234)

      for (n <- 0 until 80) {
        val tag = rnd.nextInt(nTags)
        val a = addrOf(Base, tag) + 4L * rnd.nextInt(W)
        val isStore = rnd.nextInt(3) == 0

        // modelo de referencia
        val modelHit = order.contains(tag)
        if (modelHit) {
          order = order.filterNot(_ == tag)
        } else {
          expectedReads += 1
          if (order.size == Ways) {
            val victim = order.last
            if (dirty.remove(victim)) expectedWb += 1
            order = order.init
          }
        }
        order = tag +: order
        if (isStore) dirty += tag

        // hardware
        val r =
          if (isStore) {
            val v = BigInt(rnd.nextLong() & 0xffffffffL)
            golden(a) = v
            access(dut, mem, a, write = true, data = v)
          } else {
            access(dut, mem, a)
          }

        assert(isHit(r) == modelHit, s"acesso $n (tag $tag): hit/miss divergiu do modelo")
        if (!isStore) assert(r.data == golden(a), s"acesso $n: dado errado")
        assert(mem.reads == expectedReads, s"acesso $n: leituras da memoria")
        assert(mem.writes == expectedWb, s"acesso $n: write-backs")
      }

      // nada se perdeu no caminho (inclui o que foi despejado sujo)
      for (k <- 0 until nTags; i <- 0 until W) {
        val a = addrOf(Base, k) + 4L * i
        assert(access(dut, mem, a).data == golden(a), s"varredura final tag $k palavra $i")
      }
    }
  }
}