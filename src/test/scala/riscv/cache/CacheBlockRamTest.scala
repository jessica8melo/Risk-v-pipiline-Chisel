package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random

/** L1Cache + CacheMainMemory com `CacheConfig` escolhida (para o blockRam). */
class L1CfgSystem(
    latency: Int,
    depthWords: Int,
    initialData: Seq[Long],
    cfg: CacheConfig
) extends Module {
  val io = IO(new Bundle {
    val iCache = Flipped(new CpuCacheIO)
    val dCache = Flipped(new CpuCacheIO)
    val fenceI = Input(Bool())
    val fenceIDone = Output(Bool())
  })

  val l1 = Module(new L1Cache(cfg))
  val mem = Module(
    new CacheMainMemory(depthWords, initialData, latency = latency, cfg = cfg)
  )

  l1.io.iMem <> mem.io.iMem
  l1.io.dMem <> mem.io.dMem
  l1.io.iCache <> io.iCache
  l1.io.dCache <> io.dCache
  l1.io.fenceI := io.fenceI
  io.fenceIDone := l1.io.fenceIDone
  l1.io.perfClear := false.B
}

/** Modo `blockRam` (leitura sincrona das RAMs, que mapeia em M10K).
  *
  * Custo esperado, que os testes conferem: hit continua em 2 ciclos; cada miss
  * ganha 1 ciclo (`sRefillWait`); o flush gasta 2 ciclos por conjunto. O
  * comportamento funcional tem de ser identico ao do modo padrao.
  */
class CacheBlockRamTest extends AnyFlatSpec with ChiselScalatestTester {

  private val cfg = CacheConfig.default.copy(blockRam = true)
  private val W = cfg.wordsPerLine
  private val WayStride = cfg.numSets * cfg.lineBytes // mesmo set, outra tag

  private def reset(dut: Module): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  // ------------------------------------------------------------------
  // Reagrupamento do hex (1 linha de cache por linha)
  // ------------------------------------------------------------------

  behavior of "HexLines"

  it should "agrupar 4 palavras por linha com a palavra 0 a direita" in {
    val g = HexLines.group(Seq(0x00500313L, 0x00a00393L, 0x007302b3L), 4)
    assert(g == Seq("00000000007302b300a0039300500313"))
  }

  it should "completar a ultima linha com zeros e manter as linhas cheias" in {
    val g = HexLines.group((1L to 5L).toSeq, 4)
    assert(
      g == Seq(
        "00000004000000030000000200000001",
        "00000000000000000000000000000005"
      )
    )
  }

  it should "ler palavras ignorando linhas vazias e comentarios" in {
    val w = HexLines.readWords(Seq("00500313", "", "// c", "# d", "a00393"))
    assert(w == Seq(0x00500313L, 0xa00393L))
    assertThrows[IllegalArgumentException](HexLines.readWords(Seq("xyz")))
  }

  // ------------------------------------------------------------------
  // Cache isolada, memoria modelada em Scala
  // ------------------------------------------------------------------

  private class MemModel {
    val lines = mutable.Map.empty[Long, Seq[Long]]
    def line(a: Long): Seq[Long] = lines.getOrElse(a, Seq.fill(W)(0L))
  }

  private def initCache(dut: Cache): Unit = {
    reset(dut)
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.io.cpu.address.poke(0.U)
    dut.io.cpu.writeData.poke(0.U)
    dut.io.cpu.memSize.poke(2.U)
    dut.io.cpu.unsignedLoad.poke(false.B)
    dut.io.mem.ready.poke(false.B)
    dut.io.mem.fault.poke(false.B)
    for (i <- 0 until W) dut.io.mem.readLine(i).poke(0.U)
    dut.io.flushDirty.poke(false.B)
    dut.io.invalidateAll.poke(false.B)
    dut.io.perfClear.poke(false.B)
  }

  /** Atende a cache com memoria de latencia zero (ready no mesmo ciclo). */
  private def serve(dut: Cache, mem: MemModel): Unit = {
    if (dut.io.mem.request.peekBoolean()) {
      val a = dut.io.mem.address.peekInt().toLong
      if (dut.io.mem.write.peekBoolean())
        mem.lines(a) =
          (0 until W).map(i => dut.io.mem.writeLine(i).peekInt().toLong)
      else {
        val l = mem.line(a)
        for (i <- 0 until W) dut.io.mem.readLine(i).poke(l(i).U)
      }
      dut.io.mem.ready.poke(true.B)
    } else dut.io.mem.ready.poke(false.B)
  }

  /** Executa um acesso ate o hit; devolve (dado lido, ciclos do pedido ao hit).
    */
  private def access(
      dut: Cache,
      mem: MemModel,
      addr: Long,
      write: Boolean = false,
      data: Long = 0
  ): (BigInt, Int) = {
    dut.io.cpu.request.poke(true.B)
    dut.io.cpu.write.poke(write.B)
    dut.io.cpu.address.poke(addr.U)
    dut.io.cpu.writeData.poke(data.U)
    var cycles = 0
    while (!dut.io.cpu.hit.peekBoolean()) {
      require(cycles < 60, s"timeout no acesso 0x${addr.toHexString}")
      serve(dut, mem)
      if (!dut.io.cpu.hit.peekBoolean()) {
        dut.clock.step(1)
        cycles += 1
      }
    }
    cycles += 1 // o ciclo do hit
    val value = dut.io.cpu.readData.peekInt()
    dut.clock.step(1)
    dut.io.mem.ready.poke(false.B)
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.clock.step(1)
    (value, cycles)
  }

  private def flush(dut: Cache, mem: MemModel): Int = {
    dut.io.flushDirty.poke(true.B)
    var c = 0
    while (!dut.io.flushDone.peekBoolean()) {
      require(c < 1000, "flush travado")
      serve(dut, mem)
      dut.clock.step(1)
      dut.io.mem.ready.poke(false.B)
      c += 1
    }
    dut.io.flushDirty.poke(false.B)
    dut.clock.step(2)
    c
  }

  behavior of "Cache com blockRam"

  it should "ter hit em 2 ciclos e 1 ciclo a mais por miss, com dados corretos" in {
    test(new Cache(cfg = cfg)) { dut =>
      initCache(dut)
      val mem = new MemModel
      mem.lines(0x100) = Seq(0x11, 0x22, 0x33, 0x44)

      val (v1, miss) = access(dut, mem, 0x100)
      assert(v1 == 0x11)
      // sIdle, sCompareTag (miss), sAllocate, sRefillWait, sCompareTag (hit)
      assert(miss == 5, s"miss: $miss ciclos")

      val (v2, hit) = access(dut, mem, 0x108) // mesma linha, palavra 2
      assert(v2 == 0x33)
      assert(hit == 2, s"hit: $hit ciclos")

      // misses = 1, hits = 1; a penalidade de miss tem 3 ciclos
      // (deteccao + sAllocate + sRefillWait)
      assert(dut.io.perf.misses.peekInt() == 1)
      assert(dut.io.perf.hits.peekInt() == 1)
      assert(dut.io.perf.missCycles.peekInt() == 3)
    }
  }

  it should "gravar stores, despejar a linha suja com o dado certo e reler" in {
    test(new Cache(cfg = cfg)) { dut =>
      initCache(dut)
      val mem = new MemModel
      access(dut, mem, 0x44, write = true, data = 0xaa) // miss + store
      assert(access(dut, mem, 0x44)._1 == 0xaa)
      assert(access(dut, mem, 0x40)._1 == 0, "palavra vizinha continua zero")

      // enche as 4 vias do set e despeja a linha suja (5a tag no mesmo set)
      for (k <- 1 to 4) access(dut, mem, 0x40L + k * WayStride)
      assert(mem.line(0x40)(1) == 0xaa, "write-back gravou o dado na memoria")
      assert(dut.io.perf.writeBacks.peekInt() == 1)
      // a linha despejada volta da memoria com o dado gravado
      assert(access(dut, mem, 0x44)._1 == 0xaa)
    }
  }

  it should "gastar 2 ciclos por conjunto no flush e gravar as linhas sujas" in {
    test(new Cache(cfg = cfg)) { dut =>
      initCache(dut)
      val mem = new MemModel
      access(dut, mem, 0x44, write = true, data = 0xbb) // set 4 suja
      val cycles = flush(dut, mem)
      assert(mem.line(0x40)(1) == 0xbb)
      assert(dut.io.perf.flushWriteBacks.peekInt() == 1)
      assert(cycles >= 2 * cfg.numSets, s"flush rapido demais: $cycles")
      assert(cycles <= 2 * cfg.numSets + 6, s"flush lento demais: $cycles")
      // invalidateAll continua funcionando: tudo volta a ser miss
      dut.io.invalidateAll.poke(true.B)
      dut.clock.step(1)
      dut.io.invalidateAll.poke(false.B)
      assert(access(dut, mem, 0x44)._2 == 5, "apos invalidar, o acesso e miss")
    }
  }

  it should "concordar com um modelo em Scala sob trafego aleatorio" in {
    test(new Cache(cfg = cfg)) { dut =>
      initCache(dut)
      val mem = new MemModel
      val model = mutable.Map.empty[Long, Long]
      val rnd = new Random(42)
      val bases = Seq(0x40L, 0x50L) // dois sets, 6 tags cada: forca despejos
      def addr(): Long =
        bases(rnd.nextInt(2)) + rnd.nextInt(6) * WayStride + rnd.nextInt(4) * 4L

      for (_ <- 0 until 150) {
        val a = addr()
        if (rnd.nextBoolean()) {
          val v = rnd.nextInt() & 0xffffffffL
          access(dut, mem, a, write = true, data = v)
          model(a) = v
        } else {
          val (v, _) = access(dut, mem, a)
          assert(
            v == BigInt(model.getOrElse(a, 0L)),
            s"load 0x${a.toHexString}"
          )
        }
      }
      // depois do flush, a memoria tem tudo o que foi escrito
      flush(dut, mem)
      for ((a, v) <- model) {
        val lineBase = a & ~(cfg.lineBytes - 1L)
        val word = ((a - lineBase) / 4).toInt
        assert(mem.line(lineBase)(word) == v, s"memoria em 0x${a.toHexString}")
      }
    }
  }

  // ------------------------------------------------------------------
  // CacheMainMemory com blockRam
  // ------------------------------------------------------------------

  private val memWords = 64 // 256 B
  private val memInit = Seq(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)

  private def initMem(dut: CacheMainMemory): Unit = {
    reset(dut)
    for (p <- Seq(dut.io.iMem, dut.io.dMem)) {
      p.request.poke(false.B)
      p.write.poke(false.B)
      p.address.poke(0.U)
      for (w <- 0 until W) p.writeLine(w).poke(0.U)
    }
  }

  private def waitDone(dut: CacheMainMemory, p: CacheMemIO): (Int, Boolean) = {
    var c = 0
    while (!p.ready.peekBoolean() && !p.fault.peekBoolean()) {
      require(c < 50, "memoria nao respondeu")
      dut.clock.step(1)
      c += 1
    }
    (c, p.fault.peekBoolean())
  }

  behavior of "CacheMainMemory com blockRam"

  it should "exigir latencia >= 1" in {
    val e = intercept[Throwable] {
      test(new CacheMainMemory(memWords, memInit, latency = 0, cfg = cfg)) {
        _ =>
      }
    }
    assert(e.toString.contains("latency"), e.toString)
  }

  for (latency <- Seq(1, 3)) {
    it should s"ler, escrever linhas e recusar enderecos invalidos (latencia $latency)" in {
      test(
        new CacheMainMemory(memWords, memInit, latency = latency, cfg = cfg)
      ) { dut =>
        initMem(dut)
        // leitura da ICache: linha 0 e linha 1 (conteudo inicial)
        for (
          (addr, expected) <- Seq(0 -> Seq(1, 2, 3, 4), 16 -> Seq(5, 6, 7, 8))
        ) {
          dut.io.iMem.request.poke(true.B)
          dut.io.iMem.address.poke(addr.U)
          val (c, f) = waitDone(dut, dut.io.iMem)
          assert(!f && c == latency, s"leitura em $c ciclos, esperado $latency")
          for (w <- 0 until W) dut.io.iMem.readLine(w).expect(expected(w).U)
          dut.clock.step(1)
          dut.io.iMem.request.poke(false.B)
          dut.clock.step(1)
        }

        // escrita da DCache na linha 1; depois a linha 0 continua inicial
        dut.io.dMem.request.poke(true.B)
        dut.io.dMem.write.poke(true.B)
        dut.io.dMem.address.poke(16.U)
        for (w <- 0 until W) dut.io.dMem.writeLine(w).poke((0xa0 + w).U)
        assert(!waitDone(dut, dut.io.dMem)._2)
        dut.clock.step(1)
        dut.io.dMem.request.poke(false.B)
        dut.io.dMem.write.poke(false.B)
        dut.clock.step(1)

        for (
          (addr, expected) <- Seq(
            16 -> Seq(0xa0, 0xa1, 0xa2, 0xa3),
            0 -> Seq(1, 2, 3, 4)
          )
        ) {
          dut.io.iMem.request.poke(true.B)
          dut.io.iMem.address.poke(addr.U)
          val (_, f) = waitDone(dut, dut.io.iMem)
          assert(!f)
          for (w <- 0 until W) dut.io.iMem.readLine(w).expect(expected(w).U)
          dut.clock.step(1)
          dut.io.iMem.request.poke(false.B)
          dut.clock.step(1)
        }

        // fora da faixa: fault no lugar de ready
        dut.io.dMem.request.poke(true.B)
        dut.io.dMem.address.poke(0x100.U)
        val (c, f) = waitDone(dut, dut.io.dMem)
        assert(f && c == latency)
        dut.io.dMem.ready.expect(false.B)
      }
    }
  }

  // ------------------------------------------------------------------
  // L1Cache + memoria (blockRam) com latencia: equivalencia funcional
  // ------------------------------------------------------------------

  private case class Op(addr: Long, write: Boolean = false, data: BigInt = 0)
  private case class Done(data: BigInt, cycles: Int)

  private val DataBase = 0x40L
  private val MemWords = 4096 // 16 KiB: cabe DataBase + 5 * WayStride
  private val codeWords: Seq[Long] =
    (0 until 16).map(i => 0x13L + (i.toLong << 7))

  private def initCpu(dut: L1CfgSystem): Unit = {
    reset(dut)
    for (p <- Seq(dut.io.iCache, dut.io.dCache)) {
      p.request.poke(false.B)
      p.write.poke(false.B)
      p.address.poke(0.U)
      p.writeData.poke(0.U)
      p.memSize.poke(2.U)
      p.unsignedLoad.poke(false.B)
    }
    dut.io.fenceI.poke(false.B)
  }

  private def drive(
      dut: L1CfgSystem,
      i: Option[Op],
      d: Option[Op]
  ): (Option[Done], Option[Done]) = {
    def issue(port: CpuCacheIO, op: Option[Op]): Unit = op match {
      case Some(o) =>
        port.request.poke(true.B)
        port.write.poke(o.write.B)
        port.address.poke(BigInt(o.addr).U(32.W))
        port.writeData.poke(o.data.U(32.W))
      case None => port.request.poke(false.B)
    }
    issue(dut.io.iCache, i)
    issue(dut.io.dCache, d)
    var iDone: Option[Done] = None
    var dDone: Option[Done] = None
    var cycle = 0
    while ((i.isDefined && iDone.isEmpty) || (d.isDefined && dDone.isEmpty)) {
      require(cycle < 800, s"timeout: i=$i d=$d")
      if (i.isDefined && iDone.isEmpty && dut.io.iCache.hit.peekBoolean())
        iDone = Some(Done(dut.io.iCache.readData.peekInt(), cycle + 1))
      if (d.isDefined && dDone.isEmpty && dut.io.dCache.hit.peekBoolean())
        dDone = Some(Done(dut.io.dCache.readData.peekInt(), cycle + 1))
      dut.clock.step(1)
      cycle += 1
      if (iDone.isDefined) dut.io.iCache.request.poke(false.B)
      if (dDone.isDefined) dut.io.dCache.request.poke(false.B)
    }
    dut.io.iCache.request.poke(false.B)
    dut.io.dCache.request.poke(false.B)
    dut.io.dCache.write.poke(false.B)
    (iDone, dDone)
  }

  private def fetch(dut: L1CfgSystem, a: Long): Done =
    drive(dut, Some(Op(a)), None)._1.get
  private def load(dut: L1CfgSystem, a: Long): Done =
    drive(dut, None, Some(Op(a)))._2.get
  private def store(dut: L1CfgSystem, a: Long, v: BigInt): Done =
    drive(dut, None, Some(Op(a, write = true, data = v)))._2.get

  private def fenceI(dut: L1CfgSystem): Int = {
    dut.io.fenceI.poke(true.B)
    var c = 0
    while (!dut.io.fenceIDone.peekBoolean()) {
      require(c < 4000, "FENCE.I travado")
      dut.clock.step(1)
      c += 1
    }
    dut.io.fenceI.poke(false.B)
    dut.clock.step(2)
    c
  }

  behavior of "L1Cache com blockRam e memoria lenta"

  it should "manter o comportamento: miss simultaneo, FENCE.I, despejo e trafego aleatorio" in {
    val latency = 2
    test(new L1CfgSystem(latency, MemWords, codeWords, cfg)) { dut =>
      // --- miss simultaneo de ICache e DCache: dados certos, atendidos em serie
      initCpu(dut)
      val (i, d) = drive(dut, Some(Op(0x28)), Some(Op(0x14)))
      assert(d.get.data == BigInt(codeWords(5)))
      assert(i.get.data == BigInt(codeWords(10)))
      assert(i.get.cycles >= d.get.cycles + latency)

      // --- store na DCache, FENCE.I, a ICache enxerga o dado novo
      initCpu(dut)
      store(dut, 0x44, 0xcafe0001L)
      val fc = fenceI(dut)
      assert(
        fc >= 2 * cfg.numSets,
        s"FENCE.I de $fc ciclos: devia gastar 2/conjunto"
      )
      assert(fetch(dut, 0x44).data == BigInt(0xcafe0001L))

      // --- despejo sujo com a ICache pedindo a mesma linha ao mesmo tempo
      initCpu(dut)
      store(dut, DataBase + 4, 0xbeef0001L)
      for (k <- 1 to 3) load(dut, DataBase + k * WayStride)
      val (ei, ed) =
        drive(dut, Some(Op(DataBase + 4)), Some(Op(DataBase + 4 * WayStride)))
      assert(ed.get.data == BigInt(0))
      assert(
        ei.get.data == BigInt(0xbeef0001L),
        "ICache leu dado velho da memoria"
      )

      // --- trafego aleatorio contra um modelo, com buscas simultaneas
      initCpu(dut)
      val rnd = new Random(7)
      val model = mutable.Map.empty[Long, BigInt]
      val bases = Seq(DataBase, DataBase + 0x10)
      def addr(): Long =
        bases(rnd.nextInt(2)) + rnd.nextInt(6) * WayStride + rnd.nextInt(4) * 4L
      for (_ <- 0 until 30) {
        val iAddr = rnd.nextInt(16) * 4L
        val dOp =
          if (rnd.nextBoolean())
            Op(addr(), write = true, data = BigInt(rnd.nextInt() & 0xffffffffL))
          else Op(addr())
        val withFetch = rnd.nextInt(3) != 0
        val (fi, fd) =
          drive(dut, if (withFetch) Some(Op(iAddr)) else None, Some(dOp))
        if (withFetch)
          assert(fi.get.data == BigInt(codeWords((iAddr / 4).toInt)))
        if (dOp.write) model(dOp.addr) = dOp.data
        else assert(fd.get.data == model.getOrElse(dOp.addr, BigInt(0)))
      }
      fenceI(dut)
      for ((a, v) <- model)
        assert(fetch(dut, a).data == v, s"memoria em 0x${a.toHexString}")
    }
  }
}
