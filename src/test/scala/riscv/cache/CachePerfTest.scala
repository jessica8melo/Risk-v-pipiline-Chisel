package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** L1Cache + CacheMainMemory expondo os contadores de desempenho. */
class L1PerfSystem(latency: Int, depthWords: Int, initialData: Seq[Long])
    extends Module {
  val io = IO(new Bundle {
    val iCache = Flipped(new CpuCacheIO)
    val dCache = Flipped(new CpuCacheIO)
    val fenceI = Input(Bool())
    val fenceIDone = Output(Bool())
    val iPerf = Output(new CachePerf)
    val dPerf = Output(new CachePerf)
    val perfClear = Input(Bool())
  })

  val l1 = Module(new L1Cache)
  val mem =
    Module(new CacheMainMemory(depthWords, initialData, latency = latency))

  l1.io.iMem <> mem.io.iMem
  l1.io.dMem <> mem.io.dMem
  l1.io.iCache <> io.iCache
  l1.io.dCache <> io.dCache
  l1.io.fenceI := io.fenceI
  io.fenceIDone := l1.io.fenceIDone
  io.iPerf := l1.io.iPerf
  io.dPerf := l1.io.dPerf
  l1.io.perfClear := io.perfClear
}

/** Contadores de hit/miss/write-back/fault/ciclos de miss por cache.
  *
  * A Cache e dirigida na mao com uma memoria de latencia zero (ready no mesmo
  * ciclo do pedido), entao um miss limpo custa exatamente 2 ciclos de miss
  * (deteccao em CompareTag + 1 ciclo de Allocate) e um miss com vitima suja
  * custa 3.
  */
class CachePerfTest extends AnyFlatSpec with ChiselScalatestTester {

  private val W = CacheParams.wordsPerLine
  private val WayStride =
    CacheParams.numSets * CacheParams.lineBytes // mesmo set

  private def reset(dut: Module): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  // ------------------------------------------------------------------
  // Cache isolada
  // ------------------------------------------------------------------

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

  private def issue(dut: Cache, addr: Long, write: Boolean = false): Unit = {
    dut.io.cpu.request.poke(true.B)
    dut.io.cpu.write.poke(write.B)
    dut.io.cpu.address.poke(addr.U)
    dut.io.cpu.writeData.poke(0x5a.U)
  }

  /** Atende o pedido (refill/write-back, latencia zero) ate o hit e o consome.
    */
  private def access(dut: Cache, addr: Long, write: Boolean = false): Unit = {
    issue(dut, addr, write)
    var c = 0
    while (!dut.io.cpu.hit.peekBoolean()) {
      require(c < 40, "timeout esperando hit")
      dut.io.mem.ready.poke(dut.io.mem.request.peekBoolean().B)
      dut.clock.step(1)
      dut.io.mem.ready.poke(false.B)
      c += 1
    }
    dut.clock.step(1) // consome o ciclo do hit
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.clock.step(1)
  }

  private def waitMemRequest(dut: Cache, write: Boolean): Unit = {
    var c = 0
    while (
      !(dut.io.mem.request
        .peekBoolean() && dut.io.mem.write.peekBoolean() == write)
    ) {
      require(c < 20, "a cache nao fez o pedido esperado a memoria")
      dut.clock.step(1)
      c += 1
    }
  }

  private case class P(
      hits: Int,
      misses: Int,
      wb: Int,
      flushWb: Int,
      faults: Int,
      missCycles: Int
  )

  private def perf(dut: Cache): P = P(
    dut.io.perf.hits.peekInt().toInt,
    dut.io.perf.misses.peekInt().toInt,
    dut.io.perf.writeBacks.peekInt().toInt,
    dut.io.perf.flushWriteBacks.peekInt().toInt,
    dut.io.perf.faults.peekInt().toInt,
    dut.io.perf.missCycles.peekInt().toInt
  )

  behavior of "Contadores de desempenho da Cache"

  it should "contar hits, misses, write-backs, flush, faults e ciclos de miss" in {
    test(new Cache) { dut =>
      // --- miss limpo, depois dois hits (load e store na mesma linha)
      initCache(dut)
      assert(perf(dut) == P(0, 0, 0, 0, 0, 0), "contadores comecam zerados")
      access(dut, 0x100) // miss + refill
      assert(perf(dut) == P(0, 1, 0, 0, 0, 2), "o hit pos-refill nao e hit")
      access(dut, 0x100) // hit
      access(dut, 0x104, write = true) // hit (store)
      assert(perf(dut) == P(2, 1, 0, 0, 0, 2), "hits + misses = pedidos")

      // --- despejo de linha suja: 5 misses, 1 write-back
      initCache(dut)
      access(dut, 0x40, write = true) // miss + store: linha suja
      for (k <- 1 to 3) access(dut, 0x40L + k * WayStride) // enche as 4 vias
      access(dut, 0x40L + 4 * WayStride) // despeja a suja
      // 4 misses limpos (2 ciclos) + 1 miss com write-back (3 ciclos)
      assert(perf(dut) == P(0, 5, 1, 0, 0, 11))

      // --- flush (FENCE.I): conta em flushWriteBacks, nao em writeBacks
      initCache(dut)
      access(dut, 0x40, write = true)
      dut.io.flushDirty.poke(true.B)
      var c = 0
      while (!dut.io.flushDone.peekBoolean()) {
        require(c < 400, "flush travado") // varre os 128 sets
        dut.io.mem.ready.poke(dut.io.mem.request.peekBoolean().B)
        dut.clock.step(1)
        dut.io.mem.ready.poke(false.B)
        c += 1
      }
      dut.io.flushDirty.poke(false.B)
      dut.clock.step(2)
      assert(perf(dut) == P(0, 1, 0, 1, 0, 2))

      // --- refill recusado: conta fault; o pedido refeito conta outro miss
      initCache(dut)
      issue(dut, 0x100)
      waitMemRequest(dut, write = false)
      dut.io.mem.fault.poke(true.B)
      dut.clock.step(1)
      dut.io.mem.fault.poke(false.B)
      dut.io.cpu.request.poke(false.B)
      dut.clock.step(2)
      assert(perf(dut).faults == 1)
      assert(perf(dut).misses == 1 && perf(dut).hits == 0)
      access(dut, 0x100)
      assert(perf(dut).misses == 2 && perf(dut).hits == 0)

      // --- perfClear zera tudo
      dut.io.perfClear.poke(true.B)
      dut.clock.step(1)
      dut.io.perfClear.poke(false.B)
      assert(perf(dut) == P(0, 0, 0, 0, 0, 0))
    }
  }

  // ------------------------------------------------------------------
  // L1Cache: contadores independentes por cache, com memoria lenta
  // ------------------------------------------------------------------

  private def run(
      dut: L1PerfSystem,
      port: CpuCacheIO,
      addr: Long,
      write: Boolean
  ): Unit = {
    port.request.poke(true.B)
    port.write.poke(write.B)
    port.address.poke(addr.U)
    port.writeData.poke(0x77.U)
    var c = 0
    while (!port.hit.peekBoolean()) {
      require(c < 100, "timeout esperando hit")
      dut.clock.step(1)
      c += 1
    }
    dut.clock.step(1)
    port.request.poke(false.B)
    port.write.poke(false.B)
    dut.clock.step(2)
  }

  behavior of "Contadores de desempenho do L1Cache"

  it should "manter contadores separados para ICache e DCache com memoria lenta" in {
    val latency = 3
    test(new L1PerfSystem(latency, 64, Seq(1L, 2L, 3L, 4L))) { dut =>
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
      dut.io.perfClear.poke(false.B)

      def i = dut.io.iPerf
      def d = dut.io.dPerf

      run(dut, dut.io.iCache, 0x0, write = false) // miss de instrucao
      run(dut, dut.io.iCache, 0x4, write = false) // hit de instrucao
      assert(i.hits.peekInt() == 1 && i.misses.peekInt() == 1)
      // miss = 1 ciclo de deteccao + (latencia + 1) ciclos de Allocate
      assert(i.missCycles.peekInt() == latency + 2)
      assert(
        d.hits.peekInt() == 0 && d.misses.peekInt() == 0,
        "DCache nao se mexeu"
      )

      run(dut, dut.io.dCache, 0x10, write = false) // miss de dado
      run(dut, dut.io.dCache, 0x14, write = true) // hit (store): linha suja
      assert(d.hits.peekInt() == 1 && d.misses.peekInt() == 1)
      assert(d.missCycles.peekInt() == latency + 2)
      assert(
        i.hits.peekInt() == 1 && i.misses.peekInt() == 1,
        "ICache nao se mexeu"
      )

      // FENCE.I: a DCache despeja a linha suja pelo flush
      dut.io.fenceI.poke(true.B)
      var c = 0
      while (!dut.io.fenceIDone.peekBoolean()) {
        require(c < 1000, "FENCE.I travado")
        dut.clock.step(1)
        c += 1
      }
      dut.io.fenceI.poke(false.B)
      dut.clock.step(2)
      assert(d.flushWriteBacks.peekInt() == 1)
      assert(d.writeBacks.peekInt() == 0)
      assert(i.flushWriteBacks.peekInt() == 0)

      dut.io.perfClear.poke(true.B)
      dut.clock.step(1)
      dut.io.perfClear.poke(false.B)
      for (p <- Seq(i, d)) {
        assert(p.hits.peekInt() == 0 && p.misses.peekInt() == 0)
        assert(p.flushWriteBacks.peekInt() == 0 && p.missCycles.peekInt() == 0)
      }
    }
  }
}
