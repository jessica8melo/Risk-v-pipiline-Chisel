package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.io.{File, PrintWriter}
import scala.collection.mutable

/** Cache + memoria principal (latencia 0) para a varredura de geometria. */
class CacheUnderTest(cfg: CacheConfig, depthWords: Int) extends Module {
  val io = IO(new Bundle {
    val cpu = Flipped(new CpuCacheIO)
    val perf = Output(new CachePerf)
    val flushDirty = Input(Bool())
    val flushDone = Output(Bool())
  })
  val cache = Module(new Cache(isInstructionCache = false, cfg = cfg))
  val mem = Module(new CacheMainMemory(depthWords, cfg = cfg))
  cache.io.cpu <> io.cpu
  cache.io.mem <> mem.io.dMem
  mem.io.iMem.request := false.B
  mem.io.iMem.write := false.B
  mem.io.iMem.address := 0.U
  mem.io.iMem.writeLine := VecInit(Seq.fill(cfg.wordsPerLine)(0.U(32.W)))
  cache.io.flushDirty := io.flushDirty
  cache.io.invalidateAll := false.B
  cache.io.perfClear := false.B
  io.flushDone := cache.io.flushDone
  io.perf := cache.io.perf
}

/** Modelo de referencia (so politicas deterministicas) com a mesma regra da
  * Cache: via invalida de menor indice primeiro, politica so com o conjunto
  * cheio.
  */
class RefCache(cfg: CacheConfig) {
  private case class Line(tag: Long, var dirty: Boolean)
  private val lines =
    Array.fill(cfg.numSets)(Array.fill[Option[Line]](cfg.ways)(None))
  private val recency = Array.fill(cfg.numSets)(mutable.ListBuffer[Int]())
  private val fifoPointer = Array.fill(cfg.numSets)(0)
  var hits = 0L
  var misses = 0L
  var writeBacks = 0L

  private def touch(set: Int, way: Int): Unit = {
    recency(set) -= way
    recency(set).prepend(way) // frente = mais recente
  }

  def access(addr: Long, write: Boolean): Unit = {
    val lineNumber = addr / cfg.lineBytes
    val set = (lineNumber % cfg.numSets).toInt
    val tag = lineNumber / cfg.numSets
    val ways = lines(set)
    var way = ways.indexWhere(_.exists(_.tag == tag))
    if (way >= 0) {
      hits += 1
    } else {
      misses += 1
      val invalid = ways.indexWhere(_.isEmpty)
      way =
        if (invalid >= 0) invalid
        else
          cfg.policy match {
            case ReplacementPolicy.Lru  => recency(set).last
            case ReplacementPolicy.Fifo => fifoPointer(set)
            case other                  => sys.error(s"sem modelo para $other")
          }
      if (ways(way).exists(_.dirty)) writeBacks += 1
      ways(way) = Some(Line(tag, dirty = false))
      fifoPointer(set) = (fifoPointer(set) + 1) % cfg.ways
    }
    touch(set, way) // LRU: o acerto que atende o pedido; FIFO ignora
    if (write) ways(way).get.dirty = true
  }

  def dirtyLines: Int = lines.map(_.count(_.exists(_.dirty))).sum
}

/** Varredura de geometria e de politica de substituicao, um eixo por vez com os
  * demais na linha de base (8 KiB, 4 vias, linha de 16 B, LRU):
  *   - capacidade: 1, 2, 4, 8, 16 KiB
  *   - associatividade: 1, 2, 4, 8 vias
  *   - tamanho de linha: 8, 16, 32, 64 B
  *   - politica: LRU, FIFO, aleatoria
  *
  * Todas as configuracoes rodam o MESMO trace (`CacheSweepTrace`: enderecos de
  * byte fixos, semente fixa, independentes da geometria), confere o dado
  * devolvido contra um modelo de memoria, e, para LRU e FIFO, os contadores
  * (hits, misses, write-backs) contra um modelo de referencia. Os resultados de
  * cada configuracao vao para `target/cache-sweep.csv`, com a linha de base
  * presente em todos os eixos.
  */
class CacheGeometryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CacheConfig"

  it should "derivar a geometria e igualar CacheParams no padrao" in {
    val c = CacheConfig(capacityBytes = 16 * 1024, ways = 8, lineBytes = 32)
    assert(c.numSets == 64 && c.numLines == 512)
    assert(c.offsetBits == 5 && c.indexBits == 6 && c.tagBits == 21)
    assert(c.wordsPerLine == 8 && c.wayBits == 3)
    val d = CacheConfig.default
    assert(d.numSets == CacheParams.numSets && d.tagBits == CacheParams.tagBits)
    assert(d.wordsPerLine == CacheParams.wordsPerLine)
  }

  it should "rejeitar geometrias invalidas" in {
    intercept[IllegalArgumentException] { CacheConfig(ways = 3) }
    intercept[IllegalArgumentException] { CacheConfig(lineBytes = 4) }
    intercept[IllegalArgumentException] { CacheConfig(lineBytes = 24) }
    intercept[IllegalArgumentException] {
      CacheConfig(capacityBytes = 64, ways = 4, lineBytes = 16)
    }
  }

  behavior of "Cache parametrizada (varredura)"

  private val base = CacheSweep.base

  // Mesma memoria (32 KiB) para todas as configs: o trace e o mesmo e cobre
  // 24 KiB, entao nao ha motivo para a memoria variar com a geometria.
  private val depthWords = CacheSweepTrace.MemoryWords

  // Resultado de cada configuracao distinta. A linha de base roda uma vez e o
  // CSV a repete em todos os eixos em que ela e um ponto.
  private case class Stats(hits: Long, misses: Long, writeBacks: Long)
  private val stats = mutable.Map[CacheConfig, Stats]()

  private def access(
      dut: CacheUnderTest,
      addr: Long,
      write: Boolean,
      data: BigInt
  ): BigInt = {
    // Cada poke custa uma avaliacao do circuito no Treadle: so se escreve o que
    // muda (memSize e unsignedLoad sao fixados uma vez em `initCpu`).
    dut.io.cpu.request.poke(true.B)
    dut.io.cpu.write.poke(write.B)
    dut.io.cpu.address.poke(BigInt(addr).U(32.W))
    if (write) dut.io.cpu.writeData.poke(data.U(32.W))
    var cycles = 0
    var read: Option[BigInt] = None
    while (read.isEmpty) {
      require(cycles < 2000, "timeout: cache travada")
      if (dut.io.cpu.hit.peek().litToBoolean)
        read = Some(dut.io.cpu.readData.peek().litValue)
      dut.clock.step(1)
      cycles += 1
    }
    dut.io.cpu.request.poke(false.B)
    read.get
  }

  private def initCpu(dut: CacheUnderTest): Unit = {
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.io.cpu.address.poke(0.U)
    dut.io.cpu.writeData.poke(0.U)
    dut.io.cpu.memSize.poke(2.U) // palavra
    dut.io.cpu.unsignedLoad.poke(false.B)
  }

  for ((axis, cfg) <- CacheSweep.uniqueRuns) {
    it should s"funcionar com $axis: ${cfg.label}" in {
      test(new CacheUnderTest(cfg, depthWords)) { dut =>
        initCpu(dut)
        dut.io.flushDirty.poke(false.B)
        // O MESMO trace para toda configuracao (ver CacheSweepTrace).
        val trace = CacheSweepTrace.trace
        val accesses = trace.size
        val memory = mutable.Map[Long, BigInt]() // endereco -> valor
        val written = mutable.Set[Long]()
        val ref =
          if (cfg.policy == ReplacementPolicy.Random) None
          else Some(new RefCache(cfg))

        for (a <- trace) {
          if (a.write) {
            access(dut, a.addr, write = true, a.value)
            memory(a.addr) = a.value
            written += a.addr
          } else {
            val got = access(dut, a.addr, write = false, 0)
            assert(
              got == memory.getOrElse(a.addr, BigInt(0)),
              s"leitura de 0x${a.addr.toHexString} devolveu $got"
            )
          }
          ref.foreach(_.access(a.addr, a.write))
        }

        val hits = dut.io.perf.hits.peek().litValue.toLong
        val misses = dut.io.perf.misses.peek().litValue.toLong
        val writeBacks = dut.io.perf.writeBacks.peek().litValue.toLong
        assert(
          hits + misses == accesses,
          "hits + misses deve igualar os pedidos"
        )
        assert(misses >= 1 && hits >= 1, "o trace deve ter acertos e misses")
        ref.foreach { r =>
          assert(hits == r.hits, s"hits: DUT $hits, modelo ${r.hits}")
          assert(misses == r.misses, s"misses: DUT $misses, modelo ${r.misses}")
          assert(
            writeBacks == r.writeBacks,
            s"write-backs: DUT $writeBacks, modelo ${r.writeBacks}"
          )
        }

        // FENCE.I-like: o flush grava exatamente as linhas sujas e nada se perde
        dut.io.flushDirty.poke(true.B)
        var spin = 0
        while (!dut.io.flushDone.peek().litToBoolean) {
          require(spin < 200000, "flush travado")
          dut.clock.step(1)
          spin += 1
        }
        dut.io.flushDirty.poke(false.B)
        dut.clock.step(1)
        ref.foreach { r =>
          val flushed = dut.io.perf.flushWriteBacks.peek().litValue.toLong
          assert(
            flushed == r.dirtyLines,
            "flush deve gravar so as linhas sujas"
          )
        }
        for (addr <- written) {
          assert(
            access(dut, addr, write = false, 0) == memory(addr),
            s"dado em 0x${addr.toHexString} perdido apos o flush"
          )
        }

        stats(cfg) = Stats(hits, misses, writeBacks)
        val rate = hits.toDouble / accesses
        println(
          f"[sweep] ${cfg.label}%-22s taxa de acerto = ${rate * 100}%.1f%%"
        )
      }
    }
  }

  it should "gravar a varredura em target/cache-sweep.csv" in {
    val accesses = CacheSweepTrace.trace.size
    val file = new File("target/cache-sweep.csv")
    file.getParentFile.mkdirs()
    val out = new PrintWriter(file)
    try {
      out.println(
        "eixo,config,capacidade_bytes,vias,linha_bytes,politica,acessos,hits,misses,write_backs,taxa_acerto"
      )
      // Todo ponto de todo eixo, inclusive a linha de base em cada um deles.
      for ((axis, cfg) <- CacheSweep.axes) {
        val s = stats.getOrElse(
          cfg,
          fail(s"config ${cfg.label} nao foi simulada")
        )
        val rate = s.hits.toDouble / accesses
        out.println(
          f"$axis,${cfg.label},${cfg.capacityBytes},${cfg.ways},${cfg.lineBytes},${cfg.policy},$accesses,${s.hits},${s.misses},${s.writeBacks},$rate%.4f"
        )
      }
    } finally out.close()
    assert(file.exists())
  }

  it should "ter a linha de base em todos os eixos do CSV" in {
    for ((axis, points) <- CacheSweep.axes.groupBy(_._1)) {
      assert(
        points.exists(_._2 == base),
        s"o eixo $axis nao tem a linha de base"
      )
    }
  }
}
