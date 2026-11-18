package riscv.cache

import chisel3._
import chisel3.util._

/** Interface comum das politicas de substituicao.
  *
  *   - `index`: conjunto consultado; `victim` e a via a despejar nele quando
  *     todas as vias sao validas (o preenchimento de vias invalidas e feito
  *     pela Cache, antes da politica);
  *   - `touch*`: um acerto na via `touchWay` do conjunto `touchIndex`;
  *   - `fill*`: uma linha nova foi alocada na via `fillWay` do conjunto
  *     `fillIndex` (refill concluido);
  *   - `clear`: todas as linhas foram invalidadas (FENCE.I na ICache).
  */
class ReplacementIO(cfg: CacheConfig) extends Bundle {
  val index = Input(UInt(cfg.indexBits.W))
  val victim = Output(UInt(cfg.wayBits.W))
  val touch = Input(Bool())
  val touchIndex = Input(UInt(cfg.indexBits.W))
  val touchWay = Input(UInt(cfg.wayBits.W))
  val fill = Input(Bool())
  val fillIndex = Input(UInt(cfg.indexBits.W))
  val fillWay = Input(UInt(cfg.wayBits.W))
  val clear = Input(Bool())
}

abstract class CacheReplacement(cfg: CacheConfig) extends Module {
  val io = IO(new ReplacementIO(cfg))
}

object CacheReplacement {
  def apply(cfg: CacheConfig): CacheReplacement = cfg.policy match {
    case ReplacementPolicy.Lru    => Module(new CacheLru(cfg))
    case ReplacementPolicy.Fifo   => Module(new CacheFifo(cfg))
    case ReplacementPolicy.Random => Module(new CacheRandom(cfg))
  }
}

/** LRU por ranking: para cada conjunto, `rank(via)` vai de 0 (MRU) a ways - 1
  * (LRU), sempre uma permutacao. So acertos (`touch`) atualizam.
  */
class CacheLru(cfg: CacheConfig = CacheConfig.default)
    extends CacheReplacement(cfg) {
  private val rank = RegInit(
    VecInit(
      Seq.fill(cfg.numSets)(
        VecInit(Seq.tabulate(cfg.ways)(w => w.U(cfg.wayBits.W)))
      )
    )
  )

  private val current = rank(io.index)
  io.victim := PriorityEncoder(
    VecInit(current.map(_ === (cfg.ways - 1).U)).asUInt
  )

  when(io.touch) {
    val old = rank(io.touchIndex)
    val touchedRank = old(io.touchWay)
    for (j <- 0 until cfg.ways) {
      when(j.U === io.touchWay) {
        rank(io.touchIndex)(j) := 0.U
      }.elsewhen(old(j) < touchedRank) {
        rank(io.touchIndex)(j) := old(j) + 1.U
      }
    }
  }
}

/** FIFO: um ponteiro por conjunto aponta a via mais antiga e avanca a cada
  * linha alocada; acertos nao o alteram. Como as vias invalidas sao preenchidas
  * em ordem crescente, o ponteiro coincide com a ordem de chegada; `clear`
  * (conjunto vazio de novo) o zera.
  */
class CacheFifo(cfg: CacheConfig = CacheConfig.default)
    extends CacheReplacement(cfg) {
  private val pointer = RegInit(
    VecInit(Seq.fill(cfg.numSets)(0.U(cfg.wayBits.W)))
  )

  io.victim := pointer(io.index)

  when(io.clear) {
    pointer.foreach(_ := 0.U)
  }.elsewhen(io.fill) {
    val next = Mux(
      pointer(io.fillIndex) === (cfg.ways - 1).U,
      0.U,
      pointer(io.fillIndex) + 1.U
    )
    pointer(io.fillIndex) := next
  }
}

/** Aleatoria: LFSR de 16 bits (x^16 + x^14 + x^13 + x^11 + 1) sempre em marcha;
  * a vitima e formada pelos bits menos significativos. Determinista (mesma
  * semente), mas depende dos ciclos de simulacao.
  */
class CacheRandom(cfg: CacheConfig = CacheConfig.default)
    extends CacheReplacement(cfg) {
  private val lfsr = RegInit("hACE1".U(16.W))
  lfsr := Cat(lfsr(14, 0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10))

  io.victim :=
    (if (cfg.ways == 1) 0.U(cfg.wayBits.W)
     else lfsr(log2Ceil(cfg.ways) - 1, 0))
}
