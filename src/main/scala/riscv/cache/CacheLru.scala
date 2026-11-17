package riscv.cache

import chisel3._
import chisel3.util._

/** LRU verdadeiro por conjunto, para associatividade `ways` (potencia de 2).
  *
  * Guarda, para cada conjunto, o "rank" de cada via: 0 = mais recentemente
  * usada (MRU) ... ways-1 = menos recentemente usada (LRU). Os ranks de um
  * conjunto formam sempre uma permutacao de 0..ways-1 (reset: via i tem
  * rank i).
  *
  * Toque em uma via w: w passa a rank 0 e toda via com rank menor que o
  * antigo rank de w sobe uma posicao; as mais antigas nao mudam.
  *
  * Leitura combinacional de `lruWay` (via de rank ways-1 do conjunto `index`),
  * escrita sincrona pela porta `touch*`.
  *
  * Custo: numSets * ways * log2(ways) flip-flops (1024 para 128x4x2).
  * Alternativa mais barata, se area importar: pseudo-LRU em arvore
  * (ways-1 bits por conjunto), ao custo de nao ser LRU exato.
  */
class CacheLru extends Module {
  private val wayBits = log2Ceil(CacheParams.ways)

  val io = IO(new Bundle {
    val index = Input(UInt(CacheParams.indexBits.W))
    val lruWay = Output(UInt(wayBits.W))

    val touch = Input(Bool())
    val touchIndex = Input(UInt(CacheParams.indexBits.W))
    val touchWay = Input(UInt(wayBits.W))
  })

  private val rank = RegInit(
    VecInit(
      Seq.fill(CacheParams.numSets)(
        VecInit(Seq.tabulate(CacheParams.ways)(w => w.U(wayBits.W)))
      )
    )
  )

  // Vitima: a via de maior rank (exatamente uma, pois e permutacao).
  private val current = rank(io.index)
  io.lruWay := PriorityEncoder(
    VecInit(current.map(_ === (CacheParams.ways - 1).U)).asUInt
  )

  when(io.touch) {
    val old = rank(io.touchIndex)
    val touchedRank = old(io.touchWay)
    for (j <- 0 until CacheParams.ways) {
      when(j.U === io.touchWay) {
        rank(io.touchIndex)(j) := 0.U
      }.elsewhen(old(j) < touchedRank) {
        rank(io.touchIndex)(j) := old(j) + 1.U
      }
    }
  }
}
