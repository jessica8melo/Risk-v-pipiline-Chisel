package riscv.cache

import chisel3._
import chisel3.util._

/** Politica de substituicao de linhas em um conjunto cheio. */
sealed trait ReplacementPolicy
object ReplacementPolicy {

  /** Menos recentemente usada: a via cujo ultimo acerto e o mais antigo. */
  case object Lru extends ReplacementPolicy

  /** Primeira a entrar, primeira a sair: ignora acertos, so conta
    * preenchimentos.
    */
  case object Fifo extends ReplacementPolicy

  /** Via sorteada por um LFSR de 16 bits (sequencia deterministica). */
  case object Random extends ReplacementPolicy
}

/** Geometria e politica de UMA cache L1 (a ICache e a DCache usam a mesma).
  *
  * @param capacityBytes
  *   capacidade de dados de cada cache (8 KiB por padrao)
  * @param ways
  *   associatividade (potencia de 2; 1 = mapeamento direto)
  * @param lineBytes
  *   tamanho da linha em bytes (potencia de 2, ao menos 8)
  * @param policy
  *   politica de substituicao
  *
  * Configuracao padrao (linha de base): 8 KiB, 4 vias, linha de 16 B, LRU,
  * write-back + write-allocate, bloqueante.
  *
  * NOTA DE PROJETO: o enunciado fala em "capacidade total de 16KiB" para o L1
  * ja dividido em ICache + DCache. Assumimos que os 16 KiB sao a SOMA das duas
  * (8 KiB para cada). Para outro valor, basta mudar `capacityBytes`.
  */
case class CacheConfig(
    capacityBytes: Int = 8 * 1024,
    ways: Int = 4,
    lineBytes: Int = 16,
    policy: ReplacementPolicy = ReplacementPolicy.Lru
) {
  val wordBytes: Int = 4
  val wordsPerLine: Int = lineBytes / wordBytes
  val numLines: Int = capacityBytes / lineBytes
  val numSets: Int = numLines / ways

  val offsetBits: Int = log2Ceil(lineBytes)
  val indexBits: Int = log2Ceil(numSets)
  val tagBits: Int = 32 - indexBits - offsetBits
  // Largura dos numeros de via (ao menos 1 bit, mesmo com 1 via).
  val wayBits: Int = log2Up(ways)

  require(isPow2(lineBytes) && lineBytes >= 8, "lineBytes: potencia de 2, >= 8")
  require(isPow2(ways), "ways precisa ser potencia de 2")
  require(
    capacityBytes % (lineBytes * ways) == 0 && numLines >= ways,
    "capacidade precisa conter ao menos um conjunto completo"
  )
  require(
    isPow2(numSets) && numSets >= 2,
    "numSets precisa ser potencia de 2 e >= 2 (indexacao)"
  )

  /** Nome curto para relatorios, ex.: `8KiB-4way-16B-Lru`. */
  def label: String =
    s"${capacityBytes / 1024.0}KiB-${ways}way-${lineBytes}B-${policy.toString}"
      .replace(".0KiB", "KiB")
}

object CacheConfig {
  val default: CacheConfig = CacheConfig()
}

/** Geometria padrao, para o codigo e os testes que nao variam a configuracao.
  * Equivale a `CacheConfig.default`.
  */
object CacheParams {
  private def d = CacheConfig.default
  val lineBytes: Int = d.lineBytes
  val ways: Int = d.ways
  val wordBytes: Int = d.wordBytes
  val wordsPerLine: Int = d.wordsPerLine
  val bytesPerCache: Int = d.capacityBytes
  val numLines: Int = d.numLines
  val numSets: Int = d.numSets
  val offsetBits: Int = d.offsetBits
  val indexBits: Int = d.indexBits
  val tagBits: Int = d.tagBits
}

/** Decomposicao do endereco de 32 bits em tag / index / offset, de acordo com a
  * geometria da cache.
  */
class CacheAddress(cfg: CacheConfig = CacheConfig.default) extends Bundle {
  val tag = UInt(cfg.tagBits.W)
  val index = UInt(cfg.indexBits.W)
  val offset = UInt(cfg.offsetBits.W)
}

object CacheAddress {
  def fromUInt(
      addr: UInt,
      cfg: CacheConfig = CacheConfig.default
  ): CacheAddress = {
    val decoded = Wire(new CacheAddress(cfg))
    decoded.offset := addr(cfg.offsetBits - 1, 0)
    decoded.index := addr(cfg.offsetBits + cfg.indexBits - 1, cfg.offsetBits)
    decoded.tag := addr(31, cfg.offsetBits + cfg.indexBits)
    decoded
  }
}
