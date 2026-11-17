package riscv.cache

import chisel3._
import chisel3.util._

/** Parametros geometricos do L1 (compartilhados por ICache e DCache).
  *
  * Configuracao alvo:
  *   - L1 dividido em ICache + DCache
  *   - associatividade em conjunto 4-way
  *   - linha de 16 bytes (4 palavras de 32 bits)
  *   - substituicao LRU
  *   - escrita write-back + write-allocate
  *   - cache bloqueante, single core
  *
  * NOTA DE PROJETO: o enunciado fala em "capacidade total de 16KiB" para o
  * L1 ja dividido em ICache + DCache. Aqui assumimos que os 16KiB sao a
  * SOMA das duas caches (8KiB para a ICache, 8KiB para a DCache). Se a
  * intencao for 16KiB *por cache* (32KiB no total), basta trocar
  * `bytesPerCache` para apontar direto para 16 * 1024.
  */
object CacheParams {
  val lineBytes: Int = 16
  val ways: Int = 4
  val wordBytes: Int = 4
  val wordsPerLine: Int = lineBytes / wordBytes // 4 palavras por linha

  private val totalBytesBothCaches: Int = 16 * 1024
  val bytesPerCache: Int = totalBytesBothCaches / 2 // 8 KiB para cada cache

  val numLines: Int = bytesPerCache / lineBytes
  val numSets: Int = numLines / ways

  val offsetBits: Int = log2Ceil(lineBytes)
  val indexBits: Int = log2Ceil(numSets)
  val tagBits: Int = 32 - indexBits - offsetBits

  require(isPow2(ways), "ways precisa ser potência de 2 (necessário p/ LRU e indexação)")
  require(isPow2(numSets), "numSets precisa ser potência de 2 (necessario p/ indexação)")
}

/** Decomposicao do endereco de 32 bits em tag / index / offset, de acordo
  * com a geometria de CacheParams. Uso interno da cache, mas fica aqui
  * porque decorre diretamente do sinal de endereco da interface CPU<->Cache.
  */
class CacheAddress extends Bundle {
  val tag = UInt(CacheParams.tagBits.W)
  val index = UInt(CacheParams.indexBits.W)
  val offset = UInt(CacheParams.offsetBits.W)
}

object CacheAddress {
  def fromUInt(addr: UInt): CacheAddress = {
    val decoded = Wire(new CacheAddress)
    decoded.offset := addr(CacheParams.offsetBits - 1, 0)
    decoded.index := addr(
      CacheParams.offsetBits + CacheParams.indexBits - 1,
      CacheParams.offsetBits
    )
    decoded.tag := addr(31, CacheParams.offsetBits + CacheParams.indexBits)
    decoded
  }
}
