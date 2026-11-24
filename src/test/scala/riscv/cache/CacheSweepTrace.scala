package riscv.cache

import scala.collection.mutable

/** Um acesso do trace: endereco em BYTES (alinhado a palavra), tipo e valor. */
case class TraceAccess(addr: Long, write: Boolean, value: BigInt)

/** Trace UNICO e FIXO usado por todas as configuracoes da varredura.
  *
  * Regra de ouro: o trace NAO depende da geometria da cache. Ele e gerado uma
  * vez, com semente fixa, em enderecos de byte absolutos; cada configuracao so
  * decide como esses enderecos caem em sets/tags. Se o trace fosse derivado de
  * `numLines`, `lineBytes` etc., duas configuracoes com o mesmo numero de
  * linhas e de sets (8 KiB/8 B e 16 KiB/16 B, por exemplo) receberiam a mesma
  * sequencia de linhas e produziriam contadores identicos, e a comparacao entre
  * eixos perderia o sentido.
  *
  * Formato: rajadas de 1 a 4 palavras consecutivas (localidade espacial, para o
  * tamanho de linha importar) sobre tres camadas de localidade temporal
  * (quente, morna e fria), com 25% de escritas:
  *   - 60% das rajadas na regiao quente (`HotBytes`)
  *   - 25% na regiao morna (`WarmBytes`)
  *   - 15% no footprint inteiro (`FootprintBytes`)
  */
object CacheSweepTrace {
  val Seed = 42
  val Accesses = 1500
  val HotBytes: Int = 4 * 1024
  val WarmBytes: Int = 12 * 1024
  val FootprintBytes: Int = 24 * 1024 // 1,5x a maior capacidade da varredura

  /** Palavras da memoria principal (potencia de 2) que cobrem o footprint. */
  val MemoryWords: Int = {
    val footprintWords = FootprintBytes / 4
    Integer.highestOneBit(footprintWords - 1) << 1
  }

  /** Gera o trace. Determinista: mesma semente, mesma sequencia. */
  def generate(): Seq[TraceAccess] = {
    val rng = new scala.util.Random(Seed)
    val footprintWords = FootprintBytes / 4
    val out = mutable.ArrayBuffer[TraceAccess]()
    while (out.size < Accesses) {
      val pick = rng.nextInt(100)
      val regionBytes =
        if (pick < 60) HotBytes
        else if (pick < 85) WarmBytes
        else FootprintBytes
      val baseWord = rng.nextInt(regionBytes / 4)
      val burst = 1 + rng.nextInt(4)
      var k = 0
      while (k < burst && out.size < Accesses) {
        val write = rng.nextInt(4) == 0
        val value =
          if (write) BigInt(rng.nextInt()) & BigInt(0xffffffffL)
          else BigInt(0)
        out += TraceAccess(((baseWord + k) % footprintWords) * 4L, write, value)
        k += 1
      }
    }
    out.toSeq
  }

  /** O trace compartilhado, gerado uma unica vez. */
  lazy val trace: Seq[TraceAccess] = generate()
}

/** As configuracoes da varredura, um eixo por vez com os demais na linha de
  * base (8 KiB, 4 vias, linha de 16 B, LRU).
  */
object CacheSweep {
  val base: CacheConfig = CacheConfig()

  /** Todos os pontos de cada eixo, inclusive a linha de base em CADA eixo. */
  val axes: Seq[(String, CacheConfig)] =
    Seq(1, 2, 4, 8, 16).map(k =>
      "capacidade" -> base.copy(capacityBytes = k * 1024)
    ) ++
      Seq(1, 2, 4, 8).map(w => "associatividade" -> base.copy(ways = w)) ++
      Seq(8, 16, 32, 64).map(l => "linha" -> base.copy(lineBytes = l)) ++
      Seq(
        ReplacementPolicy.Lru,
        ReplacementPolicy.Fifo,
        ReplacementPolicy.Random
      ).map(p => "politica" -> base.copy(policy = p))

  /** Configuracoes distintas, cada uma com o primeiro eixo em que aparece. A
    * linha de base participa de varios eixos mas so e simulada uma vez (mesma
    * config + mesmo trace = mesmo resultado); o CSV a repete em cada eixo.
    */
  val uniqueRuns: Seq[(String, CacheConfig)] = {
    val seen = mutable.Set[CacheConfig]()
    axes.filter { case (_, c) => seen.add(c) }
  }
}
