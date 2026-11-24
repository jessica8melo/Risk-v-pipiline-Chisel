package riscv.cache

import org.scalatest.flatspec.AnyFlatSpec

/** Guarda de regressao da varredura de cache: o trace e unico e fixo, e as
  * configuracoes que diferem em geometria ou politica NAO podem devolver
  * contadores identicos. Roda em Scala puro (sem simular hardware), com o mesmo
  * modelo de referencia (`RefCache`) que valida a Cache.
  */
class CacheSweepTraceTest extends AnyFlatSpec {
  behavior of "CacheSweepTrace"

  it should "ser determinista e bem formado" in {
    val t = CacheSweepTrace.trace
    assert(t == CacheSweepTrace.generate(), "mesma semente, mesmo trace")
    assert(t.size == CacheSweepTrace.Accesses)
    assert(t.forall(_.addr % 4 == 0), "so acessos alinhados a palavra")
    assert(
      t.forall(a => a.addr >= 0 && a.addr < CacheSweepTrace.FootprintBytes)
    )
    assert(
      CacheSweepTrace.FootprintBytes <= CacheSweepTrace.MemoryWords * 4,
      "a memoria principal cobre o footprint"
    )
    val writes = t.count(_.write)
    assert(writes > 0 && writes < t.size, "o trace mistura leituras e escritas")
  }

  it should "nao depender da geometria da cache" in {
    // O trace e um valor: nenhuma CacheConfig entra na sua geracao. Roda-lo
    // por duas geometrias diferentes parte da mesma sequencia de enderecos.
    val a = CacheSweepTrace.trace.map(_.addr)
    val b = CacheSweepTrace.generate().map(_.addr)
    assert(a == b)
  }

  behavior of "Varredura de cache"

  it should "ter a linha de base em todos os eixos" in {
    for ((axis, points) <- CacheSweep.axes.groupBy(_._1)) {
      assert(
        points.exists(_._2 == CacheSweep.base),
        s"o eixo $axis nao tem a linha de base"
      )
    }
  }

  it should "dar contadores distintos para configuracoes distintas" in {
    // Antes do trace fixo, 8 KiB/8 B e 16 KiB/16 B (mesmo numero de linhas e
    // de sets) davam exatamente os mesmos numeros, o mesmo para 32 B vs 4 KiB
    // e 64 B vs 2 KiB. Duas configs diferentes podem, em tese, empatar por
    // acaso, mas empatar em hits, misses e write-backs ao mesmo tempo, num
    // trace de 1500 acessos, indica trace derivado da geometria.
    val byCounters = CacheSweep.uniqueRuns
      .map(_._2)
      .filter(_.policy != ReplacementPolicy.Random) // sem modelo de referencia
      .groupBy { cfg =>
        val ref = new RefCache(cfg)
        CacheSweepTrace.trace.foreach(a => ref.access(a.addr, a.write))
        (ref.hits, ref.misses, ref.writeBacks)
      }
    for ((counters, cfgs) <- byCounters) {
      assert(
        cfgs.size == 1,
        s"configs diferentes com contadores identicos $counters: ${cfgs.map(_.label).mkString(", ")}"
      )
    }
  }
}
