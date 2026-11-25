package riscv.cache

import chisel3._
import chisel3.util._

/** Resultado da leitura combinacional de UMA via no conjunto selecionado. */
class CacheLineRead(cfg: CacheConfig = CacheConfig.default) extends Bundle {
  val tag = UInt(cfg.tagBits.W)
  val valid = Bool()
  val dirty = Bool()
  val data = Vec(cfg.wordsPerLine, UInt(32.W))
}

/** Porta de escrita dos arrays (uma via por vez).
  *
  *   - `wordMask` seleciona quais palavras da linha sao gravadas: todas em um
  *     refill, apenas uma em um store hit.
  *   - `metaEn` grava tag/valid/dirty da via (refill, store hit -> dirty,
  *     etc.). Pode ser usado sem gravar dado (wordMask toda em false).
  */
class CacheWritePort(cfg: CacheConfig = CacheConfig.default) extends Bundle {
  val en = Bool()
  val way = UInt(cfg.wayBits.W)
  val index = UInt(cfg.indexBits.W)
  val wordMask = Vec(cfg.wordsPerLine, Bool())
  val data = Vec(cfg.wordsPerLine, UInt(32.W))
  val metaEn = Bool()
  val tag = UInt(cfg.tagBits.W)
  val valid = Bool()
  val dirty = Bool()
}

/** Armazenamento das linhas: dado, tag, valid e dirty, por via e por conjunto.
  *
  *   - dado e tag: `Mem` (leitura combinacional, escrita sincrona), o que
  *     infere RAM distribuida em FPGA em vez de milhares de flip-flops;
  *   - valid e dirty: registradores com reset (todas as linhas comecam
  *     invalidas e limpas).
  *
  * `hasDirty = false` (ICache, somente leitura) NAO cria o armazenamento do bit
  * de sujeira: o registrador `dirtyBits` nem existe no circuito gerado,
  * `read(w).dirty` e a constante 0 e `write.dirty` e ignorado. Assim a ausencia
  * e literal em Chisel, e nao depende de a sintese eliminar um registrador que
  * nunca vale 1.
  *
  * Leitura: dado `readIndex`, as `ways` vias do conjunto saem no mesmo ciclo,
  * sem comparacao de tag (isso e a logica de hit/miss, feita fora daqui). Se ha
  * leitura e escrita no mesmo indice no mesmo ciclo, a leitura devolve o valor
  * ANTIGO (a escrita so vale no proximo ciclo).
  *
  * `cfg.blockRam = true`: tag e dado viram `SyncReadMem` (leitura SINCRONA, o
  * que a FPGA mapeia em M10K). Entao `read(w).tag` e `read(w).data` mostram o
  * conjunto de `readIndex` do CICLO ANTERIOR, e quem usa precisa manter o
  * indice estavel por um ciclo antes de consumir (o `Cache` faz isso: o ciclo
  * de `sIdle` apresenta o indice e `sCompareTag` consome; `sRefillWait` e a
  * espera do flush cobrem os demais casos). `valid` e `dirty` continuam
  * registradores de leitura combinacional (ha so 1 bit por linha), sempre do
  * indice ATUAL. Leitura e escrita no mesmo indice no mesmo ciclo dao valor
  * indefinido (nao se gera logica de bypass); o `Cache` nunca consome essa
  * leitura. Cada via guarda o dado em uma memoria de 32 bits por palavra (128 x
  * 32) e a tag em outra, que cabem em um M10K cada.
  */
class CacheArrays(
    cfg: CacheConfig = CacheConfig.default,
    hasDirty: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val readIndex = Input(UInt(cfg.indexBits.W))
    val read = Output(Vec(cfg.ways, new CacheLineRead(cfg)))
    val write = Input(new CacheWritePort(cfg))
    val invalidateAll = Input(Bool())
  })

  private val sets = cfg.numSets
  private val blockRam = cfg.blockRam
  // Armazenamento combinacional (padrao): um Mem por via.
  private val tagMem =
    if (blockRam) Seq.empty
    else Seq.fill(cfg.ways)(Mem(sets, UInt(cfg.tagBits.W)))
  private val dataMem =
    if (blockRam) Seq.empty
    else Seq.fill(cfg.ways)(Mem(sets, Vec(cfg.wordsPerLine, UInt(32.W))))
  // Armazenamento sincrono (blockRam): uma memoria de tag e uma de 32 bits por
  // palavra, para cada via.
  private val tagRam =
    if (blockRam) Seq.fill(cfg.ways)(SyncReadMem(sets, UInt(cfg.tagBits.W)))
    else Seq.empty
  private val wordRam =
    if (blockRam)
      Seq.fill(cfg.ways)(
        Seq.fill(cfg.wordsPerLine)(SyncReadMem(sets, UInt(32.W)))
      )
    else Seq.empty
  private val validBits = RegInit(
    VecInit(Seq.fill(cfg.ways)(VecInit(Seq.fill(sets)(false.B))))
  )
  // So existe quando a cache pode ser escrita (DCache).
  private val dirtyBits: Option[Vec[Vec[Bool]]] =
    if (hasDirty) {
      val bits = RegInit(
        VecInit(Seq.fill(cfg.ways)(VecInit(Seq.fill(sets)(false.B))))
      )
      bits.suggestName("dirtyBits") // nome fixo (os testes procuram por ele)
      Some(bits)
    } else None

  for (w <- 0 until cfg.ways) {
    if (blockRam) {
      // Leitura sincrona: o resultado chega no ciclo seguinte ao do indice.
      io.read(w).tag := tagRam(w).read(io.readIndex)
      for (i <- 0 until cfg.wordsPerLine)
        io.read(w).data(i) := wordRam(w)(i).read(io.readIndex)
    } else {
      // Leitura combinacional
      io.read(w).tag := tagMem(w).read(io.readIndex)
      io.read(w).data := dataMem(w).read(io.readIndex)
    }
    io.read(w).valid := validBits(w)(io.readIndex)
    // map/getOrElse em vez de `:= x match {...}`: `match` tem precedencia menor
    // que `:=` e sem parenteses isso e lido como `(a := x) match {...}`.
    io.read(w)
      .dirty := dirtyBits.map(bits => bits(w)(io.readIndex)).getOrElse(false.B)

    // Escrita sincrona
    when(io.write.en && io.write.way === w.U) {
      if (blockRam) {
        for (i <- 0 until cfg.wordsPerLine)
          when(io.write.wordMask(i)) {
            wordRam(w)(i).write(io.write.index, io.write.data(i))
          }
      } else {
        dataMem(w).write(io.write.index, io.write.data, io.write.wordMask)
      }
      when(io.write.metaEn) {
        if (blockRam) tagRam(w).write(io.write.index, io.write.tag)
        else tagMem(w).write(io.write.index, io.write.tag)
        validBits(w)(io.write.index) := io.write.valid
        dirtyBits.foreach(bits => bits(w)(io.write.index) := io.write.dirty)
      }
    }
  }

  when(io.invalidateAll) {
    for (w <- 0 until cfg.ways; set <- 0 until sets) {
      validBits(w)(set) := false.B
      dirtyBits.foreach(bits => bits(w)(set) := false.B)
    }
  }
}
