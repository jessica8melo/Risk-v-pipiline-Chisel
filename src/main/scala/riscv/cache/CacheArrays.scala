package riscv.cache

import chisel3._
import chisel3.util._

/** Resultado da leitura combinacional de UMA via no conjunto selecionado. */
class CacheLineRead extends Bundle {
  val tag = UInt(CacheParams.tagBits.W)
  val valid = Bool()
  val dirty = Bool()
  val data = Vec(CacheParams.wordsPerLine, UInt(32.W))
}

/** Porta de escrita dos arrays (uma via por vez).
  *
  *   - `wordMask` seleciona quais palavras da linha sao gravadas: todas em um
  *     refill, apenas uma em um store hit.
  *   - `metaEn` grava tag/valid/dirty da via (refill, store hit -> dirty, etc.).
  *     Pode ser usado sem gravar dado (wordMask toda em false).
  */
class CacheWritePort extends Bundle {
  val en = Bool()
  val way = UInt(log2Ceil(CacheParams.ways).W)
  val index = UInt(CacheParams.indexBits.W)
  val wordMask = Vec(CacheParams.wordsPerLine, Bool())
  val data = Vec(CacheParams.wordsPerLine, UInt(32.W))
  val metaEn = Bool()
  val tag = UInt(CacheParams.tagBits.W)
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
  * Leitura: dado `readIndex`, as `ways` vias do conjunto saem no mesmo ciclo,
  * sem comparacao de tag (isso e a logica de hit/miss, feita fora daqui).
  * Se ha leitura e escrita no mesmo indice no mesmo ciclo, a leitura devolve
  * o valor ANTIGO (a escrita so vale no proximo ciclo).
  */
class CacheArrays extends Module {
  val io = IO(new Bundle {
    val readIndex = Input(UInt(CacheParams.indexBits.W))
    val read = Output(Vec(CacheParams.ways, new CacheLineRead))
    val write = Input(new CacheWritePort)
    val invalidateAll = Input(Bool())
  })

  private val sets = CacheParams.numSets
  private val tagMem = Seq.fill(CacheParams.ways)(Mem(sets, UInt(CacheParams.tagBits.W)))
  private val dataMem = Seq.fill(CacheParams.ways)(
    Mem(sets, Vec(CacheParams.wordsPerLine, UInt(32.W)))
  )
  private val validBits = RegInit(
    VecInit(Seq.fill(CacheParams.ways)(VecInit(Seq.fill(sets)(false.B))))
  )
  private val dirtyBits = RegInit(
    VecInit(Seq.fill(CacheParams.ways)(VecInit(Seq.fill(sets)(false.B))))
  )

  for (w <- 0 until CacheParams.ways) {
    // Leitura combinacional
    io.read(w).tag := tagMem(w).read(io.readIndex)
    io.read(w).data := dataMem(w).read(io.readIndex)
    io.read(w).valid := validBits(w)(io.readIndex)
    io.read(w).dirty := dirtyBits(w)(io.readIndex)

    // Escrita sincrona
    when(io.write.en && io.write.way === w.U) {
      dataMem(w).write(io.write.index, io.write.data, io.write.wordMask)
      when(io.write.metaEn) {
        tagMem(w).write(io.write.index, io.write.tag)
        validBits(w)(io.write.index) := io.write.valid
        dirtyBits(w)(io.write.index) := io.write.dirty
      }
    }
  }

  when(io.invalidateAll) {
    for (w <- 0 until CacheParams.ways; set <- 0 until sets) {
      validBits(w)(set) := false.B
      dirtyBits(w)(set) := false.B
    }
  }
}
