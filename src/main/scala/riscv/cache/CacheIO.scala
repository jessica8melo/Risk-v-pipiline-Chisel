package riscv.cache

import chisel3._
import chisel3.util._

/** Interface entre o pipeline (CPU) e uma cache L1 (ICache ou DCache).
  *
  * Direcoes definidas do ponto de vista da CPU: quem instancia a cache usa
  * `Flipped(new CpuCacheIO)`. Os campos de dado/tamanho reaproveitam a
  * mesma convencao de RV32I.MemorySize (memSize/unsignedLoad)
  * para nao duplicar semantica ja existente no projeto.
  */
class CpuCacheIO extends Bundle {
  // CPU -> Cache
  val request = Output(Bool()) // pedido valido neste ciclo
  val write = Output(Bool()) // false = load/fetch, true = store (sempre false na ICache)
  val address = Output(UInt(32.W)) // endereco de byte
  val writeData = Output(UInt(32.W))
  val memSize = Output(UInt(2.W)) // RV32I.MemorySize: BYTE/HALF/WORD
  val unsignedLoad = Output(Bool())

  // Cache -> CPU
  val readData = Input(UInt(32.W))
  val hit = Input(Bool()) // 1 quando o pedido deste ciclo foi atendido (dado em readData valido)
  val stall = Input(Bool()) // 1 enquanto a cache ainda resolve um miss; pipeline deve congelar
}

/** Interface entre uma cache L1 e o proximo nivel de memoria (a memoria
  * principal do projeto). Transfere uma linha inteira por vez: refill em
  * miss de leitura, write-back no despejo de uma linha suja.
  *
  * Como a cache e bloqueante e o design e single core, um simples par
  * request/ready (sem Decoupled/Valid-Ready do chisel3.util) e suficiente e
  * fica consistente com o estilo do resto do projeto.
  */
class CacheMemIO extends Bundle {
  // Cache -> Memoria
  val request = Output(Bool())
  val write = Output(Bool()) // false = le linha (refill), true = escreve linha (write-back)
  val address = Output(UInt(32.W)) // endereco alinhado ao inicio da linha
  val writeLine = Output(Vec(CacheParams.wordsPerLine, UInt(32.W)))

  // Memoria -> Cache
  val readLine = Input(Vec(CacheParams.wordsPerLine, UInt(32.W)))
  val ready = Input(Bool()) // 1 quando a operacao pedida (refill ou write-back) termina
}
