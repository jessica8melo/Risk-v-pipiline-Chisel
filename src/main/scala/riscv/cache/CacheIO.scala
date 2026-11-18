package riscv.cache

import chisel3._
import chisel3.util._

/** Interface entre o pipeline (CPU) e uma cache L1 (ICache ou DCache).
  *
  * Direcoes definidas do ponto de vista da CPU: quem instancia a cache usa
  * `Flipped(new CpuCacheIO)`. Os campos de dado/tamanho reaproveitam a mesma
  * convencao de RV32I.MemorySize (memSize/unsignedLoad) para nao duplicar
  * semantica ja existente no projeto.
  */
class CpuCacheIO extends Bundle {
  // CPU -> Cache
  val request = Output(Bool()) // pedido valido neste ciclo
  val write = Output(
    Bool()
  ) // false = load/fetch, true = store (sempre false na ICache)
  val address = Output(UInt(32.W)) // endereco de byte
  val writeData = Output(UInt(32.W))
  val memSize = Output(UInt(2.W)) // RV32I.MemorySize: BYTE/HALF/WORD
  val unsignedLoad = Output(Bool())

  // Cache -> CPU
  val readData = Input(UInt(32.W))
  val hit = Input(
    Bool()
  ) // 1 quando o pedido deste ciclo foi atendido (dado em readData valido)
  val stall = Input(
    Bool()
  ) // 1 enquanto a cache ainda resolve um miss; pipeline deve congelar
  val fault = Input(
    Bool()
  ) // 1 (nivel) quando a memoria recusou o refill/write-back deste pedido; hit = 0 e
  // stall = 0. Fica em 1 ate a CPU derrubar `request` (ao tomar o trap).
  // ICache -> instruction access fault (mcause 1); DCache -> load (5) ou
  // store/AMO (7) access fault, conforme `write`; mtval = `address`.
}

/** Interface entre uma cache L1 e o proximo nivel de memoria (a memoria
  * principal do projeto). Transfere uma linha inteira por vez: refill em miss
  * de leitura, write-back no despejo de uma linha suja.
  *
  * Como a cache e bloqueante e o design e single core, um simples par
  * request/ready (sem Decoupled/Valid-Ready do chisel3.util) e suficiente e
  * fica consistente com o estilo do resto do projeto.
  */
class CacheMemIO(cfg: CacheConfig = CacheConfig.default) extends Bundle {
  // Cache -> Memoria
  val request = Output(Bool())
  val write = Output(
    Bool()
  ) // false = le linha (refill), true = escreve linha (write-back)
  val address = Output(UInt(32.W)) // endereco alinhado ao inicio da linha
  val writeLine = Output(Vec(cfg.wordsPerLine, UInt(32.W)))

  // Memoria -> Cache
  val readLine = Input(Vec(cfg.wordsPerLine, UInt(32.W)))
  val ready = Input(
    Bool()
  ) // 1 quando a operacao pedida (refill ou write-back) termina
  val fault = Input(
    Bool()
  ) // 1 quando a memoria recusa a operacao (ex.: endereco fora da memoria) no lugar
  // de ready: o pedido termina com erro. Nunca sobe junto com ready.
}

/** Contadores de desempenho de uma cache (64 bits, so crescem ate `clear`).
  *
  * Uma "consulta" e um pedido da CPU avaliado em CompareTag:
  *   - `hits`: pedidos atendidos na primeira consulta;
  *   - `misses`: consultas que falharam (um miss seguido de refill conta 1; o
  *     hit que serve o pedido depois do refill NAO entra em `hits`, entao hits
  *     + misses = pedidos atendidos, salvo refill refeito apos invalidacao ou
  *     fault);
  *   - `writeBacks`: linhas sujas gravadas por despejo (miss com vitima suja);
  *   - `flushWriteBacks`: linhas sujas gravadas pelo flush (FENCE.I);
  *   - `faults`: operacoes recusadas pela memoria (CacheMemIO.fault);
  *   - `missCycles`: ciclos gastos tratando miss (deteccao + write-back +
  *     refill), a penalidade de miss acumulada.
  */
class CachePerf extends Bundle {
  val hits = UInt(64.W)
  val misses = UInt(64.W)
  val writeBacks = UInt(64.W)
  val flushWriteBacks = UInt(64.W)
  val faults = UInt(64.W)
  val missCycles = UInt(64.W)
}
