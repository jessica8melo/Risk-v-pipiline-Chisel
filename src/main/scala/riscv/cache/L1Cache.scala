package riscv.cache

import chisel3._
import chisel3.util._

/** L1 dividido em ICache + DCache, cada uma seguindo CacheParams.
  *
  * As portas iMem/dMem podem ser conectadas a uma unica CacheMainMemory, que
  * arbitra pedidos simultaneos e compartilha o armazenamento entre instrucoes e
  * dados. O pipeline ainda precisa ser conectado separadamente a iCache/dCache.
  */
class L1Cache(cfg: CacheConfig = CacheConfig.default) extends Module {
  val io = IO(new Bundle {
    // Pipeline -> Cache (ligar no estagio de fetch e no estagio de memoria)
    val iCache = Flipped(new CpuCacheIO)
    val dCache = Flipped(new CpuCacheIO)

    // Cache -> Memoria principal
    val iMem = new CacheMemIO(cfg)
    val dMem = new CacheMemIO(cfg)

    // FENCE.I: drena linhas sujas da DCache e invalida todas as linhas da ICache.
    val fenceI = Input(Bool())
    val fenceIDone = Output(Bool())
    // 1 (valido junto com fenceIDone) se a memoria recusou algum write-back do
    // flush da DCache: o FENCE.I terminou mas ha dados sujos nao gravados.
    val fenceIFault = Output(Bool())

    // Contadores de desempenho de cada cache (a ligacao aos CSRs fica fora).
    val iPerf = Output(new CachePerf)
    val dPerf = Output(new CachePerf)
    val perfClear = Input(Bool()) // zera os contadores das duas caches
  })

  val instructionCache = Module(new Cache(isInstructionCache = true, cfg = cfg))
  val dataCache = Module(new Cache(isInstructionCache = false, cfg = cfg))

  val fenceIdle :: fenceFlushData :: fenceInvalidateInstruction :: fenceComplete :: Nil =
    Enum(4)
  val fenceState = RegInit(fenceIdle)
  val fenceActive = fenceState =/= fenceIdle

  instructionCache.io.flushDirty := false.B
  instructionCache.io.invalidateAll := fenceState === fenceInvalidateInstruction
  dataCache.io.flushDirty := fenceState === fenceFlushData
  dataCache.io.invalidateAll := false.B
  io.fenceIDone := fenceState === fenceComplete
  io.fenceIFault := dataCache.io.flushFault
  io.iPerf := instructionCache.io.perf
  io.dPerf := dataCache.io.perf
  instructionCache.io.perfClear := io.perfClear
  dataCache.io.perfClear := io.perfClear

  switch(fenceState) {
    is(fenceIdle) {
      when(io.fenceI) { fenceState := fenceFlushData }
    }
    is(fenceFlushData) {
      when(dataCache.io.flushDone) { fenceState := fenceInvalidateInstruction }
    }
    is(fenceInvalidateInstruction) {
      fenceState := fenceComplete
    }
    is(fenceComplete) {
      when(!io.fenceI) { fenceState := fenceIdle }
    }
  }

  // CPU <-> ICache
  instructionCache.io.cpu.request := io.iCache.request && !fenceActive
  instructionCache.io.cpu.write := io.iCache.write
  instructionCache.io.cpu.address := io.iCache.address
  instructionCache.io.cpu.writeData := io.iCache.writeData
  instructionCache.io.cpu.memSize := io.iCache.memSize
  instructionCache.io.cpu.unsignedLoad := io.iCache.unsignedLoad
  io.iCache.readData := instructionCache.io.cpu.readData
  io.iCache.hit := instructionCache.io.cpu.hit
  io.iCache.stall := instructionCache.io.cpu.stall
  io.iCache.fault := instructionCache.io.cpu.fault

  // ICache <-> Memoria
  io.iMem.request := instructionCache.io.mem.request
  io.iMem.write := instructionCache.io.mem.write
  io.iMem.address := instructionCache.io.mem.address
  io.iMem.writeLine := instructionCache.io.mem.writeLine
  instructionCache.io.mem.readLine := io.iMem.readLine
  instructionCache.io.mem.ready := io.iMem.ready
  instructionCache.io.mem.fault := io.iMem.fault

  // CPU <-> DCache
  dataCache.io.cpu.request := io.dCache.request && !fenceActive
  dataCache.io.cpu.write := io.dCache.write
  dataCache.io.cpu.address := io.dCache.address
  dataCache.io.cpu.writeData := io.dCache.writeData
  dataCache.io.cpu.memSize := io.dCache.memSize
  dataCache.io.cpu.unsignedLoad := io.dCache.unsignedLoad
  io.dCache.readData := dataCache.io.cpu.readData
  io.dCache.hit := dataCache.io.cpu.hit
  io.dCache.stall := dataCache.io.cpu.stall
  io.dCache.fault := dataCache.io.cpu.fault

  // DCache <-> Memoria
  io.dMem.request := dataCache.io.mem.request
  io.dMem.write := dataCache.io.mem.write
  io.dMem.address := dataCache.io.mem.address
  io.dMem.writeLine := dataCache.io.mem.writeLine
  dataCache.io.mem.readLine := io.dMem.readLine
  dataCache.io.mem.ready := io.dMem.ready
  dataCache.io.mem.fault := io.dMem.fault
}
