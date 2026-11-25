package riscv.pipeline

import chisel3._
import chisel3.util.Cat
import riscv.cache.CacheConfig

/** Top-level para sintese em FPGA (DE1-SoC).
  *
  * O `Pipeline3` tem ~1000 bits de saida (contadores de 64 bits, debug), mais
  * do que os pinos de qualquer Cyclone V. Aqui as saidas sao dobradas em 1 bit
  * de paridade (XOR registrado em 2 estagios), para o Quartus nao remover a
  * logica sem uso e os numeros de area incluirem o projeto inteiro. So 10 LEDs
  * saem do chip: writebackData[7:0], `illegal` e a paridade.
  *
  * O XOR de ~1000 entradas custa cerca de 200 ALMs, que entram no relatorio.
  */
class Pipeline3Fpga(
    memoryWords: Int = 1024,
    programFile: String = "program.hex",
    memoryLatency: Int = 0,
    cacheConfig: CacheConfig = CacheConfig.default
) extends Module {
  val io = IO(new Bundle {
    val leds = Output(UInt(10.W))
  })

  val cpu = Module(
    new Pipeline3(
      memoryWords = memoryWords,
      programFile = programFile,
      memoryLatency = memoryLatency,
      cacheConfig = cacheConfig
    )
  )
  cpu.io.perfClear := false.B

  private val core = Cat(
    cpu.io.pc,
    cpu.io.instr,
    cpu.io.aluResult,
    cpu.io.writebackData,
    cpu.io.writebackRd,
    cpu.io.writebackEnable,
    cpu.io.illegal,
    cpu.io.stalled
  )
  private val perf = Cat(cpu.io.iPerf.asUInt, cpu.io.dPerf.asUInt)
  private val debug = Cat(
    cpu.io.debug_clint_trap_assert,
    cpu.io.debug_clint_trap_to_smode,
    cpu.io.debug_clint_delegate_to_smode,
    cpu.io.debug_clint_exception_code,
    cpu.io.debug_clint_is_interrupt,
    cpu.io.debug_clint_trap_address,
    cpu.io.debug_csrReadData,
    cpu.io.debug_csrForwarding,
    cpu.io.debug_csr_address_id,
    cpu.io.debug_csr_address_ex
  )

  private val parityCore = RegNext(core.xorR)
  private val parityPerf = RegNext(perf.xorR)
  private val parityDebug = RegNext(debug.xorR)
  private val parity = RegNext(parityCore ^ parityPerf ^ parityDebug)

  io.leds := Cat(parity, cpu.io.illegal, cpu.io.writebackData(7, 0))
}
