package riscv.pipeline

import chisel3.stage.ChiselStage
import riscv.cache.CacheConfig

/** Gera o Verilog do `Pipeline3Fpga` em `quartus/Pipeline3Fpga.v`.
  *
  * Uso (todos os argumentos sao opcionais, na ordem):
  * {{{
  * sbt "runMain riscv.pipeline.GeneratePipeline3Verilog program.hex 256 0 1024"
  * }}}
  * argumentos:
  *   1. arquivo hex (uma palavra de 32 bits por linha);
  *   2. palavras da memoria principal (potencia de 2);
  *   3. latencia da memoria em ciclos;
  *   4. capacidade de CADA cache em bytes (padrao 8192).
  *
  * O caminho do hex vai para um `$readmemh` dentro do Verilog, entao use so o
  * nome do arquivo e deixe-o na pasta do projeto Quartus.
  */
object GeneratePipeline3Verilog extends App {
  private val programFile = args.lift(0).getOrElse("program.hex")
  private val memoryWords = args.lift(1).map(_.toInt).getOrElse(1024)
  private val memoryLatency = args.lift(2).map(_.toInt).getOrElse(0)
  private val capacityBytes = args
    .lift(3)
    .map(_.toInt)
    .getOrElse(CacheConfig.default.capacityBytes)

  (new ChiselStage).emitVerilog(
    new Pipeline3Fpga(
      memoryWords = memoryWords,
      programFile = programFile,
      memoryLatency = memoryLatency,
      cacheConfig = CacheConfig.default.copy(capacityBytes = capacityBytes)
    ),
    Array("--target-dir", "quartus")
  )
}
