package riscv.pipeline

import chisel3.stage.ChiselStage
import java.io.File
import riscv.cache.{CacheConfig, HexLines}

/** Gera o Verilog do `Pipeline3Fpga` em `quartus/Pipeline3Fpga.v`.
  *
  * Uso (todos os argumentos sao opcionais, na ordem):
  * {{{
  * sbt "runMain riscv.pipeline.GeneratePipeline3Verilog program.hex 256 0 1024"
  * sbt "runMain riscv.pipeline.GeneratePipeline3Verilog program.hex 256 1 8192 true"
  * }}}
  * argumentos:
  *   1. arquivo hex (uma palavra de 32 bits por linha);
  *   2. palavras da memoria principal (potencia de 2);
  *   3. latencia da memoria em ciclos (padrao 0, ou 1 com blockRam);
  *   4. capacidade de CADA cache em bytes (padrao 8192);
  *   5. `true` para usar blocos de RAM (`CacheConfig.blockRam`): caches e
  *      memoria com leitura sincrona, que cabem nos M10K da FPGA. Exige
  *      latencia >= 1, e custa 1 ciclo por miss e 2 por conjunto no FENCE.I.
  *
  * O caminho do hex vai para um `$readmemh` dentro do Verilog, entao use so o
  * nome do arquivo e deixe-o na pasta do projeto Quartus. Com blockRam a
  * memoria carrega UMA LINHA DE CACHE por linha: o gerador le
  * `quartus/<arquivo>` e grava `quartus/<nome>_lines.hex` (palavra 0 nos bits
  * menos significativos), que e o arquivo usado pelo Verilog gerado.
  */
object GeneratePipeline3Verilog extends App {
  private val blockRam =
    args.lift(4).exists(a => a.equalsIgnoreCase("true") || a == "1")
  private val programFile = args.lift(0).getOrElse("program.hex")
  private val memoryWords = args.lift(1).map(_.toInt).getOrElse(1024)
  private val memoryLatency =
    args.lift(2).map(_.toInt).getOrElse(if (blockRam) 1 else 0)
  private val capacityBytes = args
    .lift(3)
    .map(_.toInt)
    .getOrElse(CacheConfig.default.capacityBytes)

  private val cfg = CacheConfig.default.copy(
    capacityBytes = capacityBytes,
    blockRam = blockRam
  )

  private val targetDir = "quartus"
  private val memoryFile =
    if (!blockRam) programFile
    else {
      val input = new File(targetDir, programFile)
      require(
        input.exists(),
        s"blockRam: hex nao encontrado em ${input.getPath} (1 palavra por linha)"
      )
      val output = programFile.stripSuffix(".hex") + "_lines.hex"
      val words =
        HexLines.convertFile(
          input,
          new File(targetDir, output),
          cfg.wordsPerLine
        )
      require(
        words <= memoryWords,
        s"programa tem $words palavras > $memoryWords"
      )
      println(
        s"[blockRam] $programFile -> $output ($words palavras, ${cfg.wordsPerLine} por linha)"
      )
      output
    }

  (new ChiselStage).emitVerilog(
    new Pipeline3Fpga(
      memoryWords = memoryWords,
      programFile = memoryFile,
      memoryLatency = memoryLatency,
      cacheConfig = cfg
    ),
    Array("--target-dir", targetDir)
  )
}
