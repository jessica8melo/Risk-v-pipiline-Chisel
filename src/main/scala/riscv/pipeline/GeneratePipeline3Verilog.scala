package riscv.pipeline

import chisel3.stage.ChiselStage
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source

/** Gera o Verilog do `Pipeline3` com um programa em hexadecimal carregado na
  * memoria principal (`CacheMainMemory`, por `loadMemoryFromFileInline`).
  *
  * Uso:
  * {{{
  * sbt "runMain riscv.pipeline.GeneratePipeline3Verilog [programa.hex] [dirSaida] [memoryWords] [memoryLatency]"
  * }}}
  *
  * Padroes: `src/test/resources/diagnostics/rv32i-add.hex`, `generated`, 1024
  * palavras e latencia 0. O programa tem uma palavra de 32 bits em hexadecimal
  * por linha (comentarios com `#` sao ignorados). O gerador copia o programa
  * para `<dirSaida>/program_padded.hex` completando com zeros ate `memoryWords`
  * linhas: sem isso o `$readmemh` deixa o restante da memoria indefinido (X) na
  * simulacao e avisa "Not enough words in the file". O caminho gravado no
  * Verilog e absoluto, entao e preciso gerar de novo se o projeto mudar de
  * lugar.
  *
  * Saidas em `<dirSaida>`: `Pipeline3.v`, `Pipeline3.fir`,
  * `Pipeline3.anno.json` e `program_padded.hex`. O testbench fica em
  * `modelsim/tb_pipeline3.v`.
  */
object GeneratePipeline3Verilog extends App {
  val programArg = args.headOption.getOrElse(
    "src/test/resources/diagnostics/rv32i-add.hex"
  )
  val outputDir = args.drop(1).headOption.getOrElse("generated")
  val memoryWords = args.drop(2).headOption.map(_.toInt).getOrElse(1024)
  val memoryLatency = args.drop(3).headOption.map(_.toInt).getOrElse(0)

  val source = Source.fromFile(programArg, "UTF-8")
  val words =
    try
      source
        .getLines()
        .map(_.takeWhile(_ != '#').trim)
        .filter(_.nonEmpty)
        .toList
    finally source.close()

  words.zipWithIndex.foreach { case (word, index) =>
    require(
      word.matches("[0-9a-fA-F]{1,8}"),
      s"linha ${index + 1} de $programArg nao e uma palavra hex de ate 8 digitos: '$word'"
    )
  }
  require(
    words.length <= memoryWords,
    s"o programa tem ${words.length} palavras, mas a memoria so tem $memoryWords"
  )

  Files.createDirectories(Paths.get(outputDir))
  val paddedFile =
    Paths.get(outputDir, "program_padded.hex").toAbsolutePath.normalize
  val lines = words.map(word => "%08x".format(BigInt(word, 16).bigInteger)) ++
    Seq.fill(memoryWords - words.length)("00000000")
  Files.write(
    paddedFile,
    (lines.mkString("\n") + "\n").getBytes(StandardCharsets.UTF_8)
  )

  (new ChiselStage).emitVerilog(
    new Pipeline3(
      programFile = paddedFile.toString,
      memoryWords = memoryWords,
      memoryLatency = memoryLatency
    ),
    Array("--target-dir", outputDir)
  )

  println(
    s"Verilog em $outputDir/Pipeline3.v; programa: ${words.length} palavras " +
      s"(memoria de $memoryWords, latencia $memoryLatency) em $paddedFile"
  )
}
