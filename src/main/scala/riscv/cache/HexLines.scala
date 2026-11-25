package riscv.cache

import java.io.File
import java.nio.file.{Files, StandardOpenOption}
import scala.io.Source

/** Reagrupa um arquivo hex de 1 palavra de 32 bits por linha em 1 LINHA DE
  * CACHE por linha, o formato que a `CacheMainMemory` com `blockRam = true`
  * carrega por `$readmemh` (`SyncReadMem` com a linha inteira como um unico
  * UInt).
  *
  * A palavra de menor endereco vai nos bits MENOS significativos, isto e, na
  * extremidade direita da linha de texto: para 4 palavras `w0 w1 w2 w3` a linha
  * e `w3 w2 w1 w0`. A ultima linha e completada com zeros.
  */
object HexLines {

  /** Palavras de 32 bits lidas de um hex (uma por linha). Ignora linhas vazias
    * e comentarios (`//` e `#`).
    */
  def readWords(lines: Seq[String]): Seq[Long] =
    lines
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("//") || l.startsWith("#"))
      .map { l =>
        require(
          l.matches("[0-9a-fA-F]{1,8}"),
          s"linha de hex invalida (esperada 1 palavra de ate 8 digitos): '$l'"
        )
        java.lang.Long.parseLong(l, 16)
      }

  /** Linhas de cache, em hexadecimal, de `wordsPerLine` palavras cada. */
  def group(words: Seq[Long], wordsPerLine: Int): Seq[String] = {
    require(wordsPerLine >= 1, "wordsPerLine deve ser >= 1")
    words
      .padTo(
        ((words.size + wordsPerLine - 1) / wordsPerLine) * wordsPerLine,
        0L
      )
      .grouped(wordsPerLine)
      .map(line => line.reverse.map(w => f"${w & 0xffffffffL}%08x").mkString)
      .toSeq
  }

  /** Le `input` (1 palavra por linha), grava `output` (1 linha de cache por
    * linha) e devolve o numero de palavras do programa.
    */
  def convertFile(input: File, output: File, wordsPerLine: Int): Int = {
    val src = Source.fromFile(input, "UTF-8")
    val words =
      try readWords(src.getLines().toList)
      finally src.close()
    Files.write(
      output.toPath,
      (group(words, wordsPerLine).mkString("\n") + "\n").getBytes("UTF-8"),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    words.size
  }
}
