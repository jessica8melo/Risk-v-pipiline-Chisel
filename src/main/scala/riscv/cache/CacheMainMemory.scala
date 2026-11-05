package riscv.cache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

/** Memoria principal simulada, compartilhada pela ICache e pela DCache.
  *
  * Armazena palavras de 32 bits e atende transferencias de uma linha inteira
  * pelo protocolo CacheMemIO. Se as duas caches pedirem a memoria no mesmo
  * ciclo, a DCache tem prioridade; o pedido da ICache permanece pendente ate
  * receber ready.
    * Pedidos fora da capacidade nao sao aceitos: ready fica baixo, readLine
    * retorna zero e nenhuma escrita e realizada. Pedidos desalinhados tambem
    * ficam sem ready e disparam assert em simulacao. Como CacheMemIO nao possui
    * canal de erro, um pedido fora da capacidade permanece aguardando.
  *
  * `initialData` e `programFile` usam palavras de 32 bits, na mesma ordem da
  * InstructionMemory. Assim, programa e dados ocupam o mesmo espaco de
  * enderecos. Para inicializacao por arquivo, cada linha do arquivo segue o
  * formato hex aceito por loadMemoryFromFileInline.
  */
class CacheMainMemory(
    depthWords: Int = 1024,
    initialData: Seq[Long] = Seq.empty,
    programFile: String = ""
) extends Module {
  require(depthWords >= CacheParams.wordsPerLine, "memoria deve conter ao menos uma linha")
  require(isPow2(depthWords), "depthWords deve ser potencia de 2")
  require(depthWords <= (1 << 30), "memoria nao pode exceder o espaco de enderecos de 32 bits")
  require(
    depthWords % CacheParams.wordsPerLine == 0,
    "depthWords deve conter um numero inteiro de linhas de cache"
  )
  require(initialData.length <= depthWords, "initialData excede a capacidade da memoria")
  require(
    initialData.isEmpty || programFile.isEmpty,
    "use initialData para testes Chisel ou programFile para simulacao Verilog, nao ambos"
  )

  val io = IO(new Bundle {
    val iMem = Flipped(new CacheMemIO)
    val dMem = Flipped(new CacheMemIO)
  })

  private val wordIndexBits = log2Ceil(depthWords)

  // Arbitra as duas portas sobre um unico armazenamento: a DCache tem prioridade.
  private val dGrant = io.dMem.request
  private val iGrant = io.iMem.request && !io.dMem.request
  private val selectedAddress = Mux(dGrant, io.dMem.address, io.iMem.address)
  private val selectedWrite = Mux(dGrant, io.dMem.write, io.iMem.write)
  private val requestGranted = dGrant || iGrant
  private val addressAligned = selectedAddress(CacheParams.offsetBits - 1, 0) === 0.U
  private val addressInRange =
    if (wordIndexBits + 2 < 32) selectedAddress(31, wordIndexBits + 2) === 0.U
    else true.B
  private val addressValid = addressAligned && addressInRange
  private val selectedWriteLine = Wire(Vec(CacheParams.wordsPerLine, UInt(32.W)))
  private val selectedReadLine = Wire(Vec(CacheParams.wordsPerLine, UInt(32.W)))

  for (word <- 0 until CacheParams.wordsPerLine) {
    selectedWriteLine(word) := Mux(
      dGrant,
      io.dMem.writeLine(word),
      io.iMem.writeLine(word)
    )
  }

  // Enderecos invalidos usam uma linha segura para que nem a leitura
  // combinacional nem a escrita indexem a memoria fora do intervalo.
  private val safeAddress = Mux(addressValid, selectedAddress, 0.U(32.W))
  private val baseWord = safeAddress(wordIndexBits + 1, 2)

  if (programFile.nonEmpty) {
    val memory = Mem(depthWords, UInt(32.W))
    loadMemoryFromFileInline(memory, programFile)

    for (word <- 0 until CacheParams.wordsPerLine) {
      val wordAddress = baseWord + word.U
      selectedReadLine(word) := memory(wordAddress)
      when(requestGranted && addressValid && selectedWrite) {
        memory(wordAddress) := selectedWriteLine(word)
      }
    }
  } else {
    val initialWords = initialData.padTo(depthWords, 0L).map(_.U(32.W))
    val memory = RegInit(VecInit(initialWords))

    for (word <- 0 until CacheParams.wordsPerLine) {
      val wordAddress = baseWord + word.U
      selectedReadLine(word) := memory(wordAddress)
      when(requestGranted && addressValid && selectedWrite) {
        memory(wordAddress) := selectedWriteLine(word)
      }
    }
  }

  io.dMem.ready := dGrant && addressValid
  io.iMem.ready := iGrant && addressValid
  for (word <- 0 until CacheParams.wordsPerLine) {
    io.dMem.readLine(word) := Mux(addressValid, selectedReadLine(word), 0.U)
    io.iMem.readLine(word) := Mux(addressValid, selectedReadLine(word), 0.U)
  }

  when(requestGranted && !addressAligned) {
    assert(false.B, "pedido a memoria principal deve estar alinhado ao inicio da linha")
  }
}