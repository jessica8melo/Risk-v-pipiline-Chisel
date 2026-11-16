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
  *
  * `latency` e o numero de ciclos entre o primeiro ciclo em que um pedido e
  * visto e o ciclo em que ready sobe (a leitura/escrita da linha ocorre nesse
  * ciclo). Com latency = 0 (padrao) a memoria responde combinacionalmente no
  * mesmo ciclo, como sempre fez. Com latency >= 1 a memoria atende uma
  * transacao por vez: a porta escolhida no primeiro ciclo (DCache em caso de
  * empate) e dona do barramento ate completar, e a outra espera. O pedido da
  * porta dona precisa ficar estavel (como a Cache ja faz); se ele cair antes
  * de ready, a transacao e abortada sem escrever nada.
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
    programFile: String = "",
    latency: Int = 0
) extends Module {
  require(latency >= 0, "latency nao pode ser negativa")
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

  // Estado da transacao em andamento (usado so quando latency > 0).
  private val busy = RegInit(false.B)
  private val ownerIsD = RegInit(false.B)
  private val waited = RegInit(0.U(log2Ceil(latency + 1).max(1).W))

  // Arbitra as duas portas sobre um unico armazenamento: a DCache tem prioridade.
  // Com latency > 0 a porta dona da transacao em curso nao muda ate completar.
  private val dSel =
    if (latency == 0) io.dMem.request else Mux(busy, ownerIsD, io.dMem.request)
  private val selectedRequest = Mux(dSel, io.dMem.request, io.iMem.request)
  private val selectedAddress = Mux(dSel, io.dMem.address, io.iMem.address)
  private val selectedWrite = Mux(dSel, io.dMem.write, io.iMem.write)
  private val addressAligned = selectedAddress(CacheParams.offsetBits - 1, 0) === 0.U
  private val addressInRange =
    if (wordIndexBits + 2 < 32) selectedAddress(31, wordIndexBits + 2) === 0.U
    else true.B
  private val addressValid = addressAligned && addressInRange
  // Ciclo em que a transacao termina: ready sobe e a linha e lida/escrita.
  private val complete =
    if (latency == 0) selectedRequest && addressValid
    else busy && selectedRequest && addressValid && waited === latency.U
  private val selectedWriteLine = Wire(Vec(CacheParams.wordsPerLine, UInt(32.W)))
  private val selectedReadLine = Wire(Vec(CacheParams.wordsPerLine, UInt(32.W)))

  for (word <- 0 until CacheParams.wordsPerLine) {
    selectedWriteLine(word) := Mux(
      dSel,
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
      when(complete && selectedWrite) {
        memory(wordAddress) := selectedWriteLine(word)
      }
    }
  } else {
    val initialWords = initialData.padTo(depthWords, 0L).map(_.U(32.W))
    val memory = RegInit(VecInit(initialWords))

    for (word <- 0 until CacheParams.wordsPerLine) {
      val wordAddress = baseWord + word.U
      selectedReadLine(word) := memory(wordAddress)
      when(complete && selectedWrite) {
        memory(wordAddress) := selectedWriteLine(word)
      }
    }
  }

  io.dMem.ready := dSel && complete
  io.iMem.ready := !dSel && complete
  for (word <- 0 until CacheParams.wordsPerLine) {
    io.dMem.readLine(word) := Mux(addressValid, selectedReadLine(word), 0.U)
    io.iMem.readLine(word) := Mux(addressValid, selectedReadLine(word), 0.U)
  }

  if (latency > 0) {
    when(!busy) {
      when(selectedRequest) {
        busy := true.B
        ownerIsD := io.dMem.request
        waited := 1.U
      }
    }.elsewhen(!selectedRequest || complete) {
      busy := false.B // concluida, ou abortada porque o pedido caiu
    }.elsewhen(waited =/= latency.U) {
      waited := waited + 1.U
    }
  }

  when(selectedRequest && !addressAligned) {
    assert(false.B, "pedido a memoria principal deve estar alinhado ao inicio da linha")
  }
}