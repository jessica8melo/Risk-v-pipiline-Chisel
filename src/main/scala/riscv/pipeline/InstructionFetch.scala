package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.compressed.RvcDecompressor

/** Busca RV32 com RVC e um buffer de palavra, antes do decode.
  *
  * A memória (a ICache) devolve palavras de 32 bits alinhadas. O pedido é feito
  * por `memoryReadEnable`/`memoryAddress` e a resposta chega quando
  * `memoryReady` fica alto (o `hit` da ICache), com `memoryData` válido só
  * nesse ciclo. Enquanto `memoryReadEnable && !memoryReady` (miss em andamento)
  * o pedido e o endereço ficam estáveis: o chamador não deve mudar `pc` nem dar
  * `flush` nesse intervalo (contrato da Cache).
  *
  * A palavra recebida fica no buffer (`word`), mesmo que o pipeline esteja
  * parado por outro motivo; assim a busca não se repete e o PC seguinte, na
  * mesma palavra, não gera novo pedido. Uma instrução de 32 bits na metade
  * superior de uma palavra usa a palavra guardada e a seguinte.
  */
class InstructionFetch extends Module {
  val io = IO(new Bundle {
    // Endereco em bytes da instrucao que o processador quer executar.
    val pc = Input(UInt(32.W))
    // enable = false suspende os pedidos (ex.: FENCE.I em andamento);
    // flush descarta o dado guardado apos um desvio, trap ou FENCE.I.
    val enable = Input(Bool())
    val flush = Input(Bool())
    // Resposta da memoria: memoryReady indica que memoryData e valido agora.
    val memoryReady = Input(Bool())
    val memoryData = Input(UInt(32.W))
    // Pedido a memoria: endereco em bytes (alinhado a palavra); nao e o PC.
    val memoryAddress = Output(UInt(32.W))
    val memoryReadEnable = Output(Bool())
    // Instrucao pronta para o decode, sempre representada em 32 bits.
    // Uma instrucao RVC de 16 bits e expandida antes de sair daqui.
    val instr = Output(UInt(32.W))
    // Tamanho original da instrucao: 2 ou 4 bytes, usado para avancar o PC.
    val length = Output(UInt(3.W))
    val illegal = Output(Bool())
    // So consuma instr quando valid estiver true.
    val valid = Output(Bool())
  })

  // Guarda a ultima palavra lida, para reutilizar a metade superior ou
  // montar uma instrucao de 32 bits que atravessa duas palavras.
  val word = RegInit(0.U(32.W))
  val wordAddress = RegInit(0.U(32.W))
  val wordValid = RegInit(false.B)

  // Zerar os 2 bits inferiores encontra o inicio da palavra: PC 0 e PC 2 ficam
  // na mesma palavra, em metades distintas.
  val alignedPc = Cat(io.pc(31, 2), 0.U(2.W))
  val hit = wordValid && wordAddress === alignedPc

  // A metade superior da palavra guardada comeca uma instrucao de 32 bits
  // quando seus bits [1:0] sao 11. Nesse caso, os outros 16 bits estao
  // na proxima palavra; [4:2] == 111 indica um tamanho maior nao suportado.
  val bufferedCross = hit && io.pc(1) && word(17, 16) === 3.U &&
    word(20, 18) =/= 7.U

  // O pedido NAO depende de flush (evita laco combinacional com o
  // redirecionamento do pipeline): o flush so invalida o buffer.
  io.memoryReadEnable := io.enable && (!hit || bufferedCross)
  io.memoryAddress := Mux(bufferedCross, alignedPc + 4.U, alignedPc)

  // Palavra que contem o inicio da instrucao: o buffer, ou a resposta da
  // memoria neste ciclo (so significativa com memoryReady).
  val firstWord = Mux(hit, word, io.memoryData)
  val half = Mux(io.pc(1), firstWord(31, 16), firstWord(15, 0))
  // Em RISC-V com extensao C, finais diferentes de 11 significam 16 bits.
  val compressed = half(1, 0) =/= 3.U
  // Prefixo 111 em [4:2] anuncia uma instrucao maior que 32 bits, nao suportada.
  val unsupportedLength = !compressed && half(4, 2) === 7.U
  // Instrucao normal de 32 bits iniciada na metade superior cruza duas palavras.
  val cross = io.pc(1) && !compressed && !unsupportedLength

  val decompressor = Module(new RvcDecompressor)
  decompressor.io.inst_c := half

  // Sem cruzamento: basta ter a palavra (buffer ou resposta). Com cruzamento:
  // a primeira palavra precisa estar no buffer e a seguinte chegar agora; se a
  // primeira acabou de chegar (sem buffer), ela e guardada e a busca continua.
  val available = Mux(cross, hit && io.memoryReady, hit || io.memoryReady)
  io.valid := io.enable && !io.flush && available
  io.length := Mux(compressed, 2.U, 4.U)
  io.illegal := io.valid && (io.pc(0) || unsupportedLength ||
    (compressed && decompressor.io.illegal))

  // Para cruzamento, concatena os 16 bits da palavra seguinte com os 16 atuais.
  // Instrucoes ilegais ou ainda invalidas aparecem como NOP para evitar lixo no decode.
  val fullInstr = Mux(cross, Cat(io.memoryData(15, 0), half), firstWord)
  io.instr := Mux(
    io.valid && !io.illegal,
    Mux(compressed, decompressor.io.inst_out, fullInstr),
    "h00000013".U
  )

  when(io.flush) {
    // Um desvio/trap/FENCE.I mudou o fluxo: a palavra antiga nao deve ser reutilizada.
    wordValid := false.B
  }.elsewhen(io.memoryReadEnable && io.memoryReady) {
    // Registra a palavra recebida e o endereco ao qual ela pertence.
    word := io.memoryData
    wordAddress := io.memoryAddress
    wordValid := true.B
  }
}
