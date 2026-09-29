package riscv.cache

import chisel3._
import chisel3.util._

/** Parametros geometricos do L1 (compartilhados por ICache e DCache).
  *
  * Configuracao alvo:
  *   - L1 dividido em ICache + DCache
  *   - associatividade em conjunto 4-way
  *   - linha de 16 bytes (4 palavras de 32 bits)
  *   - substituicao LRU
  *   - escrita write-back + write-allocate
  *   - cache bloqueante, single core
  *
  * NOTA DE PROJETO: o enunciado fala em "capacidade total de 16KiB" para o
  * L1 ja dividido em ICache + DCache. Aqui assumimos que os 16KiB sao a
  * SOMA das duas caches (8KiB para a ICache, 8KiB para a DCache). Se a
  * intencao for 16KiB *por cache* (32KiB no total), basta trocar
  * `bytesPerCache` para apontar direto para 16 * 1024.
  */
object CacheParams {
  val lineBytes: Int = 16
  val ways: Int = 4
  val wordBytes: Int = 4
  val wordsPerLine: Int = lineBytes / wordBytes // 4 palavras por linha

  private val totalBytesBothCaches: Int = 16 * 1024
  val bytesPerCache: Int = totalBytesBothCaches / 2 // 8 KiB para cada cache

  val numLines: Int = bytesPerCache / lineBytes
  val numSets: Int = numLines / ways

  val offsetBits: Int = log2Ceil(lineBytes)
  val indexBits: Int = log2Ceil(numSets)
  val tagBits: Int = 32 - indexBits - offsetBits

  require(isPow2(ways), "ways precisa ser potência de 2 (necessário p/ LRU e indexação)")
  require(isPow2(numSets), "numSets precisa ser potência de 2 (necessario p/ indexação)")
}

/** Decomposicao do endereco de 32 bits em tag / index / offset, de acordo
  * com a geometria de CacheParams. Uso interno da cache, mas fica aqui
  * porque decorre diretamente do sinal de endereco da interface CPU<->Cache.
  */
class CacheAddress extends Bundle {
  val tag = UInt(CacheParams.tagBits.W)
  val index = UInt(CacheParams.indexBits.W)
  val offset = UInt(CacheParams.offsetBits.W)
}

object CacheAddress {
  def fromUInt(addr: UInt): CacheAddress = {
    val decoded = Wire(new CacheAddress)
    decoded.offset := addr(CacheParams.offsetBits - 1, 0)
    decoded.index := addr(
      CacheParams.offsetBits + CacheParams.indexBits - 1,
      CacheParams.offsetBits
    )
    decoded.tag := addr(31, CacheParams.offsetBits + CacheParams.indexBits)
    decoded
  }
}

/** Interface entre o pipeline (CPU) e uma cache L1 (ICache ou DCache).
  *
  * Direcoes definidas do ponto de vista da CPU: quem instancia a cache usa
  * `Flipped(new CpuCacheIO)`. Os campos de dado/tamanho reaproveitam a
  * mesma convencao de `DataMemory` (memSize/unsignedLoad de RV32I.MemorySize)
  * para nao duplicar semantica ja existente no projeto.
  */
class CpuCacheIO extends Bundle {
  // CPU -> Cache
  val request = Output(Bool()) // pedido valido neste ciclo
  val write = Output(Bool()) // false = load/fetch, true = store (sempre false na ICache)
  val address = Output(UInt(32.W)) // endereco de byte
  val writeData = Output(UInt(32.W))
  val memSize = Output(UInt(2.W)) // RV32I.MemorySize: BYTE/HALF/WORD
  val unsignedLoad = Output(Bool())

  // Cache -> CPU
  val readData = Input(UInt(32.W))
  val hit = Input(Bool()) // 1 quando o pedido deste ciclo foi atendido (dado em readData valido)
  val stall = Input(Bool()) // 1 enquanto a cache ainda resolve um miss; pipeline deve congelar
}

/** Interface entre uma cache L1 e o proximo nivel de memoria (a memoria
  * principal do projeto). Transfere uma linha inteira por vez: refill em
  * miss de leitura, write-back no despejo de uma linha suja.
  *
  * Como a cache e bloqueante e o design e single core, um simples par
  * request/ready (sem Decoupled/Valid-Ready do chisel3.util) e suficiente e
  * fica consistente com o estilo do resto do projeto.
  */
class CacheMemIO extends Bundle {
  // Cache -> Memoria
  val request = Output(Bool())
  val write = Output(Bool()) // false = le linha (refill), true = escreve linha (write-back)
  val address = Output(UInt(32.W)) // endereco alinhado ao inicio da linha
  val writeLine = Output(Vec(CacheParams.wordsPerLine, UInt(32.W)))

  // Memoria -> Cache
  val readLine = Input(Vec(CacheParams.wordsPerLine, UInt(32.W)))
  val ready = Input(Bool()) // 1 quando a operacao pedida (refill ou write-back) termina
}

/** Resultado da leitura combinacional de UMA via no conjunto selecionado. */
class CacheLineRead extends Bundle {
  val tag = UInt(CacheParams.tagBits.W)
  val valid = Bool()
  val dirty = Bool()
  val data = Vec(CacheParams.wordsPerLine, UInt(32.W))
}

/** Porta de escrita dos arrays (uma via por vez).
  *
  *   - `wordMask` seleciona quais palavras da linha sao gravadas: todas em um
  *     refill, apenas uma em um store hit.
  *   - `metaEn` grava tag/valid/dirty da via (refill, store hit -> dirty, etc.).
  *     Pode ser usado sem gravar dado (wordMask toda em false).
  */
class CacheWritePort extends Bundle {
  val en = Bool()
  val way = UInt(log2Ceil(CacheParams.ways).W)
  val index = UInt(CacheParams.indexBits.W)
  val wordMask = Vec(CacheParams.wordsPerLine, Bool())
  val data = Vec(CacheParams.wordsPerLine, UInt(32.W))
  val metaEn = Bool()
  val tag = UInt(CacheParams.tagBits.W)
  val valid = Bool()
  val dirty = Bool()
}

/** Armazenamento das linhas: dado, tag, valid e dirty, por via e por conjunto.
  *
  *   - dado e tag: `Mem` (leitura combinacional, escrita sincrona), o que
  *     infere RAM distribuida em FPGA em vez de milhares de flip-flops;
  *   - valid e dirty: registradores com reset (todas as linhas comecam
  *     invalidas e limpas).
  *
  * Leitura: dado `readIndex`, as `ways` vias do conjunto saem no mesmo ciclo,
  * sem comparacao de tag (isso e a logica de hit/miss, feita fora daqui).
  * Se ha leitura e escrita no mesmo indice no mesmo ciclo, a leitura devolve
  * o valor ANTIGO (a escrita so vale no proximo ciclo).
  */
class CacheArrays extends Module {
  val io = IO(new Bundle {
    val readIndex = Input(UInt(CacheParams.indexBits.W))
    val read = Output(Vec(CacheParams.ways, new CacheLineRead))
    val write = Input(new CacheWritePort)
  })

  private val sets = CacheParams.numSets
  private val tagMem = Seq.fill(CacheParams.ways)(Mem(sets, UInt(CacheParams.tagBits.W)))
  private val dataMem = Seq.fill(CacheParams.ways)(
    Mem(sets, Vec(CacheParams.wordsPerLine, UInt(32.W)))
  )
  private val validBits = RegInit(
    VecInit(Seq.fill(CacheParams.ways)(VecInit(Seq.fill(sets)(false.B))))
  )
  private val dirtyBits = RegInit(
    VecInit(Seq.fill(CacheParams.ways)(VecInit(Seq.fill(sets)(false.B))))
  )

  for (w <- 0 until CacheParams.ways) {
    // Leitura combinacional
    io.read(w).tag := tagMem(w).read(io.readIndex)
    io.read(w).data := dataMem(w).read(io.readIndex)
    io.read(w).valid := validBits(w)(io.readIndex)
    io.read(w).dirty := dirtyBits(w)(io.readIndex)

    // Escrita sincrona
    when(io.write.en && io.write.way === w.U) {
      dataMem(w).write(io.write.index, io.write.data, io.write.wordMask)
      when(io.write.metaEn) {
        tagMem(w).write(io.write.index, io.write.tag)
        validBits(w)(io.write.index) := io.write.valid
        dirtyBits(w)(io.write.index) := io.write.dirty
      }
    }
  }
}

/** Cache L1 unica (instanciada duas vezes: ICache e DCache).
  *
  * Config: 4-way set-associative, linha de 16B, write-back + write-allocate,
  * bloqueante, single core (ver CacheParams).
  *
  * Maquina de estados (controlador bloqueante):
  *
  *   sIdle       --request-->        sCompareTag
  *   sCompareTag --hit-->            sIdle          (atende load/store)
  *   sCompareTag --miss, vitima suja--> sWriteBack
  *   sCompareTag --miss, vitima limpa/invalida--> sAllocate
  *   sWriteBack  --mem.ready-->      sAllocate      (despeja a linha suja)
  *   sAllocate   --mem.ready-->      sCompareTag    (preenche a linha e reavalia)
  *
  * Contrato com a CPU: enquanto `stall` = 1 a CPU deve manter o pedido
  * (request/write/address/...) estavel, porque o controlador le o endereco
  * direto da porta durante todo o miss.
  *
  * Pendente: escolha da vitima por LRU (hoje: primeira via invalida, senao
  * via 0) e atualizacao do LRU nos hits.
  */
class Cache(isInstructionCache: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val cpu = Flipped(new CpuCacheIO)
    val mem = new CacheMemIO
  })

  private val wayBits = log2Ceil(CacheParams.ways)

  val addr = CacheAddress.fromUInt(io.cpu.address)
  val wordIdx = addr.offset(CacheParams.offsetBits - 1, 2)
  val byteOff = addr.offset(1, 0)

  // ---------------------------------------------------------------- arrays
  val arrays = Module(new CacheArrays)
  arrays.io.readIndex := addr.index
  // porta de escrita ociosa por padrao; a FSM sobrescreve abaixo
  arrays.io.write.en := false.B
  arrays.io.write.way := 0.U
  arrays.io.write.index := addr.index
  arrays.io.write.wordMask := VecInit(Seq.fill(CacheParams.wordsPerLine)(false.B))
  arrays.io.write.data := VecInit(Seq.fill(CacheParams.wordsPerLine)(0.U(32.W)))
  arrays.io.write.metaEn := false.B
  arrays.io.write.tag := addr.tag
  arrays.io.write.valid := false.B
  arrays.io.write.dirty := false.B

  // Vias do conjunto selecionado (leitura combinacional).
  val setLines = arrays.io.read

  // ------------------------------------------------------------ hit / miss
  val wayHit = VecInit(setLines.map(l => l.valid && l.tag === addr.tag))
  val tagHit = wayHit.asUInt.orR
  val hitWay = PriorityEncoder(wayHit.asUInt)
  val hitLine = setLines(hitWay)
  val hitWord = hitLine.data(wordIdx)

  // ------------------------------------------------- load: extracao de dado
  // Convencao de memSize (funct3[1:0] do RV32I): 0 = byte, 1 = half, 2 = word.
  val isByte = io.cpu.memSize === 0.U
  val isHalf = io.cpu.memSize === 1.U
  val shiftBits = Cat(byteOff, 0.U(3.W))
  val shifted = hitWord >> shiftBits
  val loadByte = Cat(Fill(24, shifted(7) && !io.cpu.unsignedLoad), shifted(7, 0))
  val loadHalf = Cat(Fill(16, shifted(15) && !io.cpu.unsignedLoad), shifted(15, 0))
  val loadData = Mux(isByte, loadByte, Mux(isHalf, loadHalf, hitWord))

  // ---------------------------------------------- store: merge na palavra
  val sizeMask = Mux(isByte, "hFF".U(32.W), Mux(isHalf, "hFFFF".U(32.W), "hFFFFFFFF".U(32.W)))
  val storeMask = (sizeMask << shiftBits)(31, 0)
  val storeWord = (hitWord & ~storeMask) | ((io.cpu.writeData << shiftBits)(31, 0) & storeMask)

  // ------------------------------------------------------- escolha de vitima
  val invalidWays = VecInit(setLines.map(l => !l.valid))
  val hasInvalid = invalidWays.asUInt.orR
  // TODO(LRU): trocar o 0.U por via menos recentemente usada do conjunto.
  val victimWay = Mux(hasInvalid, PriorityEncoder(invalidWays.asUInt), 0.U(wayBits.W))
  val victimWayLine = setLines(victimWay)
  val victimNeedsWriteBack = victimWayLine.valid && victimWayLine.dirty

  // ------------------------------------------------------------------- FSM
  val sIdle :: sCompareTag :: sWriteBack :: sAllocate :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val victimReg = RegInit(0.U(wayBits.W))
  val victimLine = setLines(victimReg)

  // Saidas padrao (Wire implicito: ultima atribuicao vence)
  io.cpu.hit := false.B
  io.cpu.readData := 0.U
  io.cpu.stall := false.B

  io.mem.request := false.B
  io.mem.write := false.B
  io.mem.address := Cat(addr.tag, addr.index, 0.U(CacheParams.offsetBits.W))
  io.mem.writeLine := VecInit(Seq.fill(CacheParams.wordsPerLine)(0.U(32.W)))

  switch(state) {
    is(sIdle) {
      // pedido aceito, ainda nao atendido
      io.cpu.stall := io.cpu.request
      when(io.cpu.request) { state := sCompareTag }
    }

    is(sCompareTag) {
      when(!io.cpu.request) {
        state := sIdle
      }.elsewhen(tagHit) {
        io.cpu.hit := true.B
        io.cpu.readData := loadData
        state := sIdle

        if (!isInstructionCache) {
          when(io.cpu.write) {
            // store hit: grava so a palavra alvo e marca a linha como suja
            arrays.io.write.en := true.B
            arrays.io.write.way := hitWay
            arrays.io.write.index := addr.index
            arrays.io.write.wordMask := VecInit(
              (0 until CacheParams.wordsPerLine).map(i => wordIdx === i.U)
            )
            arrays.io.write.data := VecInit(Seq.fill(CacheParams.wordsPerLine)(storeWord))
            arrays.io.write.metaEn := true.B
            arrays.io.write.tag := addr.tag
            arrays.io.write.valid := true.B
            arrays.io.write.dirty := true.B
          }
        }
      }.otherwise {
        io.cpu.stall := true.B
        victimReg := victimWay
        state := Mux(victimNeedsWriteBack, sWriteBack, sAllocate)
      }
    }

    is(sWriteBack) {
      io.cpu.stall := true.B
      io.mem.request := true.B
      io.mem.write := true.B
      io.mem.address := Cat(victimLine.tag, addr.index, 0.U(CacheParams.offsetBits.W))
      io.mem.writeLine := victimLine.data
      when(io.mem.ready) { state := sAllocate }
    }

    is(sAllocate) {
      io.cpu.stall := true.B
      io.mem.request := true.B // write = false (refill), endereco alinhado a linha
      when(io.mem.ready) {
        arrays.io.write.en := true.B
        arrays.io.write.way := victimReg
        arrays.io.write.index := addr.index
        arrays.io.write.wordMask := VecInit(Seq.fill(CacheParams.wordsPerLine)(true.B))
        arrays.io.write.data := io.mem.readLine
        arrays.io.write.metaEn := true.B
        arrays.io.write.tag := addr.tag
        arrays.io.write.valid := true.B
        arrays.io.write.dirty := false.B
        state := sCompareTag // reavalia: agora e hit (e trata o store, se for o caso)
      }
    }
  }

  // Endereco precisa ficar estavel enquanto o miss e resolvido (ver contrato).
  val prevBusy = RegNext(io.cpu.request && io.cpu.stall, false.B)
  val prevAddress = RegNext(io.cpu.address)
  assert(
    !(prevBusy && io.cpu.request && io.cpu.address =/= prevAddress),
    "endereco mudou durante um miss em andamento"
  )

  if (isInstructionCache) {
    // ICache e somente leitura: a CPU nunca deve pedir escrita por essa porta.
    assert(!io.cpu.write, "ICache nao aceita escrita")
  }
}

/** L1 dividido em ICache + DCache, cada uma seguindo CacheParams.
  *
  * Cada cache expoe sua propria porta de memoria (iMem/dMem). Como o design
  * e bloqueante e single core, isso evita qualquer arbitragem por enquanto
  * -- exatamente como o projeto ja faz hoje com InstructionMemory e
  * DataMemory como modulos separados. Se no futuro as duas caches
  * precisarem compartilhar um unico barramento fisico de memoria, um
  * arbitro simples (prioridade fixa para a DCache, por exemplo) entra aqui
  * na frente de iMem/dMem.
  */
class L1Cache extends Module {
  val io = IO(new Bundle {
    // Pipeline -> Cache (ligar no estagio de fetch e no estagio de memoria)
    val iCache = Flipped(new CpuCacheIO)
    val dCache = Flipped(new CpuCacheIO)

    // Cache -> Memoria principal
    val iMem = new CacheMemIO
    val dMem = new CacheMemIO
  })

  val instructionCache = Module(new Cache(isInstructionCache = true))
  val dataCache = Module(new Cache(isInstructionCache = false))

  // CPU <-> ICache
  instructionCache.io.cpu.request := io.iCache.request
  instructionCache.io.cpu.write := io.iCache.write
  instructionCache.io.cpu.address := io.iCache.address
  instructionCache.io.cpu.writeData := io.iCache.writeData
  instructionCache.io.cpu.memSize := io.iCache.memSize
  instructionCache.io.cpu.unsignedLoad := io.iCache.unsignedLoad
  io.iCache.readData := instructionCache.io.cpu.readData
  io.iCache.hit := instructionCache.io.cpu.hit
  io.iCache.stall := instructionCache.io.cpu.stall

  // ICache <-> Memoria
  io.iMem.request := instructionCache.io.mem.request
  io.iMem.write := instructionCache.io.mem.write
  io.iMem.address := instructionCache.io.mem.address
  io.iMem.writeLine := instructionCache.io.mem.writeLine
  instructionCache.io.mem.readLine := io.iMem.readLine
  instructionCache.io.mem.ready := io.iMem.ready

  // CPU <-> DCache
  dataCache.io.cpu.request := io.dCache.request
  dataCache.io.cpu.write := io.dCache.write
  dataCache.io.cpu.address := io.dCache.address
  dataCache.io.cpu.writeData := io.dCache.writeData
  dataCache.io.cpu.memSize := io.dCache.memSize
  dataCache.io.cpu.unsignedLoad := io.dCache.unsignedLoad
  io.dCache.readData := dataCache.io.cpu.readData
  io.dCache.hit := dataCache.io.cpu.hit
  io.dCache.stall := dataCache.io.cpu.stall

  // DCache <-> Memoria
  io.dMem.request := dataCache.io.mem.request
  io.dMem.write := dataCache.io.mem.write
  io.dMem.address := dataCache.io.mem.address
  io.dMem.writeLine := dataCache.io.mem.writeLine
  dataCache.io.mem.readLine := io.dMem.readLine
  dataCache.io.mem.ready := io.dMem.ready
}