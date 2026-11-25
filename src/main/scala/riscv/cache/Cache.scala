package riscv.cache

import chisel3._
import chisel3.util._

/** Cache L1 unica (instanciada duas vezes: ICache e DCache).
  *
  * Config: 4-way set-associative, linha de 16B, write-back + write-allocate,
  * bloqueante, single core (ver CacheParams).
  *
  * Maquina de estados (controlador bloqueante):
  *
  * sIdle --request--> sCompareTag sCompareTag --hit--> sIdle (atende
  * load/store) sCompareTag --miss, vitima suja--> sWriteBack sCompareTag
  * --miss, vitima limpa/invalida--> sAllocate sWriteBack --mem.ready-->
  * sAllocate (despeja a linha suja) sAllocate --mem.ready--> sCompareTag
  * (preenche a linha e reavalia) sWriteBack/sAllocate --mem.fault--> sFault
  * (io.cpu.fault = 1 ate a CPU derrubar o pedido; os arrays nao mudam)
  *
  * `cfg.blockRam = true` (arrays com leitura sincrona, ver CacheArrays):
  *   - o ciclo de sIdle apresenta o indice e sCompareTag consome o resultado,
  *     entao o hit continua levando 2 ciclos;
  *   - depois do refill entra sRefillWait (1 ciclo): sAllocate --mem.ready-->
  *     sRefillWait --> sCompareTag, porque a linha so pode ser lida um ciclo
  *     depois de gravada (1 ciclo a mais por miss);
  *   - o flush gasta 2 ciclos por conjunto (1 para apresentar o indice, 1 para
  *     avaliar), em vez de 1.
  *
  * Contrato com a CPU: enquanto `stall` = 1 a CPU deve manter o pedido
  * (request/write/address/...) estavel, porque o controlador le o endereco
  * direto da porta durante todo o miss.
  *
  * Reposicao: em miss, a vitima e a primeira via invalida do conjunto; se todas
  * sao validas, a menos recentemente usada (CacheReplacement: LRU, FIFO ou
  * aleatoria). O LRU e atualizado em todo hit; como o refill termina em
  * sCompareTag, a linha recem-alocada tambem vira MRU pelo hit que atende o
  * pedido.
  */
class Cache(
    isInstructionCache: Boolean = false,
    cfg: CacheConfig = CacheConfig.default
) extends Module {
  val io = IO(new Bundle {
    val cpu = Flipped(new CpuCacheIO)
    val mem = new CacheMemIO(cfg)
    val flushDirty = Input(Bool())
    val flushDone = Output(Bool())
    // 1 (nivel, valido junto com flushDone) se um write-back do flush foi
    // recusado pela memoria: o flush foi abortado e ainda ha linhas sujas.
    val flushFault = Output(Bool())
    val perf = Output(new CachePerf)
    val perfClear = Input(Bool()) // zera todos os contadores
    val invalidateAll = Input(Bool())
    // Observabilidade para testes (validos no ciclo em que hit = 1).
    val dbg = Output(new Bundle {
      val hitWay = UInt(cfg.wayBits.W) // via em que a tag casou
      val hitDirty =
        Bool() // dirty da linha ANTES de um eventual store deste ciclo
    })
  })

  private val wayBits = cfg.wayBits

  private val states = Enum(9)
  val sIdle = states(0)
  val sCompareTag = states(1)
  val sWriteBack = states(2)
  val sAllocate = states(3)
  val sFlushScan = states(4)
  val sFlushWriteBack = states(5)
  val sFlushDone = states(6)
  // A memoria recusou o refill/write-back do pedido corrente; fica aqui,
  // com io.cpu.fault = 1, ate a CPU derrubar o pedido.
  val sFault = states(7)
  // So com blockRam: 1 ciclo entre o refill e sCompareTag (ver cabecalho).
  val sRefillWait = states(8)
  val state = RegInit(sIdle)
  val victimReg = RegInit(0.U(wayBits.W))
  val flushSet = RegInit(0.U(cfg.indexBits.W))
  // So com blockRam: tag/dado do conjunto `flushSet` ja chegaram (1 ciclo apos o
  // indice ser apresentado). Sem blockRam a leitura e combinacional: sempre 1.
  val flushReadValid = RegInit(false.B)
  val flushDataReady: Bool = if (cfg.blockRam) flushReadValid else true.B
  val flushWay = RegInit(0.U(wayBits.W))
  val flushTag = RegInit(0.U(cfg.tagBits.W))
  val flushData = RegInit(
    VecInit(Seq.fill(cfg.wordsPerLine)(0.U(32.W)))
  )

  val addr = CacheAddress.fromUInt(io.cpu.address, cfg)
  val wordIdx = addr.offset(cfg.offsetBits - 1, 2)
  val byteOff = addr.offset(1, 0)

  // ---------------------------------------------------------------- arrays
  // A ICache e somente leitura: seus arrays nao tem o bit de sujeira.
  val arrays = Module(new CacheArrays(cfg, hasDirty = !isInstructionCache))
  val flushing = state === sFlushScan || state === sFlushWriteBack
  arrays.io.readIndex := Mux(flushing, flushSet, addr.index)
  arrays.io.invalidateAll := io.invalidateAll
  // porta de escrita ociosa por padrao; a FSM sobrescreve abaixo
  arrays.io.write.en := false.B
  arrays.io.write.way := 0.U
  arrays.io.write.index := addr.index
  arrays.io.write.wordMask := VecInit(
    Seq.fill(cfg.wordsPerLine)(false.B)
  )
  arrays.io.write.data := VecInit(Seq.fill(cfg.wordsPerLine)(0.U(32.W)))
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
  io.dbg.hitWay := hitWay
  io.dbg.hitDirty := hitLine.dirty

  // ------------------------------------------------------------------- LRU
  // Politica de substituicao escolhida em cfg.policy (LRU, FIFO ou aleatoria).
  val repl = CacheReplacement(cfg)
  repl.io.index := addr.index
  repl.io.touch := false.B // a FSM liga em todo hit
  repl.io.touchIndex := addr.index
  repl.io.touchWay := hitWay
  repl.io.fill := false.B // a FSM liga quando um refill termina
  repl.io.fillIndex := addr.index
  repl.io.fillWay := 0.U
  repl.io.clear := io.invalidateAll

  // ------------------------------------------------- load: extracao de dado
  // Convencao de memSize (funct3[1:0] do RV32I): 0 = byte, 1 = half, 2 = word.
  val isByte = io.cpu.memSize === 0.U
  val isHalf = io.cpu.memSize === 1.U
  val shiftBits = Cat(byteOff, 0.U(3.W))
  val shifted = hitWord >> shiftBits
  val loadByte =
    Cat(Fill(24, shifted(7) && !io.cpu.unsignedLoad), shifted(7, 0))
  val loadHalf =
    Cat(Fill(16, shifted(15) && !io.cpu.unsignedLoad), shifted(15, 0))
  // A ICache sempre entrega a palavra inteira (o fetch seleciona o que precisa),
  // ignorando memSize/unsignedLoad.
  val loadData =
    if (isInstructionCache) hitWord
    else Mux(isByte, loadByte, Mux(isHalf, loadHalf, hitWord))

  // ---------------------------------------------- store: merge na palavra
  val sizeMask = Mux(
    isByte,
    "hFF".U(32.W),
    Mux(isHalf, "hFFFF".U(32.W), "hFFFFFFFF".U(32.W))
  )
  val storeMask = (sizeMask << shiftBits)(31, 0)
  val storeWord = (hitWord & ~storeMask) | ((io.cpu.writeData << shiftBits)(
    31,
    0
  ) & storeMask)

  // ------------------------------------------------------- escolha de vitima
  val invalidWays = VecInit(setLines.map(l => !l.valid))
  val hasInvalid = invalidWays.asUInt.orR
  val victimWay =
    Mux(hasInvalid, PriorityEncoder(invalidWays.asUInt), repl.io.victim)
  val victimWayLine = setLines(victimWay)
  val victimNeedsWriteBack = victimWayLine.valid && victimWayLine.dirty

  // ------------------------------------------------------------------- FSM
  val victimLine = setLines(victimReg)
  val flushDirtyWays = VecInit(setLines.map(line => line.valid && line.dirty))
  val flushCandidateWay = PriorityEncoder(flushDirtyWays.asUInt)
  val flushCandidate = setLines(flushCandidateWay)

  // Saidas padrao (Wire implicito: ultima atribuicao vence)
  io.cpu.hit := false.B
  io.cpu.readData := 0.U
  io.cpu.stall := false.B
  io.cpu.fault := state === sFault

  io.mem.request := false.B
  io.mem.write := false.B
  io.mem.address := Cat(addr.tag, addr.index, 0.U(cfg.offsetBits.W))
  io.mem.writeLine := VecInit(Seq.fill(cfg.wordsPerLine)(0.U(32.W)))
  io.flushDone := state === sFlushDone
  val flushFaultReg = RegInit(false.B)
  io.flushFault := flushFaultReg

  // ---- Contadores de desempenho (ver CachePerf) ---------------------------
  // Eventos de um ciclo, ligados na FSM abaixo.
  val evHit = WireDefault(false.B)
  val evMiss = WireDefault(false.B)
  val evWriteBack = WireDefault(false.B)
  val evFlushWriteBack = WireDefault(false.B)
  val evFault = WireDefault(false.B)
  // 1 enquanto o pedido corrente ja contou um miss (o hit pos-refill nao e hit)
  val refilling = RegInit(false.B)
  val evMissCycle = state === sWriteBack || state === sAllocate ||
    state === sRefillWait ||
    (state === sCompareTag && io.cpu.request && !tagHit)

  private def counter(event: Bool): UInt = {
    val c = RegInit(0.U(64.W))
    when(io.perfClear) { c := 0.U }.elsewhen(event) { c := c + 1.U }
    c
  }
  io.perf.hits := counter(evHit)
  io.perf.misses := counter(evMiss)
  io.perf.writeBacks := counter(evWriteBack)
  io.perf.flushWriteBacks := counter(evFlushWriteBack)
  io.perf.faults := counter(evFault)
  io.perf.missCycles := counter(evMissCycle)

  switch(state) {
    is(sIdle) {
      // pedido aceito, ainda nao atendido
      io.cpu.stall := io.cpu.request || io.flushDirty
      when(io.flushDirty) {
        flushFaultReg := false.B
        flushSet := 0.U
        flushWay := 0.U
        flushReadValid := false.B
        state := sFlushScan
      }.elsewhen(io.cpu.request) {
        state := sCompareTag
      }
    }

    is(sCompareTag) {
      when(!io.cpu.request) {
        refilling := false.B
        state := sIdle
      }.elsewhen(tagHit) {
        evHit := !refilling
        refilling := false.B
        io.cpu.hit := true.B
        io.cpu.readData := loadData
        repl.io.touch := true.B // via de acerto (LRU: passa a MRU)
        state := sIdle

        if (!isInstructionCache) {
          when(io.cpu.write) {
            // store hit: grava so a palavra alvo e marca a linha como suja
            arrays.io.write.en := true.B
            arrays.io.write.way := hitWay
            arrays.io.write.index := addr.index
            arrays.io.write.wordMask := VecInit(
              (0 until cfg.wordsPerLine).map(i => wordIdx === i.U)
            )
            arrays.io.write.data := VecInit(
              Seq.fill(cfg.wordsPerLine)(storeWord)
            )
            arrays.io.write.metaEn := true.B
            arrays.io.write.tag := addr.tag
            arrays.io.write.valid := true.B
            arrays.io.write.dirty := true.B
          }
        }
      }.otherwise {
        evMiss := true.B
        refilling := true.B
        io.cpu.stall := true.B
        victimReg := victimWay
        state := Mux(victimNeedsWriteBack, sWriteBack, sAllocate)
      }
    }

    is(sWriteBack) {
      io.cpu.stall := true.B
      io.mem.request := true.B
      io.mem.write := true.B
      io.mem.address := Cat(
        victimLine.tag,
        addr.index,
        0.U(cfg.offsetBits.W)
      )
      io.mem.writeLine := victimLine.data
      when(io.mem.ready) {
        evWriteBack := true.B
        state := sAllocate
      }.elsewhen(io.mem.fault) {
        evFault := true.B
        refilling := false.B
        // A vitima continua valida e suja: nenhum dado se perde e a proxima
        // tentativa refaz o write-back.
        state := sFault
      }
    }

    is(sAllocate) {
      io.cpu.stall := true.B
      io.mem.request := true.B // write = false (refill), endereco alinhado a linha
      when(io.mem.ready) {
        arrays.io.write.en := true.B
        arrays.io.write.way := victimReg
        arrays.io.write.index := addr.index
        arrays.io.write.wordMask := VecInit(
          Seq.fill(cfg.wordsPerLine)(true.B)
        )
        arrays.io.write.data := io.mem.readLine
        arrays.io.write.metaEn := true.B
        arrays.io.write.tag := addr.tag
        arrays.io.write.valid := true.B
        arrays.io.write.dirty := false.B
        repl.io.fill := true.B
        repl.io.fillWay := victimReg
        // reavalia: agora e hit (e trata o store, se for o caso). Com blockRam a
        // linha recem-gravada so pode ser lida um ciclo depois.
        state := (if (cfg.blockRam) sRefillWait else sCompareTag)
      }.elsewhen(io.mem.fault) {
        evFault := true.B
        refilling := false.B
        // Nada e gravado nos arrays: a linha nao passa a existir na cache.
        state := sFault
      }
    }

    is(sRefillWait) {
      // Ciclo de espera: o indice e apresentado de novo, e no proximo ciclo
      // (sCompareTag) tag e dado ja refletem a linha gravada em sAllocate.
      io.cpu.stall := true.B
      state := sCompareTag
    }

    is(sFlushScan) {
      io.cpu.stall := true.B
      when(!flushDataReady) {
        // blockRam: espera tag/dado do conjunto `flushSet` chegarem
        flushReadValid := true.B
      }.elsewhen(flushDirtyWays.asUInt.orR) {
        flushWay := flushCandidateWay
        flushTag := flushCandidate.tag
        flushData := flushCandidate.data
        state := sFlushWriteBack
      }.otherwise {
        when(flushSet === (cfg.numSets - 1).U) {
          state := sFlushDone
        }.otherwise {
          flushSet := flushSet + 1.U
          flushReadValid := false.B
        }
      }
    }

    is(sFlushWriteBack) {
      io.cpu.stall := true.B
      io.mem.request := true.B
      io.mem.write := true.B
      io.mem.address := Cat(flushTag, flushSet, 0.U(cfg.offsetBits.W))
      io.mem.writeLine := flushData
      when(io.mem.ready) {
        evFlushWriteBack := true.B
        arrays.io.write.en := true.B
        arrays.io.write.way := flushWay
        arrays.io.write.index := flushSet
        arrays.io.write.wordMask := VecInit(
          Seq.fill(cfg.wordsPerLine)(false.B)
        )
        arrays.io.write.data := flushData
        arrays.io.write.metaEn := true.B
        arrays.io.write.tag := flushTag
        arrays.io.write.valid := true.B
        arrays.io.write.dirty := false.B
        state := sFlushScan
      }.elsewhen(io.mem.fault) {
        // Aborta o flush: a linha continua suja (rescanear a repetiria para
        // sempre). O erro fica registrado em flushFault.
        flushFaultReg := true.B
        evFault := true.B
        state := sFlushDone
      }
    }

    is(sFault) {
      // io.cpu.fault = 1 (stall = 0, hit = 0). Sai quando o pedido cai.
      when(!io.cpu.request) { state := sIdle }
    }

    is(sFlushDone) {
      io.cpu.stall := io.cpu.request
      when(!io.flushDirty) { state := sIdle }
    }
  }

  // Acesso desalinhado nao e suportado: o merge/extracao trabalha dentro de uma
  // unica palavra e corromperia o dado silenciosamente. Falha alto em simulacao.
  if (!isInstructionCache) {
    val misaligned =
      (isHalf && byteOff(0)) || (!isByte && !isHalf && byteOff =/= 0.U)
    assert(
      !(io.cpu.request && misaligned),
      "acesso desalinhado nao suportado pela DCache"
    )
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
    assert(!io.flushDirty, "ICache nao suporta flushDirty")
  }
}
