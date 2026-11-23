package riscv.pipeline

import chisel3._
import chisel3.util._
import riscv.atomic.{AmoAlu, ReservationUnit}
import riscv.cache._
import riscv.decode.{ControlSignals, Controller, ImmGen, RV32I}
import riscv.execute.ULA
import riscv.privileged.{CLINT, CSR, InterruptCode}
import riscv.registers.RegisterFile

// ============================================================
// BUNDLE: registradores do estágio ID/EX
// ============================================================
class DecodeExecuteBundle extends Bundle {
  val valid = Bool()
  val pc = UInt(32.W)
  val instr = UInt(32.W)
  val instrLength = UInt(3.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val rd = UInt(5.W)
  val rs1Value = UInt(32.W)
  val rs2Value = UInt(32.W)
  val imm = UInt(32.W)
  val memAddress = UInt(32.W)
  val memWriteData = UInt(32.W)
  val signals = new ControlSignals
  val csrAddress = UInt(12.W)
  val csrWriteData = UInt(32.W)
  val csrReadData = UInt(32.W)
  // Busca que falhou (instruction access fault) e encoding original da
  // instrucao (mtval de uma instrucao ilegal).
  val fetchFault = Bool()
  val fetchFaultAddr = UInt(32.W)
  val rawInstr = UInt(32.W)
}

/** Pipeline RV32 com instruções comprimidas, de 3 estágios, inspirado no
  * Wildcat.
  *
  * Estágios:
  *   - IF: busca, alinhamento e descompressão RVC (InstructionFetch + ICache)
  *   - ID/RF: decodificação, leitura do banco de registradores e preparação
  *   - EX/MEM/WB: execução (ULA), acesso à memória e writeback
  *
  * Características:
  *   - ICache/DCache ligadas a uma memória principal simulada compartilhada;
  *   - forwarding do resultado do estágio EX para o ID;
  *   - stall de um ciclo para load-use hazard (com registrador de WB);
  *   - suporte a CSRs, CLINT e extensão A (LR/SC e AMOs).
  */
class Pipeline3(
    initialProgram: Seq[Long] = Seq.empty,
    memoryWords: Int = 1024,
    programFile: String = "",
    memoryLatency: Int = 0,
    cacheConfig: CacheConfig = CacheConfig.default
) extends Module {
  val io = IO(new Bundle {
    val pc = Output(UInt(32.W))
    val instr = Output(UInt(32.W))
    val aluResult = Output(UInt(32.W))
    val writebackData = Output(UInt(32.W))
    val writebackRd = Output(UInt(5.W))
    val writebackEnable = Output(Bool())
    val illegal = Output(Bool())
    val stalled = Output(Bool())

    // Debug do CLINT
    val debug_clint_trap_assert = Output(Bool())
    val debug_clint_trap_to_smode = Output(Bool())
    val debug_clint_delegate_to_smode = Output(Bool())
    val debug_clint_exception_code = Output(UInt(31.W))
    val debug_clint_is_interrupt = Output(Bool())
    val debug_clint_trap_address = Output(UInt(32.W))

    // Debug do CSR forwarding
    val debug_csrReadData = Output(UInt(32.W))
    val debug_csrForwarding = Output(Bool())
    val debug_csr_address_id = Output(UInt(12.W))
    val debug_csr_address_ex = Output(UInt(12.W))
  })

  import RV32I._

  val nop = "h00000013".U(32.W) // addi x0, x0, 0

  // ============================================================
  // 1. INSTANCIAÇÃO DOS MÓDULOS
  // ============================================================
  val l1Cache = Module(new L1Cache(cacheConfig))
  val mainMemory = Module(
    new CacheMainMemory(
      depthWords = memoryWords,
      initialData = initialProgram,
      programFile = programFile,
      latency = memoryLatency,
      cfg = cacheConfig
    )
  )
  // Contadores de desempenho da cache: por enquanto nao zerados pelo pipeline.
  l1Cache.io.perfClear := false.B
  l1Cache.io.iMem <> mainMemory.io.iMem
  l1Cache.io.dMem <> mainMemory.io.dMem
  val fetch = Module(new InstructionFetch)
  val regFile = Module(new RegisterFile)
  val immGen = Module(new ImmGen)
  val controller = Module(new Controller)
  val ula = Module(new ULA)
  val amoAlu = Module(new AmoAlu)
  val reservation = Module(new ReservationUnit)
  val csr = Module(new CSR)
  val clint = Module(new CLINT)

  // ============================================================
  // 2. REGISTRADORES DE PIPELINE
  // ============================================================
  val pcReg = RegInit(0.U(32.W))
  val ifIdPc = RegInit(0.U(32.W))
  val ifIdInstr = RegInit(nop)
  val ifIdValid = RegInit(false.B)
  val ifIdLength = RegInit(4.U(3.W))
  val ifIdIllegal = RegInit(false.B)
  val ifIdFault = RegInit(false.B)
  val ifIdFaultAddr = RegInit(0.U(32.W))
  val ifIdRaw = RegInit(nop)
  // Depois de uma falha de busca, para de buscar ate o proximo flush (trap ou
  // desvio): as instrucoes seguintes seriam descartadas de qualquer jeito.
  val fetchHalted = RegInit(false.B)
  val idEx = RegInit(0.U.asTypeOf(new DecodeExecuteBundle))

  // Registrador de WB para o load durante stall
  val lwWbValid = RegInit(false.B)
  val lwWbRd = RegInit(0.U(5.W))
  val lwWbData = RegInit(0.U(32.W))

  // Respostas das caches concluidas enquanto o restante do pipeline aguardava.
  val dataAccessDone = RegInit(false.B)
  val dataAccessData = RegInit(0.U(32.W))

  // A reserva de LR/SC fica no ReservationUnit (ver secao 11).

  // ============================================================
  // 3. SINAIS DE CONTROLE DE FLUXO
  // ============================================================
  val stallPipeline = WireDefault(false.B)
  val flushPipeline = WireDefault(false.B)
  val cacheWait = WireDefault(false.B)
  val hazardStall = WireDefault(false.B)
  val fenceIComplete = WireDefault(false.B)
  val exCanCommit = WireDefault(false.B)
  val cachedReadData = WireDefault(0.U(32.W))

  // ============================================================
  // 4. CONEXÃO CLINT <-> CSR
  // ============================================================
  clint.io.mstatus := csr.io.mstatus_read
  clint.io.mepc := csr.io.mepc_read
  clint.io.mcause := csr.io.mcause_read
  clint.io.mtvec := csr.io.mtvec_read
  clint.io.mie := csr.io.mie_read
  clint.io.mtval := csr.io.mtval_read

  clint.io.sstatus := csr.io.sstatus_read
  clint.io.sepc := csr.io.sepc_read
  clint.io.scause := csr.io.scause_read
  clint.io.stvec := csr.io.stvec_read
  clint.io.sie := csr.io.sie_read
  clint.io.stval := csr.io.stval_read

  clint.io.medeleg := csr.io.medeleg_read
  clint.io.mideleg := csr.io.mideleg_read
  clint.io.current_priv := csr.io.current_priv

  // ============================================================
  // 5b. CHECAGEM DE PRIVILEGIO (no EX, com o privilegio vigente)
  // ============================================================
  // Sao ilegais (mcause 2, mtval = encoding):
  //   - CSR inexistente, ou de nivel mais alto que o privilegio atual
  //     (csr[9:8] = menor privilegio que acessa);
  //   - escrita em CSR somente leitura (csr[11:10] = 11); CSRRS/CSRRC com rs1 = 0
  //     (ou zimm = 0) so leem e sao permitidos;
  //   - MRET fora de M; SRET em U, ou em S com mstatus.TSR = 1.
  val priv = csr.io.current_priv
  val csrAddr = idEx.csrAddress
  // CSRs implementados; misa, mip e sip existem e leem zero (escrita ignorada).
  val knownCsrs = Seq(
    CSRAddress.mstatus,
    CSRAddress.misa,
    CSRAddress.medeleg,
    CSRAddress.mideleg,
    CSRAddress.mie,
    CSRAddress.mtvec,
    CSRAddress.mscratch,
    CSRAddress.mepc,
    CSRAddress.mcause,
    CSRAddress.mtval,
    CSRAddress.mip,
    CSRAddress.sstatus,
    CSRAddress.sie,
    CSRAddress.stvec,
    CSRAddress.sscratch,
    CSRAddress.sepc,
    CSRAddress.scause,
    CSRAddress.stval,
    CSRAddress.sip,
    CSRAddress.satp,
    CSRAddress.mvendorid,
    CSRAddress.marchid,
    CSRAddress.mimpid,
    CSRAddress.mhartid
  )
  val csrKnown = VecInit(knownCsrs.map(_ === csrAddr)).asUInt.orR
  val csrReadOnly = csrAddr(11, 10) === 3.U
  // RW/RWI (csrOp[1:0] = 01) sempre escrevem; RS/RC so se rs1/zimm != 0.
  val csrWrites = idEx.signals.csrOp(1, 0) === 1.U || idEx.rs1 =/= 0.U
  val csrOpValid = idEx.valid && !idEx.signals.illegal && idEx.signals.csrWrite
  val csrPrivIllegal = csrOpValid &&
    (!csrKnown || priv < csrAddr(9, 8) || (csrReadOnly && csrWrites))
  val xretValid = idEx.valid && !idEx.signals.illegal
  val mretPrivIllegal = xretValid && idEx.instr === MRET && priv =/= 3.U
  val sretPrivIllegal = xretValid && idEx.instr === SRET &&
    (priv === 0.U || (priv === 1.U && csr.io.mstatus_read(22)))
  val exIllegal =
    idEx.signals.illegal || csrPrivIllegal || mretPrivIllegal || sretPrivIllegal

  // ============================================================
  // 5. INTERRUPÇÕES (sem fonte ligada)
  // ============================================================
  // Nao ha fonte de interrupcao externa/timer ligada ainda: o CLINT recebe
  // sempre `None`. Quando houver (mtime/mtimecmp ou PLIC), basta dirigir este
  // fio com o codigo correspondente (ex.: InterruptCode.Timer0).
  val interrupt_flag = WireDefault(InterruptCode.None)

  // ECALL/EBREAK/MRET/SRET sao resolvidos no EX (ver secao 9c): o CLINT
  // recebe PC e instrucao do EX e `valid` fica alto por UM unico ciclo, o
  // ciclo em que a trap/retorno e tomado (ele dispara a escrita nos CSRs).
  val sysInstrInEx = idEx.valid && !exIllegal
  val exIsEcallEbreak =
    sysInstrInEx && (idEx.instr === ECALL || idEx.instr === EBREAK)
  val exIsXret = sysInstrInEx && (idEx.instr === MRET || idEx.instr === SRET)
  val takeSys = (exIsEcallEbreak || exIsXret) && !cacheWait
  clint.io.pc := idEx.pc
  clint.io.instr := idEx.instr
  clint.io.valid := takeSys
  clint.io.interrupt_flag := interrupt_flag

  // ============================================================
  // 6. CONEXÃO CLINT -> CSR (escrita com prioridade)
  // ============================================================
  csr.io.clint_write_enable := clint.io.direct_write_enable
  csr.io.clint_mstatus_write := clint.io.mstatus_write_data
  csr.io.clint_priv_write := clint.io.priv_write
  csr.io.clint_mepc_write := clint.io.mepc_write_data
  csr.io.clint_mcause_write := clint.io.mcause_write_data
  csr.io.clint_mtval_write := clint.io.mtval_write_data
  csr.io.clint_sepc_write := clint.io.sepc_write_data
  csr.io.clint_scause_write := clint.io.scause_write_data
  csr.io.clint_stval_write := clint.io.stval_write_data
  csr.io.clint_sstatus_write := clint.io.sstatus_write_data

  // ============================================================
  // 7. ESTÁGIO IF: busca de instrução pela ICache
  // ============================================================
  val fenceIRequest =
    idEx.valid && !exIllegal && idEx.signals.isFenceI
  l1Cache.io.fenceI := fenceIRequest

  // O InstructionFetch decide qual palavra de 32 bits pedir a ICache (buffer de
  // palavra, RVC e instrucoes que cruzam o limite de palavra). Ele continua
  // pedindo e guardando palavras mesmo com o pipeline parado por outra cache:
  // a palavra fica no buffer e o fetch nao e repetido. Durante FENCE.I nao ha
  // pedido a ICache (ela esta sendo invalidada).
  fetch.io.pc := pcReg
  fetch.io.enable := !fenceIRequest && !fetchHalted
  fetch.io.memoryFault := l1Cache.io.iCache.fault
  fetch.io.flush := flushPipeline
  fetch.io.memoryReady := l1Cache.io.iCache.hit
  fetch.io.memoryData := l1Cache.io.iCache.readData
  l1Cache.io.iCache.request := fetch.io.memoryReadEnable
  l1Cache.io.iCache.write := false.B
  l1Cache.io.iCache.address := fetch.io.memoryAddress
  l1Cache.io.iCache.writeData := 0.U
  l1Cache.io.iCache.memSize := MemorySize.WORD
  l1Cache.io.iCache.unsignedLoad := false.B
  val fetchWait = fetch.io.memoryReadEnable && !l1Cache.io.iCache.hit
  val fenceIWait = fenceIRequest && !l1Cache.io.fenceIDone

  // ============================================================
  // 8. ESTÁGIO ID: decodificação e leitura do banco de registradores
  // ============================================================
  val idRs1 = ifIdInstr(19, 15)
  val idRs2 = ifIdInstr(24, 20)

  immGen.io.instr := ifIdInstr
  controller.io.opcode := ifIdInstr(6, 0)
  controller.io.funct3 := ifIdInstr(14, 12)
  controller.io.funct7 := ifIdInstr(31, 25)
  controller.io.rs2 := ifIdInstr(24, 20)

  regFile.io.rs1 := idRs1
  regFile.io.rs2 := idRs2

  // CSR: endereço lido pelo ID
  val csrAddressID = ifIdInstr(31, 20)
  csr.io.read_address := csrAddressID

  // ============================================================
  // 9. ESTÁGIO EX: ULA, branches, CSR forwarding e writeback data
  // ============================================================
  val exOperandA = WireDefault(idEx.rs1Value)
  val exOperandB = WireDefault(idEx.rs2Value)
  switch(idEx.signals.operandASel) {
    is(OperandASel.PC) { exOperandA := idEx.pc }
    is(OperandASel.ZERO) { exOperandA := 0.U }
  }
  when(idEx.signals.operandBSel === OperandBSel.IMM) {
    exOperandB := idEx.imm
  }

  ula.io.a := exOperandA
  ula.io.b := exOperandB
  ula.io.op := idEx.signals.aluOp

  // Endereços de desvio
  val jalrTarget = ula.io.result & "hFFFFFFFE".U(32.W)
  val pcRelativeTarget = (idEx.pc.asSInt + idEx.imm.asSInt).asUInt

  val branchTaken = WireDefault(false.B)
  switch(idEx.signals.branchType) {
    is(BranchType.BEQ) { branchTaken := idEx.rs1Value === idEx.rs2Value }
    is(BranchType.BNE) { branchTaken := idEx.rs1Value =/= idEx.rs2Value }
    is(BranchType.BLT) {
      branchTaken := idEx.rs1Value.asSInt < idEx.rs2Value.asSInt
    }
    is(BranchType.BGE) {
      branchTaken := idEx.rs1Value.asSInt >= idEx.rs2Value.asSInt
    }
    is(BranchType.BLTU) { branchTaken := idEx.rs1Value < idEx.rs2Value }
    is(BranchType.BGEU) { branchTaken := idEx.rs1Value >= idEx.rs2Value }
  }

  val controlRedirect = idEx.valid && !exIllegal &&
    (idEx.signals.jump || (idEx.signals.branchType =/= BranchType.NONE && branchTaken))
  val redirectTarget = Mux(idEx.signals.jalr, jalrTarget, pcRelativeTarget)

  // ============================================================
  // 9b. TRAP DE ACESSO DESALINHADO (detectado no EX, ANTES da DCache)
  // ============================================================
  // Politica: sem suporte por hardware. Acessos desalinhados geram excecao
  // sincrona precisa (a instrucao nao tem efeito arquitetural):
  //   - load / LR.W                 -> mcause 4 (load address misaligned)
  //   - store / SC.W / AMO          -> mcause 6 (store/AMO address misaligned)
  //   - mtval = endereco efetivo que falhou; mepc = PC da instrucao.
  // LR/SC/AMO sao sempre de palavra (exigem alinhamento natural de 4 bytes);
  // SC desalinhado gera trap mesmo sem reserva (a checagem vem antes).
  val exIsLR = idEx.valid && !exIllegal && idEx.signals.isLR
  val exIsSC = idEx.valid && !exIllegal && idEx.signals.isSC
  val exIsAMO = idEx.valid && !exIllegal && idEx.signals.isAMO
  val exIsAtomic = exIsLR || exIsSC || exIsAMO
  val exIsStore = exIsSC || exIsAMO || (idEx.signals.memWrite && !exIsAtomic)
  val exMemAccess = idEx.valid && !exIllegal &&
    (idEx.signals.memWrite || idEx.signals.writebackSel === WritebackSel.MEM || exIsAtomic)

  val exAccessSize = Mux(exIsAtomic, MemorySize.WORD, idEx.signals.memSize)
  val exAddrLow = idEx.memAddress(1, 0)
  val exMisaligned = MuxLookup(exAccessSize, false.B)(
    Seq(
      MemorySize.HALF -> exAddrLow(0),
      MemorySize.WORD -> (exAddrLow =/= 0.U)
    )
  )
  val misalignTrap = exMemAccess && exMisaligned

  // Outras excecoes sincronas detectadas na instrucao do EX:
  //   - instruction access fault (mcause 1): a busca desta instrucao foi
  //     recusada pela memoria (o marcador viaja desde o IF);
  //   - instrucao ilegal (mcause 2): mtval = encoding original;
  //   - load/store access fault (mcause 5/7): a DCache sinalizou `fault`
  //     (ligado na secao 14), mtval = endereco efetivo.
  val fetchFaultTrap = idEx.valid && idEx.fetchFault
  val illegalTrap = idEx.valid && exIllegal
  val dataFaultTrap = WireDefault(false.B)
  val excTrap = fetchFaultTrap || illegalTrap || misalignTrap || dataFaultTrap
  // Como o redirecionamento de desvios, a trap so e tomada quando nao ha
  // espera de cache (evita mudar pcReg no meio de um miss da ICache).
  val takeTrap = excTrap && !cacheWait

  clint.io.exc_valid := takeTrap
  // As condicoes sao exclusivas (instrucao ilegal ou marcador de falha nao
  // acessam a DCache); a ordem do Mux so documenta a prioridade.
  clint.io.exc_cause := Mux(
    fetchFaultTrap,
    1.U(31.W),
    Mux(
      illegalTrap,
      2.U(31.W),
      Mux(
        misalignTrap,
        Mux(exIsStore, 6.U(31.W), 4.U(31.W)),
        Mux(exIsStore, 7.U(31.W), 5.U(31.W))
      )
    )
  )
  clint.io.exc_pc := idEx.pc
  clint.io.exc_tval := Mux(
    fetchFaultTrap,
    idEx.fetchFaultAddr,
    Mux(illegalTrap, idEx.rawInstr, idEx.memAddress)
  )
  // Destino da trap: excecoes usam o endereco base de mtvec/stvec (bits de MODE
  // ignorados); MRET/SRET usam mepc/sepc (bit 0 ignorado, RVC permite PC par).
  val trapTarget = Mux(
    takeSys && exIsXret,
    clint.io.trap_address & "hFFFFFFFE".U(32.W),
    clint.io.trap_address & "hFFFFFFFC".U(32.W)
  )
  val takeAnyTrap = takeTrap || takeSys

  // ============================================================
  // 10. HAZARD DETECTION (load-use)
  // ============================================================
  val idOpcode = ifIdInstr(6, 0)
  val usesRs1 = idOpcode === Opcode.OP || idOpcode === Opcode.OP_IMM ||
    idOpcode === Opcode.LOAD || idOpcode === Opcode.STORE ||
    idOpcode === Opcode.BRANCH || idOpcode === Opcode.JALR ||
    idOpcode === Opcode.ATOMIC ||
    (idOpcode === Opcode.SYSTEM && controller.io.signals.csrOp =/= 0.U &&
      !controller.io.signals.csrOp(2))
  val usesRs2 = idOpcode === Opcode.OP || idOpcode === Opcode.STORE ||
    idOpcode === Opcode.BRANCH || idOpcode === Opcode.ATOMIC
  val currentUsesRd = (usesRs1 && idRs1 === idEx.rd && idRs1 =/= 0.U) ||
    (usesRs2 && idRs2 === idEx.rd && idRs2 =/= 0.U)
  val isLoadInEx = idEx.valid &&
    !excTrap &&
    !exIllegal &&
    idEx.signals.writebackSel === WritebackSel.MEM &&
    !idEx.signals.memWrite &&
    idEx.signals.regWrite &&
    idEx.signals.branchType === BranchType.NONE &&
    !idEx.signals.jump &&
    !idEx.signals.jalr

  val loadUseHazard = ifIdValid && !ifIdIllegal && isLoadInEx && currentUsesRd
  val hazardDetected = loadUseHazard
  hazardStall := hazardDetected && !cacheWait

  // ============================================================
  // 11. EXTENSÃO A: LR/SC e AMOs
  // ============================================================
  val isLR = idEx.valid && !exIllegal && idEx.signals.isLR
  val isSC = idEx.valid && !exIllegal && idEx.signals.isSC
  val isAMO = idEx.valid && !exIllegal && idEx.signals.isAMO
  val scHasReservation = reservation.io.hasReservation

  // Eventos de reserva (ver ReservationUnit para a prioridade entre eles):
  //   LR      -> estabelece a reserva
  //   SC      -> consome a reserva; so tem sucesso se ela cobre o endereco
  //   AMO     -> invalida a reserva
  //   escrita -> qualquer escrita no endereco reservado invalida a reserva
  //   ilegal  -> instrucao ilegal / trap invalida a reserva
  reservation.io.addr := idEx.memAddress
  reservation.io.lrCommit := isLR && exCanCommit
  reservation.io.scCommit := isSC && exCanCommit
  reservation.io.amoCommit := isAMO && exCanCommit
  // O proprio SC fica de fora: o hit da DCache dele pode chegar antes do commit
  // (ex.: durante um miss de ICache). Se ele apagasse a reserva aqui, o SC
  // gravaria na memoria mas `scSuccess` veria "sem reserva" e devolveria falha.
  // O SC consome a reserva no commit (`scCommit`).
  reservation.io.storeHit :=
    l1Cache.io.dCache.hit && idEx.signals.memWrite && !idEx.signals.isSC
  reservation.io.invalidate := io.illegal || exIllegal

  val scSuccess = isSC && exCanCommit && scHasReservation

  // ============================================================
  // 12. FORWARDING (registradores e CSR)
  // ============================================================
  // 12a. Writeback data (prioridade: lwWb > writebackSel > ULA)
  val writebackData = WireDefault(ula.io.result)
  switch(idEx.signals.writebackSel) {
    is(WritebackSel.MEM) { writebackData := cachedReadData }
    is(WritebackSel.PC4) { writebackData := idEx.pc + idEx.instrLength }
    is(WritebackSel.IMM) { writebackData := idEx.imm }
    is(WritebackSel.CSR) { writebackData := idEx.csrReadData }
  }
  when(isLR) { writebackData := cachedReadData }
  when(isSC) { writebackData := Mux(scSuccess, 0.U, 1.U) }
  when(isAMO) { writebackData := cachedReadData }
  when(lwWbValid) { writebackData := lwWbData }

  // 12b. Writeback enable/rd
  val writebackEnable =
    (idEx.valid && !exIllegal && idEx.signals.regWrite && exCanCommit &&
      !hazardStall) || lwWbValid
  val writebackRd = Mux(lwWbValid, lwWbRd, idEx.rd)

  // 12c. CSR forwarding
  val csrAddressEX = idEx.csrAddress
  val csrForwarding = idEx.valid && !exIllegal &&
    idEx.signals.csrWrite &&
    (csrAddressID === csrAddressEX)
  val csrReadData = Mux(csrForwarding, idEx.csrWriteData, csr.io.read_data)

  // 12d. Forwarding de registradores (EX -> ID)
  val forwardedRs1 = Mux(
    writebackEnable && writebackRd =/= 0.U && writebackRd === idRs1,
    writebackData,
    regFile.io.readData1
  )
  val forwardedRs2 = Mux(
    writebackEnable && writebackRd =/= 0.U && writebackRd === idRs2,
    writebackData,
    regFile.io.readData2
  )

  // Endereço de memória calculado no ID
  val decodedMemAddress = (forwardedRs1.asSInt + immGen.io.imm.asSInt).asUInt

  // ============================================================
  // 13. CSR: dados a serem escritos e conexão CPU -> CSR
  // ============================================================
  val csrSrc = Mux(
    controller.io.signals.csrOp(2),
    ifIdInstr(19, 15),
    forwardedRs1
  )

  val csrWriteData = WireDefault(0.U(32.W))
  switch(controller.io.signals.csrOp) {
    is(1.U) { csrWriteData := csrSrc }
    is(2.U) { csrWriteData := csrReadData | csrSrc }
    is(3.U) { csrWriteData := csrReadData & ~csrSrc }
    is(5.U) { csrWriteData := csrSrc }
    is(6.U) { csrWriteData := csrReadData | csrSrc }
    is(7.U) { csrWriteData := csrReadData & ~csrSrc }
  }

  csr.io.cpu_write_enable := idEx.valid && !exIllegal && exCanCommit &&
    idEx.signals.csrWrite &&
    !clint.io.direct_write_enable
  csr.io.cpu_write_address := idEx.csrAddress
  csr.io.cpu_write_data := idEx.csrWriteData

  // ============================================================
  // 14. ACESSO À DCache E HANDSHAKE DAS CACHES
  // ============================================================
  val dataAccess = idEx.valid && !exIllegal &&
    (idEx.signals.memWrite || idEx.signals.writebackSel === WritebackSel.MEM) &&
    (!isSC || scHasReservation) &&
    !misalignTrap
  val dataCacheRequest = dataAccess && !dataAccessDone

  l1Cache.io.dCache.request := dataCacheRequest
  l1Cache.io.dCache.write := idEx.signals.memWrite && (!isSC || scHasReservation)
  l1Cache.io.dCache.address := idEx.memAddress
  l1Cache.io.dCache.memSize := idEx.signals.memSize
  l1Cache.io.dCache.unsignedLoad := idEx.signals.memUnsigned

  cachedReadData := Mux(
    dataAccessDone,
    dataAccessData,
    l1Cache.io.dCache.readData
  )

  amoAlu.io.op := idEx.signals.amoOp
  amoAlu.io.memData := cachedReadData
  amoAlu.io.operand := idEx.memWriteData
  val amoWriteData = Mux(isAMO, amoAlu.io.result, idEx.memWriteData)
  l1Cache.io.dCache.writeData := amoWriteData

  // `fault` encerra a espera: a trap de access fault e tomada no lugar.
  dataFaultTrap := dataCacheRequest && l1Cache.io.dCache.fault
  val dataWait =
    dataCacheRequest && !l1Cache.io.dCache.hit && !l1Cache.io.dCache.fault
  cacheWait := fetchWait || dataWait || fenceIWait
  fenceIComplete := fenceIRequest && l1Cache.io.fenceIDone
  val takeRedirect = controlRedirect && !cacheWait && !hazardStall
  stallPipeline := cacheWait || hazardStall
  flushPipeline := takeRedirect || fenceIComplete || takeAnyTrap
  exCanCommit := !cacheWait && !excTrap

  // ============================================================
  // 15. CONEXÃO AO BANCO DE REGISTRADORES (writeback)
  // ============================================================
  regFile.io.rd := idEx.rd
  regFile.io.writeData := writebackData
  regFile.io.regWrite := writebackEnable

  // ============================================================
  // 16. ATUALIZAÇÃO DOS REGISTRADORES DE PIPELINE
  // ============================================================
  val pcNext = WireDefault(pcReg)
  when(cacheWait || hazardStall) {
    pcNext := pcReg
  }.elsewhen(fenceIComplete) {
    // IF/ID pode conter uma instrução buscada antes da sincronização. Refaz o
    // fetch a partir dela (se valida), agora que linhas dirty foram gravadas e
    // a ICache foi invalidada.
    pcNext := Mux(ifIdValid, ifIdPc, pcReg)
  }.elsewhen(takeAnyTrap) {
    pcNext := trapTarget
  }.elsewhen(takeRedirect) {
    pcNext := redirectTarget
  }.elsewhen(fetch.io.valid) {
    pcNext := pcReg + fetch.io.length
  }

  pcReg := pcNext

  when(cacheWait) {
    // Miss de cache: congela o pipeline. A palavra de instrucao que chegar
    // fica no buffer do InstructionFetch; o store/load ja atendido da DCache
    // e guardado para nao ser repetido.
    lwWbValid := false.B
    when(dataCacheRequest && l1Cache.io.dCache.hit) {
      dataAccessDone := true.B
      dataAccessData := l1Cache.io.dCache.readData
    }
  }.elsewhen(hazardStall) {
    // Load-use: a leitura ja foi atendida; grava o valor e injeta uma bolha.
    lwWbValid := true.B
    lwWbRd := idEx.rd
    lwWbData := cachedReadData
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
    dataAccessDone := false.B
  }.elsewhen(fenceIComplete) {
    lwWbValid := false.B
    ifIdValid := false.B
    ifIdIllegal := false.B
    ifIdFault := false.B
    ifIdInstr := nop
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
    dataAccessDone := false.B
  }.elsewhen(takeRedirect || takeAnyTrap) {
    // FLUSH (desvio/trap tomados): insere NOP no IF/ID e limpa o ID/EX
    lwWbValid := false.B
    ifIdValid := false.B
    ifIdIllegal := false.B
    ifIdFault := false.B
    ifIdInstr := nop
    idEx := 0.U.asTypeOf(new DecodeExecuteBundle)
    dataAccessDone := false.B
  }.otherwise {
    // ATUALIZAÇÃO NORMAL
    lwWbValid := false.B
    ifIdPc := pcReg
    ifIdInstr := fetch.io.instr
    ifIdValid := fetch.io.valid
    ifIdLength := fetch.io.length
    ifIdIllegal := fetch.io.illegal
    ifIdFault := fetch.io.fault
    ifIdFaultAddr := fetch.io.faultAddress
    ifIdRaw := fetch.io.rawInstr
    when(fetch.io.fault) { fetchHalted := true.B }
    dataAccessDone := false.B

    idEx.valid := ifIdValid
    idEx.pc := ifIdPc
    idEx.instr := ifIdInstr
    idEx.instrLength := ifIdLength
    idEx.rs1 := idRs1
    idEx.rs2 := idRs2
    idEx.rd := ifIdInstr(11, 7)
    idEx.rs1Value := forwardedRs1
    idEx.rs2Value := forwardedRs2
    idEx.imm := immGen.io.imm
    idEx.memAddress := decodedMemAddress
    idEx.memWriteData := forwardedRs2
    idEx.signals := controller.io.signals
    idEx.signals.illegal := controller.io.signals.illegal || ifIdIllegal
    idEx.csrAddress := ifIdInstr(31, 20)
    idEx.csrWriteData := csrWriteData
    idEx.csrReadData := csrReadData
    idEx.fetchFault := ifIdFault
    idEx.fetchFaultAddr := ifIdFaultAddr
    idEx.rawInstr := ifIdRaw
  }

  // O flush (trap, desvio ou FENCE.I) reabilita a busca.
  when(flushPipeline) { fetchHalted := false.B }

  // ============================================================
  // 17. SAÍDAS
  // ============================================================
  io.pc := pcReg
  io.instr := ifIdInstr
  io.aluResult := ula.io.result
  io.writebackData := writebackData
  io.writebackRd := writebackRd
  io.writebackEnable := writebackEnable
  io.illegal := idEx.valid && exIllegal
  io.stalled := stallPipeline

  // Debug do CLINT
  io.debug_clint_trap_assert := clint.io.debug_trap_assert
  io.debug_clint_trap_to_smode := clint.io.debug_trap_to_smode
  io.debug_clint_delegate_to_smode := clint.io.debug_delegate_to_smode
  io.debug_clint_exception_code := clint.io.debug_exception_code
  io.debug_clint_is_interrupt := clint.io.debug_is_interrupt
  io.debug_clint_trap_address := clint.io.debug_trap_address

  // Debug do CSR forwarding
  io.debug_csrReadData := csrReadData
  io.debug_csrForwarding := csrForwarding
  io.debug_csr_address_id := csrAddressID
  io.debug_csr_address_ex := csrAddressEX
}
