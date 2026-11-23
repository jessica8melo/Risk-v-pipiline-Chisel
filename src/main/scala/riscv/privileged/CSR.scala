package riscv.privileged

import chisel3._
import chisel3.util._
import riscv.cache.CachePerf
import riscv.decode.RV32I

/** Contadores de desempenho visiveis como CSRs.
  *
  * Cada contador de 64 bits aparece em 4 enderecos de 32 bits: `mXXX` (0xB00 +
  * n, M), `mXXXh` (0xB80 + n, M), `XXX` (0xC00 + n, leitura em S/U se
  * `mcounteren[n]`) e `XXXh` (0xC80 + n).
  *
  * n = 0 mcycle, n = 2 minstret, n = 3..12 contadores fixos das caches
  * (mhpmcounterN), n = 1 (time) nao existe:
  *
  * {{{
  *  3 ICache hits         4 ICache misses        5 ICache miss cycles
  *  6 DCache hits         7 DCache misses        8 DCache miss cycles
  *  9 DCache write-backs 10 DCache flush WBs    11 ICache faults
  * 12 DCache faults
  * }}}
  */
object CsrCounters {
  val hpmNames: Seq[String] = Seq(
    "icache.hits",
    "icache.misses",
    "icache.missCycles",
    "dcache.hits",
    "dcache.misses",
    "dcache.missCycles",
    "dcache.writeBacks",
    "dcache.flushWriteBacks",
    "icache.faults",
    "dcache.faults"
  )
  val firstHpm = 3
  val numbers: Seq[Int] = Seq(0, 2) ++ (firstHpm until firstHpm + hpmNames.size)

  /** Todos os enderecos de CSR de contador (M e usuario, baixo e alto). */
  val addresses: Seq[Int] =
    numbers.flatMap(n => Seq(0xb00 + n, 0xb80 + n, 0xc00 + n, 0xc80 + n))
}

class CSR extends Module {
  val io = IO(new Bundle {
    // ============================================================
    // LEITURA
    // ============================================================
    val read_address = Input(UInt(12.W))
    val read_data = Output(UInt(32.W))

    // Leituras específicas para o CLINT (M-Mode)
    val mstatus_read = Output(UInt(32.W))
    val mepc_read = Output(UInt(32.W))
    val mcause_read = Output(UInt(32.W))
    val mtvec_read = Output(UInt(32.W))
    val mie_read = Output(UInt(32.W))
    val medeleg_read = Output(UInt(32.W))
    val mideleg_read = Output(UInt(32.W))
    val mtval_read = Output(UInt(32.W)) // NOVO

    // Leituras específicas para o CLINT (S-Mode)
    val sstatus_read = Output(UInt(32.W))
    val sepc_read = Output(UInt(32.W))
    val scause_read = Output(UInt(32.W))
    val stvec_read = Output(UInt(32.W))
    val sie_read = Output(UInt(32.W))
    val stval_read = Output(UInt(32.W)) // NOVO
    val satp_read = Output(UInt(32.W))

    // ============================================================
    // ESCRITA VIA CPU
    // ============================================================
    val cpu_write_address = Input(UInt(12.W))
    val cpu_write_data = Input(UInt(32.W))
    val cpu_write_enable = Input(Bool())

    // ============================================================
    // ESCRITA VIA CLINT (prioridade)
    // ============================================================
    val clint_write_enable = Input(Bool())
    val clint_mstatus_write = Input(UInt(32.W))
    val clint_priv_write = Input(UInt(2.W)) // novo privilegio
    val clint_mepc_write = Input(UInt(32.W))
    val clint_mcause_write = Input(UInt(32.W))
    val clint_mtval_write = Input(UInt(32.W)) // NOVO
    val clint_sstatus_write = Input(UInt(32.W))
    val clint_sepc_write = Input(UInt(32.W))
    val clint_scause_write = Input(UInt(32.W))
    val clint_stval_write = Input(UInt(32.W))

    // ============================================================
    // MODO DE PRIVILÉGIO
    // ============================================================
    val current_priv = Output(UInt(2.W))

    // ============================================================
    // CONTADORES DE DESEMPENHO
    // ============================================================
    // Pulso: uma instrucao do EX conclui neste ciclo (minstret).
    val retire = Input(Bool())
    val iPerf = Input(new CachePerf)
    val dPerf = Input(new CachePerf)
    val mcounteren_read = Output(UInt(32.W))
  })

  import RV32I._

  // ============================================================
  // REGISTRADORES CSR
  // ============================================================
  // M-Mode
  val mstatus = RegInit(0.U(32.W))
  val mie = RegInit(0.U(32.W))
  val mtvec = RegInit(0.U(32.W))
  val mscratch = RegInit(0.U(32.W))
  val mepc = RegInit(0.U(32.W))
  val mcause = RegInit(0.U(32.W))
  val mtval = RegInit(0.U(32.W))
  val medeleg = RegInit(0.U(32.W))
  val mideleg = RegInit(0.U(32.W))

  // S-Mode
  val sstatus = RegInit(0.U(32.W))
  val sie = RegInit(0.U(32.W))
  val stvec = RegInit(0.U(32.W))
  val sscratch = RegInit(0.U(32.W))
  val sepc = RegInit(0.U(32.W))
  val scause = RegInit(0.U(32.W))
  val stval = RegInit(0.U(32.W))
  val satp = RegInit(0.U(32.W))

  // Modo de privilégio
  val current_priv = RegInit(3.U(2.W)) // Começa em M-mode
  io.current_priv := current_priv

  // Contadores de 64 bits. mcycle conta todo ciclo; minstret conta cada
  // instrucao concluida. A escrita por CSR tem prioridade sobre a contagem.
  val mcounteren = RegInit(0.U(32.W))
  val mcycle = RegInit(0.U(64.W))
  val minstret = RegInit(0.U(64.W))
  mcycle := mcycle + 1.U
  minstret := minstret + io.retire.asUInt
  io.mcounteren_read := mcounteren

  // Valores lidos. Como o CSR e lido no ID, a instrucao que esta no EX e
  // conclui neste ciclo ainda nao entrou em minstret: soma-se `retire` para
  // que o valor lido conte todas as instrucoes anteriores.
  val instretRead = minstret + io.retire.asUInt
  private val hpmValues: Seq[UInt] = Seq(
    io.iPerf.hits,
    io.iPerf.misses,
    io.iPerf.missCycles,
    io.dPerf.hits,
    io.dPerf.misses,
    io.dPerf.missCycles,
    io.dPerf.writeBacks,
    io.dPerf.flushWriteBacks,
    io.iPerf.faults,
    io.dPerf.faults
  )
  private val counterValues: Seq[(Int, UInt)] =
    Seq(0 -> mcycle, 2 -> instretRead) ++
      hpmValues.zipWithIndex.map { case (v, k) =>
        (CsrCounters.firstHpm + k) -> v
      }
  private val counterReads: Seq[(UInt, UInt)] = counterValues.flatMap {
    case (n, v) =>
      Seq(
        (0xb00 + n).U(12.W) -> v(31, 0),
        (0xb80 + n).U(12.W) -> v(63, 32),
        (0xc00 + n).U(12.W) -> v(31, 0),
        (0xc80 + n).U(12.W) -> v(63, 32)
      )
  }

  // ============================================================
  // LEITURA (MuxLookup)
  // ============================================================
  io.read_data := MuxLookup(io.read_address, 0.U(32.W))(
    Seq(
      // M-Mode
      CSRAddress.mstatus -> mstatus,
      CSRAddress.mie -> mie,
      CSRAddress.mtvec -> mtvec,
      CSRAddress.mscratch -> mscratch,
      CSRAddress.mepc -> mepc,
      CSRAddress.mcause -> mcause,
      CSRAddress.mtval -> mtval,
      CSRAddress.medeleg -> medeleg,
      CSRAddress.mideleg -> mideleg,
      // S-Mode
      CSRAddress.sstatus -> sstatus,
      CSRAddress.sie -> sie,
      CSRAddress.stvec -> stvec,
      CSRAddress.sscratch -> sscratch,
      CSRAddress.sepc -> sepc,
      CSRAddress.scause -> scause,
      CSRAddress.stval -> stval,
      CSRAddress.satp -> satp,
      // Read-only
      CSRAddress.mvendorid -> 0.U(32.W),
      CSRAddress.marchid -> 0.U(32.W),
      CSRAddress.mimpid -> 0.U(32.W),
      CSRAddress.mhartid -> 0.U(32.W),
      CSRAddress.mcounteren -> mcounteren
    ) ++ counterReads
  )

  // ============================================================
  // LEITURAS ESPECÍFICAS (para o CLINT)
  // ============================================================
  // M-Mode
  io.mstatus_read := mstatus
  io.mepc_read := mepc
  io.mcause_read := mcause
  io.mtvec_read := mtvec
  io.mie_read := mie
  io.medeleg_read := medeleg
  io.mideleg_read := mideleg
  io.mtval_read := mtval

  // S-Mode (CORRIGIDO: agora todas as saídas são atribuídas)
  io.sstatus_read := sstatus
  io.sepc_read := sepc
  io.scause_read := scause
  io.stvec_read := stvec
  io.sie_read := sie
  io.stval_read := stval
  io.satp_read := satp

  // ============================================================
  // ESCRITA COM PRIORIDADE: CLINT > CPU
  // ============================================================
  when(io.clint_write_enable) {
    // CLINT escreve nos CSRs de trap
    mstatus := io.clint_mstatus_write
    mepc := io.clint_mepc_write
    mcause := io.clint_mcause_write
    mtval := io.clint_mtval_write // NOVO
    sstatus := io.clint_sstatus_write
    sepc := io.clint_sepc_write
    scause := io.clint_scause_write
    stval := io.clint_stval_write

    // Novo privilégio calculado pelo CLINT (trap: M/S; MRET: MPP; SRET: SPP)
    current_priv := io.clint_priv_write

  }.elsewhen(io.cpu_write_enable) {
    switch(io.cpu_write_address) {
      // M-Mode
      is(CSRAddress.mstatus) { mstatus := io.cpu_write_data }
      is(CSRAddress.mie) { mie := io.cpu_write_data }
      is(CSRAddress.mtvec) { mtvec := io.cpu_write_data }
      is(CSRAddress.mscratch) { mscratch := io.cpu_write_data }
      is(CSRAddress.mepc) { mepc := io.cpu_write_data }
      is(CSRAddress.mcause) { mcause := io.cpu_write_data }
      is(CSRAddress.mtval) { mtval := io.cpu_write_data }
      is(CSRAddress.medeleg) { medeleg := io.cpu_write_data }
      is(CSRAddress.mideleg) { mideleg := io.cpu_write_data }

      // S-Mode
      is(CSRAddress.sstatus) { sstatus := io.cpu_write_data }
      is(CSRAddress.sie) { sie := io.cpu_write_data }
      is(CSRAddress.stvec) { stvec := io.cpu_write_data }
      is(CSRAddress.sscratch) { sscratch := io.cpu_write_data }
      is(CSRAddress.sepc) { sepc := io.cpu_write_data }
      is(CSRAddress.scause) { scause := io.cpu_write_data }
      is(CSRAddress.stval) { stval := io.cpu_write_data }
      is(CSRAddress.satp) { satp := io.cpu_write_data }
      is(CSRAddress.mcounteren) { mcounteren := io.cpu_write_data }
      // mcycle/minstret sao graváveis (metades baixa e alta separadas).
      // Os contadores hpm das caches sao somente leitura: escrita ignorada.
      is(CSRAddress.mcycle) { mcycle := Cat(mcycle(63, 32), io.cpu_write_data) }
      is(CSRAddress.mcycleh) { mcycle := Cat(io.cpu_write_data, mcycle(31, 0)) }
      is(CSRAddress.minstret) {
        minstret := Cat(minstret(63, 32), io.cpu_write_data)
      }
      is(CSRAddress.minstreth) {
        minstret := Cat(io.cpu_write_data, minstret(31, 0))
      }
    }
  }
}
