package riscv.integration.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.cache.CacheConfig
import riscv.decode.RV32I
import riscv.pipeline.Pipeline3
import riscv.privileged.CLINT
import scala.collection.mutable

/** Checagem de privilegio de CSR e de MRET/SRET (instrucao ilegal, mcause 2) e
  * transicoes de privilegio (MRET entra em U/S/M conforme MPP; trap volta a M).
  */
class Pipeline3PrivilegeSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 (privilegio)"

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def lui(rd: Int, imm20: Int): Long =
    (imm20.toLong << 12) | (rd.toLong << 7) | 0x37L
  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x1L << 12) | (rd.toLong << 7) | 0x73L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private val MRET = 0x30200073L
  private val SRET = 0x10200073L
  private val NOP = 0x00000013L
  private val jal0 = 0x0000006fL // jal x0, 0

  private val UserPc = 0x30
  private val U = 0; private val S = 1; private val M = 3

  /** M-mode: mtvec = 0x40; mstatus.MPP = mpp; mepc = 0x30; MRET (entra em mpp).
    * Em 0x30 fica a instrucao testada; em 0x34, `x4 = 1` (so executa sem trap).
    * O handler (0x40, em M) le mcause, mtval, mepc e mstatus em x5, x7, x8, x9.
    */
  private def program(mpp: Int, tested: Long): Seq[Long] = {
    val v = mpp << 11
    val hi = (v + 0x800) >> 12
    val lo = v - (hi << 12)
    Seq(
      addi(1, 0, 0x40), // 0x00
      csrrw(0, 0x305, 1), // 0x04 mtvec = 0x40
      addi(2, 0, UserPc), // 0x08
      csrrw(0, 0x341, 2), // 0x0C mepc = 0x30
      lui(3, hi), // 0x10
      addi(3, 3, lo), // 0x14
      csrrw(0, 0x300, 3), // 0x18 mstatus = MPP << 11
      MRET, // 0x1C entra em `mpp` no PC 0x30
      jal0, // 0x20
      NOP,
      NOP,
      NOP, // 0x24..0x2C
      tested, // 0x30
      addi(4, 0, 1), // 0x34
      jal0, // 0x38
      NOP, // 0x3C
      csrrs(5, 0x342, 0), // 0x40 x5 = mcause
      csrrs(7, 0x343, 0), // 0x44 x7 = mtval
      csrrs(8, 0x341, 0), // 0x48 x8 = mepc
      csrrs(9, 0x300, 0), // 0x4C x9 = mstatus
      jal0 // 0x50
    )
  }

  private val smallCache =
    CacheConfig(capacityBytes = 1024, ways = 2, lineBytes = 16)

  private def run(prog: Seq[Long]): Map[Int, BigInt] = {
    val writes = mutable.Map.empty[Int, BigInt]
    test(
      new Pipeline3(
        initialProgram = prog,
        memoryWords = 64,
        cacheConfig = smallCache
      )
    ) { dut =>
      for (_ <- 0 until 220) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          if (rd != 0 && !writes.contains(rd))
            writes += rd -> dut.io.writebackData.peekInt()
        }
        dut.clock.step(1)
      }
    }
    writes.toMap
  }

  /** Trap de instrucao ilegal vinda do privilegio `from` (MPP salvo = from). */
  private def expectIllegal(mpp: Int, tested: Long): Unit = {
    val w = run(program(mpp, tested))
    assert(w.get(5).contains(BigInt(2)), s"mcause esperado 2: ${w.get(5)}")
    assert(
      w.get(7).contains(BigInt(tested)),
      s"mtval esperado 0x${tested.toHexString}: ${w.get(7)}"
    )
    assert(
      w.get(8).contains(BigInt(UserPc)),
      s"mepc esperado 0x30: ${w.get(8)}"
    )
    assert(
      w.get(9).exists(x => ((x >> 11) & 3) == BigInt(mpp)),
      s"mstatus.MPP deveria guardar o privilegio $mpp: ${w.get(9)}"
    )
    assert(!w.contains(4), "a instrucao apos a trap nao deveria executar")
    assert(!w.contains(6), "a instrucao ilegal nao pode escrever rd")
  }

  private def expectLegal(mpp: Int, tested: Long): Unit = {
    val w = run(program(mpp, tested))
    assert(w.contains(6), "a instrucao deveria executar e escrever rd")
    assert(w.contains(4), "o fluxo normal deveria continuar")
    assert(!w.contains(5), "o handler nao deveria ter sido executado")
  }

  it should "bloquear CSR de M e de S em U-mode (e o handler, em M, consegue ler CSRs)" in {
    expectIllegal(U, csrrs(6, 0x300, 0)) // mstatus (M) lido em U
    expectIllegal(U, csrrw(0, 0x305, 0)) // mtvec escrito em U
    expectIllegal(U, csrrs(6, 0x105, 0)) // stvec (S) lido em U
  }

  it should "bloquear CSR de M em S-mode e permitir CSR de S" in {
    expectIllegal(S, csrrs(6, 0x300, 0))
    expectLegal(S, csrrs(6, 0x105, 0)) // stvec em S-mode
  }

  it should "bloquear MRET em U e S, e SRET em U" in {
    expectIllegal(U, MRET)
    expectIllegal(S, MRET)
    expectIllegal(U, SRET)
  }

  it should "bloquear escrita em CSR somente leitura, mas permitir a leitura" in {
    expectIllegal(M, csrrw(0, 0xf14, 1)) // mhartid <- x1
    expectIllegal(M, csrrs(6, 0xf14, 1)) // CSRRS com rs1 != 0 escreve
    expectLegal(M, csrrs(6, 0xf14, 0)) // CSRRS com rs1 = 0 so le
  }

  it should "tratar CSR inexistente como instrucao ilegal em M-mode" in {
    expectIllegal(M, csrrs(6, 0x7c0, 0))
  }

  it should "permitir CSR de M em M-mode" in {
    expectLegal(M, csrrs(6, 0x300, 0))
  }
}

class CLINTPrivilegeSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CLINT (novo privilegio)"

  it should "voltar a M em trap, ir a S quando delegada, e restaurar MPP/SPP em MRET/SRET" in {
    test(new CLINT) { dut =>
      // Excecao vinda de U sem delegacao: o handler roda em M.
      dut.io.current_priv.poke(0.U)
      dut.io.exc_valid.poke(true.B)
      dut.io.exc_cause.poke(2.U)
      dut.io.mtvec.poke(0x40.U)
      dut.clock.step(0)
      dut.io.priv_write.expect(3.U)

      // Com medeleg[2] = 1, a mesma excecao vai para S.
      dut.io.medeleg.poke(4.U)
      dut.io.stvec.poke(0x80.U)
      dut.clock.step(0)
      dut.io.priv_write.expect(1.U)
      dut.io.exc_valid.poke(false.B)
      dut.io.medeleg.poke(0.U)

      // MRET volta ao privilegio salvo em MPP.
      dut.io.current_priv.poke(3.U)
      dut.io.valid.poke(true.B)
      dut.io.instr.poke(RV32I.MRET)
      dut.io.mstatus.poke((1 << 11).U) // MPP = S
      dut.clock.step(0)
      dut.io.priv_write.expect(1.U)
      dut.io.mstatus.poke(0.U) // MPP = U
      dut.clock.step(0)
      dut.io.priv_write.expect(0.U)

      // SRET volta a S ou U conforme SPP.
      dut.io.current_priv.poke(1.U)
      dut.io.instr.poke(RV32I.SRET)
      dut.io.sstatus.poke((1 << 8).U) // SPP = S
      dut.clock.step(0)
      dut.io.priv_write.expect(1.U)
      dut.io.sstatus.poke(0.U) // SPP = U
      dut.clock.step(0)
      dut.io.priv_write.expect(0.U)
    }
  }
}
