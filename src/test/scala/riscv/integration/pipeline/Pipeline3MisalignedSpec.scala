package riscv.integration.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.pipeline.Pipeline3

/** Politica de acesso desalinhado: trap precisa no estagio EX, antes da DCache.
  *   - load / LR.W -> mcause 4
  *   - store / SC.W / AMO -> mcause 6
  *   - mtval = endereco efetivo, mepc = PC da instrucao
  */
class Pipeline3MisalignedSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 (acesso desalinhado)"

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x1L << 12) | (rd.toLong << 7) | 0x73L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private def load(f3: Int, rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (f3.toLong << 12) | (rd.toLong << 7) | 0x03L
  private def store(f3: Int, rs2: Int, rs1: Int, imm: Int): Long =
    (((imm >> 5) & 0x7f).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (f3.toLong << 12) | ((imm & 0x1f).toLong << 7) | 0x23L
  private def atomic(funct5: Int, rd: Int, rs1: Int, rs2: Int): Long =
    (funct5.toLong << 27) | (rs2.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) |
      (rd.toLong << 7) | 0x2fL
  private val NOP = 0x00000013L
  private val jal0 = 0x0000006fL // jal x0, 0

  private val LB = 0; private val LH = 1; private val LW = 2
  private val SB = 0; private val SH = 1; private val SW = 2

  /** Monta: mtvec=0x40; x2=addr; x6=0x55; [instrucao testada em 0x14]; x4=1
    * (nao deve executar); handler em 0x40 le mcause/mtval/mepc em x5/x7/x8.
    */
  private def program(addr: Int, tested: Long): Seq[Long] = {
    val main = Seq(
      addi(1, 0, 0x40), // 0x00
      csrrw(0, 0x305, 1), // 0x04 mtvec = 0x40
      addi(2, 0, addr), // 0x08
      addi(6, 0, 0x55), // 0x0C
      NOP, // 0x10
      tested, // 0x14
      addi(4, 0, 1), // 0x18 apos o trap NAO pode ser executada
      jal0, // 0x1C laco infinito: sem trap, o fluxo nao pode cair no handler
      NOP,
      NOP,
      NOP,
      NOP,
      NOP,
      NOP,
      NOP,
      NOP, // 0x20..0x3C
      csrrs(5, 0x342, 0), // 0x40 x5 = mcause
      csrrs(7, 0x343, 0), // 0x44 x7 = mtval
      csrrs(8, 0x341, 0), // 0x48 x8 = mepc
      NOP
    )
    main
  }

  private case class Result(writes: Map[Int, BigInt])

  private def run(prog: Seq[Long], cycles: Int = 80): Result = {
    var writes = Map.empty[Int, BigInt]
    test(new Pipeline3(initialProgram = prog, memoryWords = 64)) { dut =>
      for (_ <- 0 until cycles) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          if (!writes.contains(rd))
            writes += rd -> dut.io.writebackData.peekInt()
        }
        dut.clock.step(1)
      }
    }
    Result(writes)
  }

  private def expectTrap(r: Result, cause: Int, tval: Int): Unit = {
    assert(
      r.writes.get(5).contains(BigInt(cause)),
      s"mcause esperado $cause, obtido ${r.writes.get(5)}"
    )
    assert(
      r.writes.get(7).contains(BigInt(tval)),
      s"mtval esperado 0x${tval.toHexString}, obtido ${r.writes.get(7)}"
    )
    assert(
      r.writes.get(8).contains(BigInt(0x14)),
      s"mepc esperado 0x14, obtido ${r.writes.get(8)}"
    )
    assert(
      !r.writes.contains(4),
      "instrucao apos o trap nao deveria ter sido executada"
    )
  }

  it should "gerar mcause 4 em LW desalinhado (offset 1) sem escrever rd" in {
    val r = run(program(0x81, load(LW, 3, 2, 0)))
    expectTrap(r, 4, 0x81)
    assert(!r.writes.contains(3), "LW com trap nao pode escrever rd")
  }

  it should "gerar mcause 4 em LW com offset 2 (so alinhado a meia palavra)" in {
    expectTrap(run(program(0x82, load(LW, 3, 2, 0))), 4, 0x82)
  }

  it should "gerar mcause 4 em LH em endereco impar" in {
    expectTrap(run(program(0x81, load(LH, 3, 2, 0))), 4, 0x81)
  }

  it should "somar o imediato ao calcular o endereco do trap (mtval)" in {
    expectTrap(run(program(0x80, load(LW, 3, 2, 2))), 4, 0x82)
  }

  it should "gerar mcause 6 em SW e SH desalinhados" in {
    expectTrap(run(program(0x82, store(SW, 6, 2, 0))), 6, 0x82)
    expectTrap(run(program(0x81, store(SH, 6, 2, 0))), 6, 0x81)
  }

  it should "gerar mcause 4 em LR.W e 6 em AMOADD.W / SC.W desalinhados" in {
    expectTrap(run(program(0x82, atomic(0x02, 3, 2, 0))), 4, 0x82)
    expectTrap(run(program(0x82, atomic(0x00, 3, 2, 6))), 6, 0x82)
    expectTrap(run(program(0x81, atomic(0x03, 3, 2, 6))), 6, 0x81)
  }

  it should "nao gerar trap em acessos alinhados (LB impar, LH par, LW multiplo de 4)" in {
    for (
      t <- Seq(
        (0x81, load(LB, 3, 2, 0)),
        (0x82, load(LH, 3, 2, 0)),
        (0x84, load(LW, 3, 2, 0))
      )
    ) {
      val r = run(program(t._1, t._2))
      assert(
        r.writes.contains(3),
        s"load alinhado em 0x${t._1.toHexString} deveria escrever rd"
      )
      assert(!r.writes.contains(5), "handler nao deveria ter sido executado")
      assert(r.writes.contains(4), "fluxo normal deveria continuar")
    }
  }
}
