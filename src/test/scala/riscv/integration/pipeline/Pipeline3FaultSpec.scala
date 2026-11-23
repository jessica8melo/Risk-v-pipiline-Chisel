package riscv.integration.pipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.cache.CacheConfig
import riscv.pipeline.Pipeline3
import scala.collection.mutable

/** Excecoes sincronas de acesso e de decodificacao no Pipeline3:
  *   - load / LR fora da memoria -> mcause 5 (load access fault)
  *   - store / SC / AMO fora -> mcause 7 (store/AMO access fault)
  *   - busca fora da memoria -> mcause 1 (instruction access fault)
  *   - instrucao ilegal -> mcause 2, mtval = encoding original
  * Em todos os casos mepc = PC da instrucao e a instrucao nao tem efeito.
  */
class Pipeline3FaultSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 (access fault e instrucao ilegal)"

  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x1L << 12) | (rd.toLong << 7) | 0x73L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private def load(f3: Int, rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (f3.toLong << 12) |
      (rd.toLong << 7) | 0x03L
  private def store(f3: Int, rs2: Int, rs1: Int, imm: Int): Long =
    (((imm >> 5) & 0x7f).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (f3.toLong << 12) | ((imm & 0x1f).toLong << 7) | 0x23L
  private def atomic(funct5: Int, rd: Int, rs1: Int, rs2: Int): Long =
    (funct5.toLong << 27) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (0x2L << 12) | (rd.toLong << 7) | 0x2fL
  private def jalr(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x67L
  private val NOP = 0x00000013L
  private val jal0 = 0x0000006fL // jal x0, 0

  private val LB = 0; private val LH = 1; private val LW = 2;
  private val LBU = 4
  private val SB = 0; private val SW = 2

  // Memoria de 64 palavras (256 B): 0x100 em diante esta fora da faixa.
  private val MemWords = 64
  private val Outside = 0x100
  private val TestedPc = 0x14

  /** mtvec = 0x40; x2 = addr; x6 = 0x55; [testada em 0x14]; x4 = 1 (nao pode
    * executar apos um trap); laco em 0x1C; handler em 0x40 le mcause, mtval e
    * mepc em x5, x7 e x8 e fica em laco.
    */
  private def program(addr: Int, tested: Long): Seq[Long] =
    Seq(
      addi(1, 0, 0x40), // 0x00
      csrrw(0, 0x305, 1), // 0x04 mtvec = 0x40
      addi(2, 0, addr), // 0x08
      addi(6, 0, 0x55), // 0x0C
      NOP, // 0x10
      tested, // 0x14
      addi(4, 0, 1), // 0x18 so executa se NAO houve trap
      jal0, // 0x1C
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
      jal0 // 0x4C
    )

  // Caches pequenas (1 KiB, 2 vias): elaboracao e simulacao mais rapidas.
  private val smallCache =
    CacheConfig(capacityBytes = 1024, ways = 2, lineBytes = 16)

  private def run(prog: Seq[Long], latency: Int): Map[Int, BigInt] = {
    val writes = mutable.Map.empty[Int, BigInt]
    test(
      new Pipeline3(
        initialProgram = prog,
        memoryWords = MemWords,
        memoryLatency = latency,
        cacheConfig = smallCache
      )
    ) { dut =>
      for (_ <- 0 until 150 + 120 * latency) {
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

  private def expectTrap(
      w: Map[Int, BigInt],
      cause: Int,
      tval: BigInt,
      epc: Int = TestedPc
  ): Unit = {
    assert(
      w.get(5).contains(BigInt(cause)),
      s"mcause esperado $cause: ${w.get(5)}"
    )
    assert(
      w.get(7).contains(tval),
      s"mtval esperado 0x${tval.toString(16)}: ${w.get(7)}"
    )
    assert(
      w.get(8).contains(BigInt(epc)),
      s"mepc esperado 0x${epc.toHexString}: ${w.get(8)}"
    )
    assert(!w.contains(4), "a instrucao apos a trap nao deveria executar")
  }

  for (lat <- Seq(0, 3)) {
    it should s"gerar mcause 5 em LW fora da memoria, sem escrever rd (latencia $lat)" in {
      val w = run(program(Outside, load(LW, 3, 2, 0)), lat)
      expectTrap(w, 5, Outside)
      assert(!w.contains(3), "load com fault nao pode escrever rd")
    }

    it should s"gerar mcause 7 em SW fora da memoria (latencia $lat)" in {
      expectTrap(run(program(Outside, store(SW, 6, 2, 0)), lat), 7, Outside)
    }

    it should s"gerar mcause 1 ao buscar instrucao fora da memoria (latencia $lat)" in {
      // jalr para 0x100: a busca no destino e recusada.
      expectTrap(
        run(program(Outside, jalr(0, 2, 0)), lat),
        1,
        Outside,
        epc = Outside
      )
    }

    it should s"gerar mcause 2 em instrucao ilegal com mtval = encoding (latencia $lat)" in {
      expectTrap(
        run(program(0, 0xffffffffL), lat),
        2,
        BigInt(0xffffffffL)
      )
    }
  }

  it should "gerar mcause 5 em LH e LBU fora da memoria, e mtval com o endereco efetivo" in {
    expectTrap(run(program(Outside, load(LH, 3, 2, 2)), 0), 5, Outside + 2)
    expectTrap(run(program(Outside, load(LBU, 3, 2, 1)), 0), 5, Outside + 1)
  }

  it should "gerar mcause 7 em SB, AMO e SC fora da memoria, e mcause 5 em LR" in {
    expectTrap(run(program(Outside, store(SB, 6, 2, 0)), 0), 7, Outside)
    expectTrap(run(program(Outside, atomic(0x00, 3, 2, 6)), 0), 7, Outside)
    expectTrap(run(program(Outside, atomic(0x02, 3, 2, 0)), 0), 5, Outside)
  }

  it should "nao gerar trap em acessos dentro da memoria (ultima palavra)" in {
    val w = run(program(0xfc, load(LW, 3, 2, 0)), 0)
    assert(w.contains(3) && w.contains(4), "o fluxo normal deveria continuar")
    assert(!w.contains(5), "o handler nao deveria ter sido executado")
  }

  it should "gerar mcause 2 na palavra zero e em instrucao RVC ilegal" in {
    expectTrap(run(program(0, 0x00000000L), 0), 2, BigInt(0))
    // C.JR com rs1 = 0 (0x8002) e ilegal; mtval = os 16 bits originais.
    expectTrap(run(program(0, 0x00018002L), 0), 2, BigInt(0x8002))
  }

  it should "gerar mcause 2 em LR.W com rs2 diferente de zero" in {
    val bad = atomic(0x02, 3, 2, 5)
    expectTrap(run(program(0x80, bad), 0), 2, BigInt(bad))
  }
}
