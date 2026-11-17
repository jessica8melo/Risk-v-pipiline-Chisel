package riscv.integration.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
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

  private def jalr(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x67L

  // Cenarios: cada um e uma instrucao testada + endereco base em x2. Todos rodam
  // num UNICO programa (e portanto numa unica compilacao do Pipeline3, que e o
  // que domina o tempo do teste); o handler volta para o marcador do cenario.
  private case class Scenario(name: String, addr: Int, tested: Long)

  private val scenarios = Seq(
    Scenario("lw+1", 0x81, load(LW, 3, 2, 0)),
    Scenario("lw+2", 0x82, load(LW, 3, 2, 0)),
    Scenario("lh+1", 0x81, load(LH, 3, 2, 0)),
    Scenario("lw+imm", 0x80, load(LW, 3, 2, 2)),
    Scenario("sw+2", 0x82, store(SW, 6, 2, 0)),
    Scenario("sh+1", 0x81, store(SH, 6, 2, 0)),
    Scenario("lr+2", 0x82, atomic(0x02, 3, 2, 0)),
    Scenario("amoadd+2", 0x82, atomic(0x00, 3, 2, 6)),
    Scenario("sc+1", 0x81, atomic(0x03, 3, 2, 6)),
    Scenario("lb+1 alinhado", 0x81, load(LB, 3, 2, 0)),
    Scenario("lh+2 alinhado", 0x82, load(LH, 3, 2, 0)),
    Scenario("lw+4 alinhado", 0x84, load(LW, 3, 2, 0))
  )
  private def named(n: String): Int = scenarios.indexWhere(_.name == n)

  private val Slot = 0x20 // 8 instrucoes por cenario
  private val FirstSlot = 0x08
  private def base(k: Int): Int = FirstSlot + k * Slot
  private def mepcOf(k: Int): Int = base(k) + 0x10 // PC da instrucao testada
  private val loopPc = FirstSlot + scenarios.size * Slot
  private val handlerPc = loopPc + 0x10

  /** mtvec=handler; para cada cenario k: x2=addr; x11=marcador; x6=0x55;
    * [testada]; x4=1 (nao deve executar se houver trap); x9=k+1 (marcador). O
    * handler le mcause/mtval/mepc em x5/x7/x8 e volta ao marcador via x11.
    */
  private def program: Seq[Long] = {
    val prologue = Seq(
      addi(1, 0, handlerPc), // 0x00
      csrrw(0, 0x305, 1) // 0x04 mtvec = handler
    )
    val body = scenarios.zipWithIndex.flatMap { case (sc, k) =>
      Seq(
        addi(2, 0, sc.addr), // +0x00
        addi(11, 0, base(k) + 0x18), // +0x04 destino do handler
        addi(6, 0, 0x55), // +0x08
        NOP, // +0x0C
        sc.tested, // +0x10
        addi(4, 0, 1), // +0x14 apos o trap NAO pode ser executada
        addi(9, 0, k + 1), // +0x18 marcador de fim do cenario
        NOP // +0x1C
      )
    }
    val tail = Seq(jal0, NOP, NOP, NOP) // laco infinito em loopPc
    val handler = Seq(
      csrrs(5, 0x342, 0), // x5 = mcause
      csrrs(7, 0x343, 0), // x7 = mtval
      csrrs(8, 0x341, 0), // x8 = mepc
      jalr(0, 11, 0)
    )
    prologue ++ body ++ tail ++ handler
  }

  private case class Result(writes: Map[Int, BigInt])

  /** Roda o programa uma vez e separa os writebacks por cenario (cada cenario
    * termina com a escrita do seu marcador em x9). Vale a 1a escrita por rd.
    */
  private def runAll(maxCycles: Int = 4000): Seq[Result] = {
    val log = ArrayBuffer.empty[(Int, BigInt)]
    test(new Pipeline3(initialProgram = program, memoryWords = 256)) { dut =>
      var cycle = 0
      while (
        cycle < maxCycles && !log.exists(_ == ((9, BigInt(scenarios.size))))
      ) {
        if (dut.io.writebackEnable.peekBoolean())
          log += ((
            dut.io.writebackRd.peekInt().toInt,
            dut.io.writebackData.peekInt()
          ))
        dut.clock.step(1)
        cycle += 1
      }
    }
    var segment = Map.empty[Int, BigInt]
    val out = ArrayBuffer.empty[Result]
    for ((rd, data) <- log) {
      if (rd == 9 && data == BigInt(out.size + 1)) {
        out += Result(segment)
        segment = Map.empty
      } else if (!segment.contains(rd)) segment += rd -> data
    }
    require(
      out.size == scenarios.size,
      s"so ${out.size} de ${scenarios.size} cenarios terminaram"
    )
    out.toSeq
  }

  // Try memoriza a falha tambem: se a simulacao quebrar, ela nao roda de novo.
  private lazy val results: Try[Seq[Result]] = Try(runAll())
  private def result(name: String): Result = results.get(named(name))

  private def expectTrap(name: String, cause: Int, tval: Int): Unit = {
    val k = named(name)
    val r = result(name)
    assert(
      r.writes.get(5).contains(BigInt(cause)),
      s"$name: mcause esperado $cause, obtido ${r.writes.get(5)}"
    )
    assert(
      r.writes.get(7).contains(BigInt(tval)),
      s"$name: mtval esperado 0x${tval.toHexString}, obtido ${r.writes.get(7)}"
    )
    assert(
      r.writes.get(8).contains(BigInt(mepcOf(k))),
      s"$name: mepc esperado 0x${mepcOf(k).toHexString}, obtido ${r.writes.get(8)}"
    )
    assert(
      !r.writes.contains(4),
      s"$name: instrucao apos o trap nao deveria ter sido executada"
    )
  }

  it should "gerar mcause 4 em LW desalinhado (offset 1) sem escrever rd" in {
    expectTrap("lw+1", 4, 0x81)
    assert(!result("lw+1").writes.contains(3), "rd nao pode ser escrito")
  }

  it should "gerar mcause 4 em LW com offset 2 (so alinhado a meia palavra)" in {
    expectTrap("lw+2", 4, 0x82)
  }

  it should "gerar mcause 4 em LH em endereco impar" in {
    expectTrap("lh+1", 4, 0x81)
  }

  it should "somar o imediato ao calcular o endereco do trap (mtval)" in {
    expectTrap("lw+imm", 4, 0x82)
  }

  it should "gerar mcause 6 em SW e SH desalinhados" in {
    expectTrap("sw+2", 6, 0x82)
    expectTrap("sh+1", 6, 0x81)
  }

  it should "gerar mcause 4 em LR.W e 6 em AMOADD.W / SC.W desalinhados" in {
    expectTrap("lr+2", 4, 0x82)
    expectTrap("amoadd+2", 6, 0x82)
    expectTrap("sc+1", 6, 0x81)
  }

  it should "nao gerar trap em acessos alinhados (LB impar, LH par, LW multiplo de 4)" in {
    for (n <- Seq("lb+1 alinhado", "lh+2 alinhado", "lw+4 alinhado")) {
      val r = result(n)
      assert(r.writes.contains(3), s"$n: load alinhado deveria escrever rd")
      assert(
        !r.writes.contains(5),
        s"$n: handler nao deveria ter sido executado"
      )
      assert(r.writes.contains(4), s"$n: fluxo normal deveria continuar")
    }
  }
}
