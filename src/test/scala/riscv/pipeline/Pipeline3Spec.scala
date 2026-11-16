package riscv.pipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Pipeline3Spec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3"

  private def addi(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x13L
  }

  private def lui(rd: Int, upper20: Int): Long =
    ((upper20 & 0xfffff).toLong << 12) | ((rd & 0x1f).toLong << 7) | 0x37L

  private def add(rd: Int, rs1: Int, rs2: Int): Long = {
    (0x00L << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x33L
  }

  private def lw(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x03L
  }

  private def sw(rs2: Int, rs1: Int, imm: Int): Long = {
    val imm11_5 = (imm >> 5) & 0x7f
    val imm4_0 = imm & 0x1f
    (imm11_5.toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      (imm4_0.toLong << 7) |
      0x23L
  }

  private def atomic(funct5: Int, rd: Int, rs1: Int, rs2: Int): Long = {
    ((funct5 & 0x1f).toLong << 27) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x2L << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x2fL
  }

  private def jal(rd: Int, imm: Int): Long = {
    val encoded = imm & 0x1fffff
    (((encoded >> 20) & 1).toLong << 31) |
      (((encoded >> 1) & 0x3ff).toLong << 21) |
      (((encoded >> 11) & 1).toLong << 20) |
      (((encoded >> 12) & 0xff).toLong << 12) |
      ((rd & 0x1f).toLong << 7) |
      0x6fL
  }

  private def jalr(rd: Int, rs1: Int, imm: Int): Long = {
    ((imm & 0xfff).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      ((rd & 0x1f).toLong << 7) |
      0x67L
  }

  private def beq(rs1: Int, rs2: Int, imm: Int): Long = {
    val encodedImm = imm & 0x1fff
    (((encodedImm >> 12) & 0x1).toLong << 31) |
      (((encodedImm >> 5) & 0x3f).toLong << 25) |
      ((rs2 & 0x1f).toLong << 20) |
      ((rs1 & 0x1f).toLong << 15) |
      (0x0L << 12) |
      (((encodedImm >> 1) & 0xf).toLong << 8) |
      (((encodedImm >> 11) & 0x1).toLong << 7) |
      0x63L
  }

  it should "executar uma sequencia aritmetica pequena com forwarding basico" in {
    val program = Seq(
      addi(1, 0, 5), // x1 = 5
      addi(2, 1, 3), // x2 = x1 + 3
      add(3, 1, 2), // x3 = x1 + x2
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      var sawX3 = false
      var cycles = 0
      while (!sawX3 && cycles < 32) {
        if (dut.io.writebackEnable.peekBoolean() && dut.io.writebackRd.peekInt() == 3 &&
            dut.io.writebackData.peekInt() == 13) {
          sawX3 = true
        }
        dut.clock.step(1)
        cycles += 1
      }

      assert(sawX3, "x3 deveria receber 13 após os misses da ICache")
    }
  }

  it should "fazer flush da instrucao seguinte quando um branch e tomado" in {
    val program = Seq(
      addi(1, 0, 5), // x1 = 5
      addi(2, 0, 5), // x2 = 5
      beq(1, 2, 8), // pula a proxima instrucao
      addi(3, 0, 99), // deve ser descartada pelo flush
      addi(3, 0, 42), // alvo do branch
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 32)) { dut =>
      var sawTargetValue = false
      var sawFlushedValue = false
      var cycles = 0
      while (!sawTargetValue && cycles < 48) {
        if (dut.io.writebackEnable.peekBoolean() && dut.io.writebackRd.peekInt() == 3) {
          if (dut.io.writebackData.peekInt() == 42) sawTargetValue = true
          if (dut.io.writebackData.peekInt() == 99) sawFlushedValue = true
        }
        dut.clock.step(1)
        cycles += 1
      }

      assert(sawTargetValue, "o valor do alvo do branch deveria ser escrito em x3")
      assert(!sawFlushedValue, "a instrução no caminho errado nao deveria escrever em x3")
    }
  }

  it should "executar store e load pela DCache ligada a mesma memoria principal" in {
    val program = Seq(
      addi(1, 0, 64), // endereco de dados
      addi(2, 0, 42),
      sw(2, 1, 0),
      lw(3, 1, 0),
      addi(4, 3, 1),
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      var loadedValue: Option[BigInt] = None
      var dependentValue: Option[BigInt] = None
      var loadWritebackEvents = 0
      var cycles = 0
      while ((loadedValue.isEmpty || dependentValue.isEmpty) && cycles < 80) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          if (rd == 3) loadWritebackEvents += 1
          rd match {
            case 3 => loadedValue = Some(dut.io.writebackData.peekInt())
            case 4 => dependentValue = Some(dut.io.writebackData.peekInt())
            case _ =>
          }
        }
        dut.clock.step(1)
        cycles += 1
      }

      assert(loadedValue.contains(42), s"lw deveria retornar 42, observado: $loadedValue")
      assert(dependentValue.contains(43), s"load-use deveria produzir 43, observado: $dependentValue")
      assert(loadWritebackEvents == 1, s"lw deveria gerar exatamente um writeback: $loadWritebackEvents")
    }
  }

  it should "executar LR, SC e AMOADD pela DCache" in {
    val program = Seq(
      addi(1, 0, 64),
      addi(2, 0, 5),
      atomic(funct5 = 0x02, rd = 3, rs1 = 1, rs2 = 0), // lr.w x3, (x1)
      atomic(funct5 = 0x03, rd = 4, rs1 = 1, rs2 = 2), // sc.w x4, x2, (x1)
      atomic(funct5 = 0x02, rd = 5, rs1 = 1, rs2 = 0), // lr.w x5, (x1)
      addi(2, 0, 3),
      atomic(funct5 = 0x00, rd = 6, rs1 = 1, rs2 = 2), // amoadd.w x6, x2, (x1)
      atomic(funct5 = 0x02, rd = 7, rs1 = 1, rs2 = 0), // lr.w x7, (x1)
      addi(0, 0, 0)
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      val writes = scala.collection.mutable.Map.empty[Int, BigInt]
      var cycles = 0
      var sawFinalLoad = false
      while (!sawFinalLoad && cycles < 80) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()
          if (rd != 0) writes(rd) = data
          if (rd == 7 && data == 8) sawFinalLoad = true
        }
        dut.clock.step(1)
        cycles += 1
      }

      assert(writes.get(3).contains(BigInt(0)), s"LR inicial deveria ler 0: $writes")
      assert(writes.get(4).contains(BigInt(0)), s"SC deveria indicar sucesso (0): $writes")
      assert(writes.get(5).contains(BigInt(5)), s"segundo LR deveria ler 5: $writes")
      assert(writes.get(6).contains(BigInt(5)), s"AMOADD deveria retornar valor antigo 5: $writes")
      assert(writes.get(7).contains(BigInt(8)), s"LR final deveria ler 8: $writes")
    }
  }

  it should "sincronizar instrucoes modificadas pela DCache com FENCE.I" in {
    val nop = 0x00000013L
    val replacement = addi(3, 0, 42)
    val program = Seq(
      jal(5, 40),                  // pré-carrega a linha de código modificável
      addi(1, 0, 40),              // x1 aponta para a instrução-alvo
      lui(2, (replacement >> 12).toInt),
      addi(2, 2, (replacement & 0xfff).toInt),
      sw(2, 1, 0),                 // escreve os novos bits na DCache
      0x0000100fL,                 // fence.i
      jalr(0, 1, 0),               // volta à instrução modificada
      nop,
      nop,
      nop,
      nop,
      jalr(0, 5, 0)                // instrução antiga em 0x28: retorna ao patcher
    )

    test(new Pipeline3(initialProgram = program, memoryWords = 64)) { dut =>
      var sawUpdatedInstruction = false
      var cycles = 0
      while (!sawUpdatedInstruction && cycles < 300) {
        if (dut.io.writebackEnable.peekBoolean() && dut.io.writebackRd.peekInt() == 3 &&
            dut.io.writebackData.peekInt() == 42) {
          sawUpdatedInstruction = true
        }
        dut.clock.step(1)
        cycles += 1
      }

      assert(sawUpdatedInstruction, "FENCE.I deveria fazer executar a instrução nova (addi x3, x0, 42)")
    }
  }

  // ----------------------------------------------------------------------
  // Testes de LR/SC/AMO. Memoria de 1 KiB: codigo em [0, 512) e dados a partir de
  // 512, para que codigo e dados nunca dividam linha de cache (sem FENCE.I).
  // ----------------------------------------------------------------------
  private val dataBase = 512
  private val marker = 0x55

  private val AMOADD = 0x00
  private val AMOSWAP = 0x01
  private val AMOXOR = 0x04
  private val AMOOR = 0x08
  private val AMOAND = 0x0c
  private val AMOMIN = 0x10
  private val AMOMINU = 0x12
  private val AMOMAX = 0x14
  private val AMOMAXU = 0x16
  private val LR = 0x02
  private val SC = 0x03

  private def u32(value: Int): BigInt = BigInt(value.toLong & 0xffffffffL)

  /** Executa o programa (mais um addi x31, x0, marker) e devolve o ultimo valor
    * escrito em cada registrador. Roda alguns ciclos extras apos o marcador para
    * drenar writebacks de loads atrasados.
    */
  private def runAndCollect(program: Seq[Long], maxCycles: Int = 4000): Map[Int, BigInt] = {
    val writes = scala.collection.mutable.Map.empty[Int, BigInt]
    var sawMarker = false
    test(new Pipeline3(initialProgram = program :+ addi(31, 0, marker), memoryWords = 256)) { dut =>
      var cycles = 0
      var extra = 0
      while (extra < 12 && cycles < maxCycles) {
        if (dut.io.writebackEnable.peekBoolean()) {
          val rd = dut.io.writebackRd.peekInt().toInt
          val data = dut.io.writebackData.peekInt()
          if (rd != 0) writes(rd) = data
          if (rd == 31 && data == marker) sawMarker = true
        }
        if (sawMarker) extra += 1
        dut.clock.step(1)
        cycles += 1
      }
    }
    assert(sawMarker, s"programa nao terminou (marcador x31 nao observado): $writes")
    writes.toMap
  }

  it should "falhar SC sem reserva e nao alterar a memoria" in {
    val program = Seq(
      addi(1, 0, dataBase),
      addi(2, 0, 7),
      sw(2, 1, 0), // mem = 7
      addi(3, 0, 9),
      atomic(SC, rd = 4, rs1 = 1, rs2 = 3), // sc.w x4, x3, (x1): sem LR previo
      lw(5, 1, 0)
    )

    val writes = runAndCollect(program)
    assert(writes.get(4).contains(BigInt(1)), s"SC sem reserva deveria retornar 1: $writes")
    assert(writes.get(5).contains(BigInt(7)), s"SC falho nao pode escrever na memoria: $writes")
  }

  it should "falhar SC em endereco diferente do reservado pelo LR" in {
    val program = Seq(
      addi(1, 0, dataBase),
      addi(6, 0, dataBase + 4),
      addi(2, 0, 7),
      atomic(LR, rd = 4, rs1 = 1, rs2 = 0), // reserva dataBase
      atomic(SC, rd = 5, rs1 = 6, rs2 = 2), // sc em dataBase + 4
      lw(7, 6, 0)
    )

    val writes = runAndCollect(program)
    assert(writes.get(5).contains(BigInt(1)), s"SC em outro endereco deveria falhar: $writes")
    assert(writes.get(7).contains(BigInt(0)), s"SC falho nao pode escrever na memoria: $writes")
  }

  it should "invalidar a reserva quando outra escrita atinge o mesmo endereco" in {
    val program = Seq(
      addi(1, 0, dataBase),
      addi(2, 0, 7),
      sw(2, 1, 0), // mem = 7
      atomic(LR, rd = 4, rs1 = 1, rs2 = 0), // reserva e le 7
      addi(3, 0, 9),
      sw(3, 1, 0), // escrita intermediaria: mem = 9, reserva perdida
      atomic(SC, rd = 5, rs1 = 1, rs2 = 2), // deveria falhar
      lw(6, 1, 0)
    )

    val writes = runAndCollect(program)
    assert(writes.get(4).contains(BigInt(7)), s"LR deveria ler 7: $writes")
    assert(writes.get(5).contains(BigInt(1)), s"SC apos escrita intermediaria deveria falhar: $writes")
    assert(writes.get(6).contains(BigInt(9)), s"memoria deveria manter o valor do sw (9): $writes")
  }

  it should "consumir a reserva no SC: um segundo SC falha" in {
    val program = Seq(
      addi(1, 0, dataBase),
      addi(2, 0, 5),
      addi(3, 0, 6),
      atomic(LR, rd = 4, rs1 = 1, rs2 = 0),
      atomic(SC, rd = 5, rs1 = 1, rs2 = 2), // sucesso: mem = 5
      atomic(SC, rd = 6, rs1 = 1, rs2 = 3), // sem reserva: falha
      lw(7, 1, 0)
    )

    val writes = runAndCollect(program)
    assert(writes.get(5).contains(BigInt(0)), s"primeiro SC deveria ter sucesso: $writes")
    assert(writes.get(6).contains(BigInt(1)), s"segundo SC deveria falhar: $writes")
    assert(writes.get(7).contains(BigInt(5)), s"memoria deveria conter o valor do primeiro SC: $writes")
  }

  it should "invalidar a reserva quando um AMO atinge o mesmo endereco" in {
    val program = Seq(
      addi(1, 0, dataBase),
      addi(2, 0, 4),
      addi(3, 0, 1),
      atomic(LR, rd = 4, rs1 = 1, rs2 = 0), // le 0 e reserva
      atomic(AMOADD, rd = 5, rs1 = 1, rs2 = 3), // mem = 1
      atomic(SC, rd = 6, rs1 = 1, rs2 = 2), // reserva perdida: falha
      lw(7, 1, 0)
    )

    val writes = runAndCollect(program)
    assert(writes.get(5).contains(BigInt(0)), s"AMOADD deveria devolver o valor antigo (0): $writes")
    assert(writes.get(6).contains(BigInt(1)), s"SC apos AMO deveria falhar: $writes")
    assert(writes.get(7).contains(BigInt(1)), s"memoria deveria conter apenas o resultado do AMO: $writes")
  }

  it should "executar todas as operacoes AMO.W com semantica correta" in {
    // (nome, funct5, valor inicial na memoria, operando rs2, valor esperado na memoria)
    val cases = Seq(
      ("AMOSWAP", AMOSWAP, 17, -3, -3),
      ("AMOADD", AMOADD, 100, -30, 70),
      ("AMOAND", AMOAND, 0xf0, 0x3c, 0x30),
      ("AMOOR", AMOOR, 0xf0, 0x0f, 0xff),
      ("AMOXOR", AMOXOR, 0xff, 0xf0, 0x0f),
      ("AMOMAX (mem < rs2)", AMOMAX, -5, 3, 3),
      ("AMOMAX (mem > rs2)", AMOMAX, 3, -5, 3),
      ("AMOMIN (mem < rs2)", AMOMIN, -5, 3, -5),
      ("AMOMIN (mem > rs2)", AMOMIN, 3, -5, -5),
      ("AMOMAXU (mem > rs2)", AMOMAXU, -5, 3, -5),
      ("AMOMAXU (mem < rs2)", AMOMAXU, 3, -5, -5),
      ("AMOMINU (mem > rs2)", AMOMINU, -5, 3, 3),
      ("AMOMINU (mem < rs2)", AMOMINU, 3, -5, 3)
    )

    // Cada caso usa seu proprio endereco e dois registradores: x(5+2i) = valor antigo
    // devolvido pelo AMO, x(6+2i) = valor final lido da memoria (x31 fica com o marcador).
    val program = cases.zipWithIndex.flatMap { case ((_, funct5, mem, operand, _), i) =>
      Seq(
        addi(1, 0, dataBase + 16 * i),
        addi(2, 0, mem),
        sw(2, 1, 0),
        addi(3, 0, operand),
        atomic(funct5, rd = 5 + 2 * i, rs1 = 1, rs2 = 3),
        lw(6 + 2 * i, 1, 0)
      )
    }

    val writes = runAndCollect(program)
    val failures = cases.zipWithIndex.flatMap { case ((name, _, mem, _, expected), i) =>
      val old = writes.get(5 + 2 * i)
      val fin = writes.get(6 + 2 * i)
      val problems = Seq(
        if (old.contains(u32(mem))) None
        else Some(s"$name: valor antigo esperado ${u32(mem)}, observado $old"),
        if (fin.contains(u32(expected))) None
        else Some(s"$name: valor final esperado ${u32(expected)}, observado $fin")
      )
      problems.flatten
    }
    assert(failures.isEmpty, failures.mkString("\n"))
  }
}
