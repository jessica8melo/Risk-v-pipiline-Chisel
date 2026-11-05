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
      var cycles = 0
      while ((loadedValue.isEmpty || dependentValue.isEmpty) && cycles < 80) {
        if (dut.io.writebackEnable.peekBoolean()) {
          dut.io.writebackRd.peekInt().toInt match {
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
}
