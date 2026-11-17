package riscv.unit.atomic

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.atomic.AmoAlu
import riscv.decode.RV32I.AMOOp

class AmoAluSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AmoAlu"

  private def lit(value: Int) = (value.toLong & 0xffffffffL).U(32.W)

  // Modelo de referencia em Int de 32 bits (complemento de dois).
  private val operations: Seq[(String, UInt, (Int, Int) => Int)] = Seq(
    ("SWAP", AMOOp.SWAP, (_: Int, s: Int) => s),
    ("ADD", AMOOp.ADD, (m: Int, s: Int) => m + s),
    ("AND", AMOOp.AND, (m: Int, s: Int) => m & s),
    ("OR", AMOOp.OR, (m: Int, s: Int) => m | s),
    ("XOR", AMOOp.XOR, (m: Int, s: Int) => m ^ s),
    ("MAX", AMOOp.MAX, (m: Int, s: Int) => math.max(m, s)),
    ("MIN", AMOOp.MIN, (m: Int, s: Int) => math.min(m, s)),
    (
      "MAXU",
      AMOOp.MAXU,
      (m: Int, s: Int) => if (Integer.compareUnsigned(m, s) >= 0) m else s
    ),
    (
      "MINU",
      AMOOp.MINU,
      (m: Int, s: Int) => if (Integer.compareUnsigned(m, s) <= 0) m else s
    )
  )

  private val interesting = Seq(
    0, 1, -1, 2, -2, 0x7fffffff, 0x80000000, 0x80000001, 0x7fff0000, 123456789,
    -987654321
  )

  it should "calcular todas as operacoes AMO para pares representativos" in {
    test(new AmoAlu) { dut =>
      for (
        (name, op, model) <- operations; m <- interesting; s <- interesting
      ) {
        dut.io.op.poke(op)
        dut.io.memData.poke(lit(m))
        dut.io.operand.poke(lit(s))
        withClue(
          s"AMO$name mem=0x${m.toHexString} operand=0x${s.toHexString}: "
        ) {
          dut.io.result.expect(lit(model(m, s)))
        }
      }
    }
  }

  it should "distinguir comparacao com sinal e sem sinal" in {
    test(new AmoAlu) { dut =>
      dut.io.memData.poke(
        lit(-1)
      ) // 0xffffffff: negativo com sinal, maximo sem sinal
      dut.io.operand.poke(lit(1))

      dut.io.op.poke(AMOOp.MAX)
      dut.io.result.expect(lit(1))
      dut.io.op.poke(AMOOp.MIN)
      dut.io.result.expect(lit(-1))
      dut.io.op.poke(AMOOp.MAXU)
      dut.io.result.expect(lit(-1))
      dut.io.op.poke(AMOOp.MINU)
      dut.io.result.expect(lit(1))
    }
  }

  it should "devolver o operando para codigos de operacao fora da tabela" in {
    test(new AmoAlu) { dut =>
      dut.io.memData.poke(lit(0x1234))
      dut.io.operand.poke(lit(0x5678))
      for (invalidOp <- 9 to 15) {
        dut.io.op.poke(invalidOp.U)
        withClue(s"op=$invalidOp: ") { dut.io.result.expect(lit(0x5678)) }
      }
    }
  }
}
