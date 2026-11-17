package riscv.integration.diagnostics

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import riscv.integration.support.Rv32iBlackBox

/** RV32I_HEX=program.hex RV32I_CYCLES=40 sbt 'testOnly
  * riscv.integration.diagnostics.Rv32iHexTraceSpec'
  */
class Rv32iHexTraceSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "saídas arquiteturais de um programa HEX"

  it should "listar as gravações públicas em registradores" in {
    val path = sys.env.getOrElse(
      "RV32I_HEX",
      cancel("defina RV32I_HEX com o caminho do programa")
    )
    val source = Source.fromFile(path)
    val lines =
      try
        source
          .getLines()
          .map(_.takeWhile(_ != '#').trim)
          .filter(_.nonEmpty)
          .toVector
      finally source.close()
    require(lines.nonEmpty, "o arquivo HEX está vazio")
    lines.zipWithIndex.foreach { case (line, index) =>
      require(
        line.matches("[0-9a-fA-F]{8}"),
        s"linha ${index + 1}: use 8 dígitos hexadecimais"
      )
    }
    val program = lines.map(java.lang.Long.parseUnsignedLong(_, 16))
    val cycles = sys.env
      .get("RV32I_CYCLES")
      .map(_.toInt)
      .getOrElse(math.max(40, program.size * 4 + 16))
    require(cycles > 0, "RV32I_CYCLES deve ser positivo")

    test(
      new Rv32iBlackBox(program, words = math.max(64, program.size + cycles))
    ) { dut =>
      val writes = ArrayBuffer.empty[(Int, BigInt)]
      for (_ <- 0 until cycles) {
        if (
          dut.io.writebackEnable
            .peekBoolean() && dut.io.writebackRd.peekInt() != 0
        )
          writes += ((
            dut.io.writebackRd.peekInt().toInt,
            dut.io.writebackData.peekInt()
          ))
        dut.clock.step()
      }
      println(s"Programa: $path (${program.size} instruções, $cycles ciclos)")
      writes.foreach { case (rd, value) =>
        println(f"x$rd%d = 0x${(value & BigInt(0xffffffffL)).toLong}%08x")
      }
    }
  }
}
