package riscv.elementosbasicos

import chisel3._
import chisel3.util._

/** Memoria de instrucoes somente leitura.
  *
  * O endereco chega em bytes e a leitura entrega uma palavra alinhada de 4
  * bytes. O fetch seleciona e monta as instrucoes de 16 ou 32 bits dessa
  * palavra.
  */
class InstructionMemory(
    depthWords: Int = 1024,
    initialData: Seq[Long] = Seq.empty
) extends Module {
  require(depthWords > 0, "depthWords deve ser maior que zero")

  val io = IO(new Bundle {
    val address = Input(UInt(32.W))
    val readEnable = Input(Bool())
    val readData = Output(UInt(32.W))
  })

  private val wordIndex = io.address(log2Ceil(depthWords) + 1, 2)

  val program = initialData.padTo(depthWords, 0L)
  val mem = VecInit(program.map(_.U(32.W)))
  io.readData := Mux(io.readEnable, mem(wordIndex), 0.U)
}

/** Memoria de dados byte-addressable.
  *
  * Ela guarda bytes internamente para conseguir implementar acessos de 8, 16 e
  * 32 bits. Os dados sao little-endian: o byte menos significativo fica no
  * menor endereco.
  */
class DataMemory(depthWords: Int = 1024) extends Module {
  require(depthWords > 0, "depthWords deve ser maior que zero")

  private val depthBytes = depthWords * 4
  private val addrBits = log2Ceil(depthBytes)

  val io = IO(new Bundle {
    val address = Input(UInt(32.W))
    val writeData = Input(UInt(32.W))
    val writeEnable = Input(Bool())
    val memSize = Input(UInt(2.W))
    val unsignedLoad = Input(Bool())
    val readData = Output(UInt(32.W))
  })

  import RV32I.MemorySize

  private val mem = RegInit(VecInit(Seq.fill(depthBytes)(0.U(8.W))))
  private val byteAddress = io.address(addrBits - 1, 0)

  private val byte = mem(byteAddress)
  private val half = Cat(mem(byteAddress + 1.U), mem(byteAddress))
  private val word = Cat(
    mem(byteAddress + 3.U),
    mem(byteAddress + 2.U),
    mem(byteAddress + 1.U),
    mem(byteAddress)
  )

  private val byteExtended = Mux(
    io.unsignedLoad,
    Cat(0.U(24.W), byte),
    Cat(Fill(24, byte(7)), byte)
  )

  private val halfExtended = Mux(
    io.unsignedLoad,
    Cat(0.U(16.W), half),
    Cat(Fill(16, half(15)), half)
  )

  io.readData := MuxLookup(
    io.memSize,
    word
  )(
    Seq(
      MemorySize.BYTE -> byteExtended,
      MemorySize.HALF -> halfExtended,
      MemorySize.WORD -> word
    )
  )

  when(io.writeEnable) {
    switch(io.memSize) {
      is(MemorySize.BYTE) {
        mem(byteAddress) := io.writeData(7, 0)
      }

      is(MemorySize.HALF) {
        mem(byteAddress) := io.writeData(7, 0)
        mem(byteAddress + 1.U) := io.writeData(15, 8)
      }

      is(MemorySize.WORD) {
        mem(byteAddress) := io.writeData(7, 0)
        mem(byteAddress + 1.U) := io.writeData(15, 8)
        mem(byteAddress + 2.U) := io.writeData(23, 16)
        mem(byteAddress + 3.U) := io.writeData(31, 24)
      }
    }
  }
}
