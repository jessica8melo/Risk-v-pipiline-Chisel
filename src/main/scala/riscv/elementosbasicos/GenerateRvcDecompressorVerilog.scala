package riscv.elementosbasicos

import chisel3.stage.ChiselStage

object GenerateRvcDecompressorVerilog extends App {
  (new ChiselStage).emitVerilog(
    new RvcDecompressor,
    Array("--target-dir", "output/decompressor")
  )
}
