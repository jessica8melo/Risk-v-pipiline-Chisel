package riscv.cache

import chisel3._
import chiseltest._
import scala.collection.mutable

/** Utilitarios compartilhados pelos testes da Cache: memoria principal
  * simulada e um `access` que conduz um pedido ate ser atendido.
  */
trait CacheTestHelpers {
  val W: Int = CacheParams.wordsPerLine
  // enderecos com mesmo index e tags diferentes
  val SetStride: Long = CacheParams.numSets.toLong * CacheParams.lineBytes

  /** Memoria principal simulada: responde no mesmo ciclo em que ve o pedido. */
  class MemModel {
    val lines = mutable.Map[BigInt, Seq[BigInt]]()
    var reads = 0
    var writes = 0
    def line(a: BigInt): Seq[BigInt] = lines.getOrElse(a, Seq.fill(W)(BigInt(0)))
    def preload(base: BigInt, words: Seq[BigInt]): Unit = lines(base) = words
  }

  /** hitWay/hitDirty vem da porta de debug, lida no ciclo do acerto. */
  case class Result(data: BigInt, cycles: Int, hitWay: Int, hitDirty: Boolean)

  def initIO(dut: Cache): Unit = {
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.io.cpu.address.poke(0.U)
    dut.io.cpu.writeData.poke(0.U)
    dut.io.cpu.memSize.poke(2.U)
    dut.io.cpu.unsignedLoad.poke(false.B)
    dut.io.mem.ready.poke(false.B)
    for (i <- 0 until W) dut.io.mem.readLine(i).poke(0.U)
  }

  /** size: 0 = byte, 1 = half, 2 = word. Devolve o dado lido e os ciclos gastos. */
  def access(
      dut: Cache, mem: MemModel, addr: Long, write: Boolean = false,
      data: BigInt = 0, size: Int = 2, unsigned: Boolean = false
  ): Result = {
    dut.io.cpu.request.poke(true.B)
    dut.io.cpu.write.poke(write.B)
    dut.io.cpu.address.poke(BigInt(addr).U(32.W))
    dut.io.cpu.writeData.poke(data.U(32.W))
    dut.io.cpu.memSize.poke(size.U)
    dut.io.cpu.unsignedLoad.poke(unsigned.B)

    var cycles = 0
    var result: Option[Result] = None
    while (result.isEmpty) {
      require(cycles < 100, "timeout: cache travada")
      if (dut.io.mem.request.peek().litToBoolean) {
        val lineAddr = dut.io.mem.address.peek().litValue
        if (dut.io.mem.write.peek().litToBoolean) {
          mem.lines(lineAddr) = (0 until W).map(i => dut.io.mem.writeLine(i).peek().litValue)
          mem.writes += 1
        } else {
          val l = mem.line(lineAddr)
          for (i <- 0 until W) dut.io.mem.readLine(i).poke(l(i).U(32.W))
          mem.reads += 1
        }
        dut.io.mem.ready.poke(true.B)
      } else {
        dut.io.mem.ready.poke(false.B)
      }

      if (dut.io.cpu.hit.peek().litToBoolean) {
        dut.io.cpu.stall.expect(false.B)
        result = Some(
          Result(
            dut.io.cpu.readData.peek().litValue,
            cycles + 1,
            dut.io.dbg.hitWay.peek().litValue.toInt,
            dut.io.dbg.hitDirty.peek().litToBoolean
          )
        )
      }
      dut.clock.step(1)
      cycles += 1
    }
    dut.io.cpu.request.poke(false.B)
    dut.io.mem.ready.poke(false.B)
    result.get
  }
}