package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

class CacheFsmTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Cache (FSM)"

  private val W = CacheParams.wordsPerLine
  // enderecos com mesmo index e tags diferentes
  private val SetStride = CacheParams.numSets * CacheParams.lineBytes

  /** Memoria principal simulada: responde no mesmo ciclo em que ve o pedido. */
  private class MemModel {
    val lines = mutable.Map[BigInt, Seq[BigInt]]()
    var reads = 0
    var writes = 0
    def line(a: BigInt): Seq[BigInt] = lines.getOrElse(a, Seq.fill(W)(BigInt(0)))
    def preload(base: BigInt, words: Seq[BigInt]): Unit = lines(base) = words
  }

  private case class Result(data: BigInt, cycles: Int)

  private def initIO(dut: Cache): Unit = {
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
  private def access(
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
    var result: Option[BigInt] = None
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
        result = Some(dut.io.cpu.readData.peek().litValue)
      }
      dut.clock.step(1)
      cycles += 1
    }
    dut.io.cpu.request.poke(false.B)
    dut.io.mem.ready.poke(false.B)
    Result(result.get, cycles)
  }

  it should "ficar ocioso sem pedido" in {
    test(new Cache) { dut =>
      initIO(dut)
      for (_ <- 0 until 5) {
        dut.clock.step(1)
        dut.io.cpu.stall.expect(false.B)
        dut.io.cpu.hit.expect(false.B)
        dut.io.mem.request.expect(false.B)
      }
    }
  }

  it should "resolver read miss com refill e depois acertar na mesma linha" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      mem.preload(0x1000, Seq(0xa0, 0xa1, 0xa2, 0xa3).map(BigInt(_)))

      val miss = access(dut, mem, 0x1004)
      assert(miss.data == BigInt(0xa1))
      assert(mem.reads == 1 && mem.writes == 0)

      val hit = access(dut, mem, 0x1008)
      assert(hit.data == BigInt(0xa2))
      assert(mem.reads == 1, "hit nao pode acessar a memoria")
      assert(hit.cycles == 2, "hit = Idle + CompareTag")
      assert(miss.cycles > hit.cycles)
    }
  }

  it should "tratar store miss com write-allocate, sem escrever na memoria" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      access(dut, mem, 0x2000, write = true, data = BigInt(0xcafebabeL))
      assert(mem.reads == 1 && mem.writes == 0)
      assert(access(dut, mem, 0x2000).data == BigInt(0xcafebabeL))
      assert(mem.writes == 0, "write-back: memoria so e atualizada no despejo")
    }
  }

  it should "fazer write-back ao despejar linha suja" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      val d0 = BigInt(0x12345678L)
      access(dut, mem, 0L, write = true, data = d0) // via 0, suja
      for (k <- 1 to 3) access(dut, mem, k.toLong * SetStride) // enche as 4 vias
      assert(mem.writes == 0)

      access(dut, mem, 4L * SetStride) // 5a tag no mesmo conjunto: despejo
      assert(mem.writes == 1)
      assert(mem.lines(BigInt(0))(0) == d0)

      // o dado despejado volta corretamente da memoria
      assert(access(dut, mem, 0L).data == d0)
    }
  }

  it should "nao fazer write-back ao despejar linha limpa" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      for (k <- 0 to 4) access(dut, mem, k.toLong * SetStride)
      assert(mem.reads == 5)
      assert(mem.writes == 0)
    }
  }

  it should "tratar load/store de byte e half (extensao de sinal e merge)" in {
    test(new Cache) { dut =>
      initIO(dut)
      val mem = new MemModel
      mem.preload(0x1000, Seq(BigInt(0x11223380L), 0, 0, 0))

      assert(access(dut, mem, 0x1000, size = 0).data == BigInt(0xffffff80L)) // lb
      assert(access(dut, mem, 0x1000, size = 0, unsigned = true).data == BigInt(0x80)) // lbu
      assert(access(dut, mem, 0x1000, size = 1).data == BigInt(0x3380)) // lh
      assert(access(dut, mem, 0x1001, size = 0).data == BigInt(0x33)) // lb offset 1

      access(dut, mem, 0x1001, write = true, data = BigInt(0xaa), size = 0) // sb
      assert(access(dut, mem, 0x1000).data == BigInt(0x1122aa80L))
      access(dut, mem, 0x1002, write = true, data = BigInt(0xbeef), size = 1) // sh
      assert(access(dut, mem, 0x1000).data == BigInt(0xbeefaa80L))
    }
  }

  it should "funcionar como ICache (somente leitura)" in {
    test(new Cache(isInstructionCache = true)) { dut =>
      initIO(dut)
      val mem = new MemModel
      mem.preload(0x0, Seq(0x13, 0x23, 0x33, 0x43).map(BigInt(_)))
      assert(access(dut, mem, 0x8).data == BigInt(0x33))
      assert(access(dut, mem, 0xc).data == BigInt(0x43))
      assert(mem.reads == 1)
    }
  }
}