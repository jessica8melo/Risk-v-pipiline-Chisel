package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

/** DUT de integracao: L1Cache (ICache + DCache) ligada a uma CacheMainMemory
  * com latencia configuravel. Expoe so o lado da CPU e o FENCE.I.
  */
class L1WithMemory(latency: Int, depthWords: Int, initialData: Seq[Long])
    extends Module {
  val io = IO(new Bundle {
    val iCache = Flipped(new CpuCacheIO)
    val dCache = Flipped(new CpuCacheIO)
    val fenceI = Input(Bool())
    val fenceIDone = Output(Bool())
  })

  val l1 = Module(new L1Cache)
  val mem = Module(
    new CacheMainMemory(depthWords, initialData, latency = latency)
  )

  l1.io.iMem <> mem.io.iMem
  l1.io.dMem <> mem.io.dMem
  l1.io.iCache <> io.iCache
  l1.io.dCache <> io.dCache
  l1.io.fenceI := io.fenceI
  io.fenceIDone := l1.io.fenceIDone
}

/** Testes de estresse da memoria principal: ready demorando varios ciclos,
  * pedidos simultaneos da ICache e da DCache e write-back da DCache seguido de
  * leitura pela ICache.
  */
class CacheMemoryLatencyTest extends AnyFlatSpec with ChiselScalatestTester {

  private val Latencies = Seq(1, 3, 6)

  // ------------------------------------------------------------------
  // Parte 1: CacheMainMemory isolada
  // ------------------------------------------------------------------

  private def initMem(dut: CacheMainMemory): Unit = {
    for (port <- Seq(dut.io.iMem, dut.io.dMem)) {
      port.request.poke(false.B)
      port.write.poke(false.B)
      port.address.poke(0.U)
      for (w <- 0 until CacheParams.wordsPerLine) port.writeLine(w).poke(0.U)
    }
  }

  /** Reinicia a memoria (conteudo inicial, FSM) para reaproveitar uma unica
    * compilacao em varios cenarios independentes.
    */
  private def resetMem(dut: CacheMainMemory): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    initMem(dut)
  }

  /** Espera `ready` subir e devolve quantos ciclos foram esperados. */
  private def waitReady(
      dut: CacheMainMemory,
      ready: Bool,
      max: Int = 50
  ): Int = {
    var c = 0
    while (!ready.peek().litToBoolean) {
      require(c < max, "timeout esperando ready")
      dut.clock.step(1)
      c += 1
    }
    c
  }

  private def expectLine(port: CacheMemIO, words: Seq[Long]): Unit =
    for (w <- words.indices) port.readLine(w).expect(words(w).U)

  private val memData = Seq(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)

  private def newMem(latency: Int) =
    new CacheMainMemory(
      depthWords = 64,
      initialData = memData,
      latency = latency
    )

  behavior of "CacheMainMemory com latencia"

  for (latency <- Latencies) {
    it should s"cobrir leitura, escrita, arbitragem e aborto (latencia $latency)" in {
      test(newMem(latency)) { dut =>
        def scenario(name: String)(body: CacheMainMemory => Unit): Unit = {
          resetMem(dut)
          body(dut)
        }

        scenario(s"demorar exatamente $latency ciclos para ler uma linha") {
          dut =>
            {
              dut.io.iMem.request.poke(true.B)
              dut.io.iMem.address.poke(16.U)
              dut.io.dMem.ready.expect(false.B)

              assert(waitReady(dut, dut.io.iMem.ready) == latency)
              expectLine(dut.io.iMem, Seq(5, 6, 7, 8))
              dut.io.dMem.ready.expect(false.B)
            }
        }

        scenario(
          s"escrever uma linha da DCache e le-la pela ICache (latencia $latency)"
        ) { dut =>
          {
            dut.io.dMem.request.poke(true.B)
            dut.io.dMem.write.poke(true.B)
            dut.io.dMem.address.poke(16.U)
            for (w <- 0 until CacheParams.wordsPerLine)
              dut.io.dMem.writeLine(w).poke((0xa0 + w).U)

            assert(waitReady(dut, dut.io.dMem.ready) == latency)
            dut.io.iMem.ready.expect(false.B)
            dut.clock.step(1)
            dut.io.dMem.request.poke(false.B)
            dut.io.dMem.write.poke(false.B)
            dut.clock.step(1)

            dut.io.iMem.request.poke(true.B)
            dut.io.iMem.address.poke(16.U)
            assert(waitReady(dut, dut.io.iMem.ready) == latency)
            expectLine(dut.io.iMem, Seq(0xa0, 0xa1, 0xa2, 0xa3))
            dut.clock.step(1)

            // a linha vizinha nao foi alterada (novo pedido comeca neste ciclo)
            dut.io.iMem.address.poke(0.U)
            assert(waitReady(dut, dut.io.iMem.ready) == latency)
            expectLine(dut.io.iMem, Seq(1, 2, 3, 4))
          }
        }

        scenario(
          s"serializar pedidos simultaneos com prioridade da DCache (latencia $latency)"
        ) { dut =>
          {
            dut.io.dMem.request.poke(true.B)
            dut.io.dMem.address.poke(0.U)
            dut.io.iMem.request.poke(true.B)
            dut.io.iMem.address.poke(16.U)

            // DCache atendida primeiro; ICache fica sem ready ate la.
            var c = 0
            while (!dut.io.dMem.ready.peek().litToBoolean) {
              require(c < 50, "timeout esperando a DCache")
              dut.io.iMem.ready.expect(false.B)
              dut.clock.step(1)
              c += 1
            }
            assert(c == latency)
            dut.io.iMem.ready.expect(false.B)
            expectLine(dut.io.dMem, Seq(1, 2, 3, 4))

            // A cache solta o pedido depois de receber ready; o da ICache continua.
            dut.clock.step(1)
            dut.io.dMem.request.poke(false.B)
            assert(waitReady(dut, dut.io.iMem.ready) == latency)
            expectLine(dut.io.iMem, Seq(5, 6, 7, 8))
          }
        }

        scenario(
          s"nao tirar o barramento da ICache quando a DCache chega depois (latencia $latency)"
        ) { dut =>
          {
            dut.io.iMem.request.poke(true.B)
            dut.io.iMem.address.poke(16.U)
            dut.clock.step(1)
            // DCache chega com a transacao da ICache em andamento
            dut.io.dMem.request.poke(true.B)
            dut.io.dMem.address.poke(0.U)

            var c = 1
            while (!dut.io.iMem.ready.peek().litToBoolean) {
              require(c < 50, "timeout esperando a ICache")
              dut.io.dMem.ready.expect(false.B)
              dut.clock.step(1)
              c += 1
            }
            assert(
              c == latency,
              "a transacao da ICache nao pode ser atrasada pela DCache"
            )
            dut.io.dMem.ready.expect(false.B)
            expectLine(dut.io.iMem, Seq(5, 6, 7, 8))

            dut.clock.step(1)
            dut.io.iMem.request.poke(false.B)
            assert(waitReady(dut, dut.io.dMem.ready) == latency)
            expectLine(dut.io.dMem, Seq(1, 2, 3, 4))
          }
        }

        if (latency >= 3) // precisa de ao menos 1 ciclo de espera para abortar
          scenario("abortar sem escrever quando o pedido cai antes de ready") {
            dut =>
              {
                dut.io.dMem.request.poke(true.B)
                dut.io.dMem.write.poke(true.B)
                dut.io.dMem.address.poke(16.U)
                for (w <- 0 until CacheParams.wordsPerLine)
                  dut.io.dMem.writeLine(w).poke(0xff.U)
                dut.clock.step(latency - 1)
                dut.io.dMem.request.poke(false.B)
                dut.io.dMem.write.poke(false.B)
                dut.clock.step(6)

                dut.io.iMem.request.poke(true.B)
                dut.io.iMem.address.poke(16.U)
                assert(waitReady(dut, dut.io.iMem.ready) == latency)
                expectLine(dut.io.iMem, Seq(5, 6, 7, 8))
              }
          }
      }
    }
  }

  // ------------------------------------------------------------------
  // Parte 2: L1Cache (ICache + DCache) sobre memoria lenta
  // ------------------------------------------------------------------

  private case class Op(addr: Long, write: Boolean = false, data: BigInt = 0)
  private case class Done(data: BigInt, cycles: Int)

  private val DataBase = 0x40L // set 4 (indice), longe da regiao de codigo
  private val WayStride =
    CacheParams.numSets.toLong * CacheParams.lineBytes // mesmo set, tag diferente
  // Cada compilacao do treadle e cara e cresce com a memoria (um Reg por palavra),
  // entao ha dois tamanhos: o pequeno para a maioria dos cenarios e o grande so
  // para os que precisam de 5 tags no mesmo set (despejo).
  private val SmallWords = 1024 // 4 KiB
  private val MemWords = 4096 // 16 KiB: cabe DataBase + 5 * WayStride

  private def code(n: Int): Long =
    0x13L + (n.toLong << 7) // palavras de "codigo" distintas
  private val codeWords: Seq[Long] = (0 until 16).map(code)

  private def newSystem(latency: Int, words: Int) =
    new L1WithMemory(latency, words, codeWords)

  /** Reinicia o DUT (caches, memoria e FSMs voltam ao estado inicial) para
    * reaproveitar uma unica compilacao em varios cenarios independentes.
    */
  private def resetSystem(dut: L1WithMemory): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    initCpu(dut)
  }

  private def initCpu(dut: L1WithMemory): Unit = {
    for (port <- Seq(dut.io.iCache, dut.io.dCache)) {
      port.request.poke(false.B)
      port.write.poke(false.B)
      port.address.poke(0.U)
      port.writeData.poke(0.U)
      port.memSize.poke(2.U)
      port.unsignedLoad.poke(false.B)
    }
    dut.io.fenceI.poke(false.B)
  }

  /** Conduz um pedido na ICache e/ou na DCache ao mesmo tempo, mantendo cada
    * pedido ate o respectivo hit (contrato da cache). Devolve ciclos e dado.
    */
  private def drive(
      dut: L1WithMemory,
      i: Option[Op],
      d: Option[Op],
      maxCycles: Int = 600
  ): (Option[Done], Option[Done]) = {
    def issue(port: CpuCacheIO, op: Option[Op]): Unit = op match {
      case Some(o) =>
        port.request.poke(true.B)
        port.write.poke(o.write.B)
        port.address.poke(BigInt(o.addr).U(32.W))
        port.writeData.poke(o.data.U(32.W))
      case None => port.request.poke(false.B)
    }
    issue(dut.io.iCache, i)
    issue(dut.io.dCache, d)

    var iDone: Option[Done] = None
    var dDone: Option[Done] = None
    var cycle = 0
    while ((i.isDefined && iDone.isEmpty) || (d.isDefined && dDone.isEmpty)) {
      require(cycle < maxCycles, s"timeout: cache travada (i=$i d=$d)")
      if (i.isDefined && iDone.isEmpty && dut.io.iCache.hit.peek().litToBoolean)
        iDone = Some(Done(dut.io.iCache.readData.peek().litValue, cycle + 1))
      if (d.isDefined && dDone.isEmpty && dut.io.dCache.hit.peek().litToBoolean)
        dDone = Some(Done(dut.io.dCache.readData.peek().litValue, cycle + 1))
      dut.clock.step(1)
      cycle += 1
      if (iDone.isDefined) dut.io.iCache.request.poke(false.B)
      if (dDone.isDefined) dut.io.dCache.request.poke(false.B)
    }
    dut.io.iCache.request.poke(false.B)
    dut.io.dCache.request.poke(false.B)
    dut.io.dCache.write.poke(false.B)
    (iDone, dDone)
  }

  private def fetch(dut: L1WithMemory, addr: Long): Done =
    drive(dut, Some(Op(addr)), None)._1.get
  private def load(dut: L1WithMemory, addr: Long): Done =
    drive(dut, None, Some(Op(addr)))._2.get
  private def store(dut: L1WithMemory, addr: Long, data: BigInt): Done =
    drive(dut, None, Some(Op(addr, write = true, data = data)))._2.get

  /** FENCE.I: devolve quantos ciclos ate fenceIDone. */
  private def fenceI(dut: L1WithMemory): Int = {
    dut.io.fenceI.poke(true.B)
    var c = 0
    while (!dut.io.fenceIDone.peek().litToBoolean) {
      require(c < 2000, "timeout: FENCE.I travado")
      dut.clock.step(1)
      c += 1
    }
    dut.io.fenceI.poke(false.B)
    dut.clock.step(2)
    c
  }

  behavior of "L1Cache sobre CacheMainMemory com latencia"

  for (latency <- Latencies) {
    it should s"cobrir miss, hit, miss simultaneo e FENCE.I (latencia $latency)" in {
      test(newSystem(latency, SmallWords)) { dut =>
        def scenario(name: String)(body: L1WithMemory => Unit): Unit = {
          resetSystem(dut)
          body(dut)
        }

        scenario(
          s"demorar pelo menos $latency ciclos num miss e acertar sem memoria depois"
        ) { dut =>
          {
            initCpu(dut)
            val miss = fetch(dut, 0x4)
            assert(miss.data == BigInt(code(1)))
            assert(
              miss.cycles > latency,
              s"miss de ${miss.cycles} ciclos com latencia $latency"
            )

            val hit = fetch(dut, 0x8)
            assert(hit.data == BigInt(code(2)))
            assert(
              hit.cycles == 2,
              "hit = Idle + CompareTag, independente da latencia"
            )

            val dMiss = load(dut, 0x14)
            assert(dMiss.data == BigInt(code(5)))
            assert(dMiss.cycles > latency)
          }
        }

        scenario(
          s"atender miss simultaneo de ICache e DCache em serie (latencia $latency)"
        ) { dut =>
          {
            initCpu(dut)
            // DCache le a linha 0x10 (code 4..7), ICache a linha 0x20 (code 8..11)
            val (i, d) = drive(dut, Some(Op(0x28)), Some(Op(0x14)))
            assert(d.get.data == BigInt(code(5)))
            assert(i.get.data == BigInt(code(10)))
            assert(d.get.cycles > latency)
            assert(
              i.get.cycles >= d.get.cycles + latency,
              s"ICache (${i.get.cycles}) deveria terminar ao menos $latency ciclos apos a DCache (${d.get.cycles})"
            )
          }
        }

        scenario(
          s"ler na ICache o dado gravado pela DCache apos FENCE.I (latencia $latency)"
        ) { dut =>
          {
            initCpu(dut)
            store(
              dut,
              0x44,
              0xcafe0001L
            ) // write-allocate: linha fica suja na DCache
            store(dut, 0x4c, 0xcafe0002L)

            val cycles =
              fenceI(dut) // precisa drenar a linha suja pela memoria lenta
            assert(
              cycles >= latency,
              s"FENCE.I terminou em $cycles ciclos, antes da latencia"
            )

            assert(fetch(dut, 0x44).data == BigInt(0xcafe0001L))
            assert(fetch(dut, 0x4c).data == BigInt(0xcafe0002L))
            assert(
              fetch(dut, 0x40).data == BigInt(0),
              "palavra nao escrita continua zero"
            )
          }
        }
      }
    }
  }

  it should "cobrir despejo com write-back e trafego aleatorio (memoria grande, latencia 3)" in {
    val L = 3
    test(newSystem(L, MemWords)) { dut =>
      def scenario(name: String)(body: L1WithMemory => Unit): Unit = {
        resetSystem(dut)
        body(dut)
      }

      scenario(
        s"entregar a ICache o dado de um write-back por despejo (latencia $L)"
      ) { dut =>
        {
          initCpu(dut)
          store(dut, DataBase + 4, 0xbeef0001L) // linha suja, depois LRU
          for (k <- 1 to 3)
            load(dut, DataBase + k * WayStride) // enche as 4 vias

          // Quinta tag no mesmo set: despeja a linha suja (write-back) e, ao
          // mesmo tempo, a ICache pede essa mesma linha. A DCache tem
          // prioridade, entao a ICache so pode ser servida depois do write-back.
          val (i, d) = drive(
            dut,
            Some(Op(DataBase + 4)),
            Some(Op(DataBase + 4 * WayStride))
          )
          assert(d.get.data == BigInt(0))
          assert(
            i.get.data == BigInt(0xbeef0001L),
            s"ICache leu dado velho da memoria: 0x${i.get.data.toString(16)}"
          )
          assert(
            d.get.cycles >= 2 * L,
            "despejo sujo = write-back + refill"
          )
        }
      }

      scenario(
        "manter DCache e memoria consistentes sob trafego aleatorio com memoria lenta"
      ) { dut =>
        {
          initCpu(dut)
          val rnd = new Random(1234)
          val model =
            scala.collection.mutable.Map[Long, BigInt]() // endereco -> palavra
          val bases = Seq(DataBase, DataBase + 0x10) // dois sets
          def randomAddr(): Long =
            bases(rnd.nextInt(bases.size)) + rnd.nextInt(6) * WayStride + rnd
              .nextInt(4) * 4L

          for (_ <- 0 until 30) {
            val iAddr = rnd.nextInt(16) * 4L // regiao de codigo, so leitura
            val dOp =
              if (rnd.nextBoolean())
                Op(
                  randomAddr(),
                  write = true,
                  data = BigInt(rnd.nextInt() & 0xffffffffL)
                )
              else Op(randomAddr())
            val withFetch = rnd.nextInt(3) != 0
            val (i, d) =
              drive(dut, if (withFetch) Some(Op(iAddr)) else None, Some(dOp))

            if (withFetch)
              assert(
                i.get.data == BigInt(code((iAddr / 4).toInt)),
                s"fetch 0x${iAddr.toHexString}"
              )
            if (dOp.write) model(dOp.addr) = dOp.data
            else
              assert(
                d.get.data == model.getOrElse(dOp.addr, BigInt(0)),
                s"load 0x${dOp.addr.toHexString}"
              )
          }

          // Depois do FENCE.I, tudo o que a DCache escreveu precisa estar na memoria
          // principal, visivel pela ICache.
          fenceI(dut)
          for ((addr, value) <- model) {
            assert(
              fetch(dut, addr).data == value,
              s"memoria divergente em 0x${addr.toHexString}"
            )
          }
        }
      }
    }
  }
}
