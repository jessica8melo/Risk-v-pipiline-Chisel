package riscv.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** Canal de erro da memoria (`CacheMemIO.fault`) e sua propagacao ate a CPU
  * (`CpuCacheIO.fault`). Antes dele um pedido invalido deixava a cache
  * esperando `ready` para sempre.
  *
  * Cada DUT roda varios cenarios, reiniciados por reset, para manter o numero
  * de compilacoes (o que mais pesa no treadle) baixo.
  */
class CacheFaultTest extends AnyFlatSpec with ChiselScalatestTester {

  private val W = CacheParams.wordsPerLine
  private val WayStride =
    CacheParams.numSets * CacheParams.lineBytes // mesmo set

  private def reset(dut: Module): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  // ------------------------------------------------------------------
  // Parte 1: Cache (FSM) com a memoria dirigida na mao
  // ------------------------------------------------------------------

  private def initCache(dut: Cache): Unit = {
    reset(dut)
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.io.cpu.address.poke(0.U)
    dut.io.cpu.writeData.poke(0.U)
    dut.io.cpu.memSize.poke(2.U)
    dut.io.cpu.unsignedLoad.poke(false.B)
    dut.io.mem.ready.poke(false.B)
    dut.io.mem.fault.poke(false.B)
    for (i <- 0 until W) dut.io.mem.readLine(i).poke(0.U)
    dut.io.flushDirty.poke(false.B)
    dut.io.invalidateAll.poke(false.B)
  }

  private def issue(
      dut: Cache,
      addr: Long,
      write: Boolean = false,
      data: Long = 0
  ): Unit = {
    dut.io.cpu.request.poke(true.B)
    dut.io.cpu.write.poke(write.B)
    dut.io.cpu.address.poke(addr.U)
    dut.io.cpu.writeData.poke(data.U)
  }

  private def drop(dut: Cache): Unit = {
    dut.io.cpu.request.poke(false.B)
    dut.io.cpu.write.poke(false.B)
    dut.clock.step(1)
  }

  /** Avanca ate a cache pedir `write` (write-back) ou refill a memoria. */
  private def waitMemRequest(
      dut: Cache,
      write: Boolean,
      max: Int = 20
  ): Unit = {
    var c = 0
    while (
      !(dut.io.mem.request
        .peekBoolean() && dut.io.mem.write.peekBoolean() == write)
    ) {
      require(
        c < max,
        s"a cache nao pediu ${if (write) "write-back" else "refill"}"
      )
      dut.clock.step(1)
      c += 1
    }
  }

  /** A memoria recusa o pedido em curso por um ciclo. */
  private def injectFault(dut: Cache): Unit = {
    dut.io.mem.fault.poke(true.B)
    dut.clock.step(1)
    dut.io.mem.fault.poke(false.B)
  }

  /** Atende o pedido corrente da CPU com memoria de latencia zero ate o hit. */
  private def serveUntilHit(
      dut: Cache,
      line: Seq[Long] = Seq.fill(W)(0L)
  ): Unit = {
    var c = 0
    while (!dut.io.cpu.hit.peekBoolean()) {
      require(c < 40, "timeout esperando hit")
      require(!dut.io.cpu.fault.peekBoolean(), "fault inesperado")
      if (dut.io.mem.request.peekBoolean()) {
        for (i <- 0 until W) dut.io.mem.readLine(i).poke(line(i).U)
        dut.io.mem.ready.poke(true.B)
      } else dut.io.mem.ready.poke(false.B)
      dut.clock.step(1)
      dut.io.mem.ready.poke(false.B)
      c += 1
    }
  }

  private def finishAccess(dut: Cache): Unit = {
    dut.clock.step(1) // consome o ciclo do hit
    drop(dut)
  }

  behavior of "Cache (DCache) com fault da memoria"

  it should "cobrir refill, write-back e flush recusados pela memoria" in {
    test(new Cache) { dut =>
      // --- refill recusado: fault sticky, nada e preenchido, a CPU se recupera
      initCache(dut)
      issue(dut, 0x100)
      waitMemRequest(dut, write = false)
      dut.io.cpu.fault.expect(false.B, "ainda nao falhou")
      injectFault(dut)
      for (_ <- 0 until 4) { // nivel: segue ate a CPU derrubar o pedido
        dut.io.cpu.fault.expect(true.B)
        dut.io.cpu.hit.expect(false.B)
        dut.io.cpu.stall.expect(false.B)
        dut.io.mem.request.expect(false.B, "nao deve insistir na memoria")
        dut.clock.step(1)
      }
      drop(dut)
      dut.io.cpu.fault.expect(false.B, "fault cai quando o pedido cai")

      // Nova tentativa: precisa ir de novo a memoria (a linha nao foi preenchida)
      issue(dut, 0x100)
      waitMemRequest(dut, write = false)
      serveUntilHit(dut, Seq(0x11, 0x22, 0x33, 0x44))
      dut.io.cpu.readData.expect(0x11.U)
      finishAccess(dut)

      // --- write-back recusado: a vitima continua suja e e regravada depois
      initCache(dut)
      issue(dut, 0x40, write = true, data = 0xaa)
      serveUntilHit(dut)
      finishAccess(dut)
      for (k <- 1 to 3) { // enche as 4 vias do set
        issue(dut, 0x40L + k * WayStride)
        serveUntilHit(dut)
        finishAccess(dut)
      }
      issue(dut, 0x40L + 4 * WayStride) // 5a tag: despeja a linha suja
      waitMemRequest(dut, write = true)
      dut.io.mem.writeLine(0).expect(0xaa.U)
      injectFault(dut)
      dut.io.cpu.fault.expect(true.B)
      dut.io.cpu.hit.expect(false.B)
      drop(dut)

      issue(dut, 0x40L + 4 * WayStride)
      waitMemRequest(dut, write = true) // tenta de novo o write-back
      dut.io.mem
        .writeLine(0)
        .expect(0xaa.U, "dado sujo nao pode ter sido perdido")
      serveUntilHit(dut)
      finishAccess(dut)

      // --- flush recusado: aborta, sinaliza flushFault e nao trava
      initCache(dut)
      issue(dut, 0x40, write = true, data = 0xbb)
      serveUntilHit(dut)
      finishAccess(dut)
      dut.io.flushDirty.poke(true.B)
      waitMemRequest(dut, write = true)
      injectFault(dut)
      var c = 0
      while (!dut.io.flushDone.peekBoolean()) {
        require(c < 20, "flush ficou travado apos o fault")
        dut.clock.step(1)
        c += 1
      }
      dut.io.flushFault.expect(true.B)
      dut.io.flushDirty.poke(false.B)
      dut.clock.step(2)

      // novo flush, agora sem erro: conclui e limpa flushFault
      dut.io.flushDirty.poke(true.B)
      c = 0
      while (!dut.io.flushDone.peekBoolean()) {
        require(c < 400, "flush travado") // varre os 128 sets
        if (dut.io.mem.request.peekBoolean()) dut.io.mem.ready.poke(true.B)
        dut.clock.step(1)
        dut.io.mem.ready.poke(false.B)
        c += 1
      }
      dut.io.flushFault.expect(false.B)
    }
  }

  behavior of "Cache (ICache) com fault da memoria"

  it should "sinalizar fault no refill recusado e voltar a buscar depois" in {
    test(new Cache(isInstructionCache = true)) { dut =>
      initCache(dut)
      issue(dut, 0x200)
      waitMemRequest(dut, write = false)
      injectFault(dut)
      dut.io.cpu.fault.expect(true.B)
      dut.io.cpu.hit.expect(false.B)
      drop(dut)
      dut.io.cpu.fault.expect(false.B)

      issue(dut, 0x0)
      waitMemRequest(dut, write = false)
      serveUntilHit(dut, Seq(0x13, 0, 0, 0))
      dut.io.cpu.readData.expect(0x13.U)
    }
  }

  // ------------------------------------------------------------------
  // Parte 2: CacheMainMemory recusa enderecos fora da capacidade
  // ------------------------------------------------------------------

  private val memWords = 64 // 256 B: 0x100 em diante e invalido
  private val memData = Seq(1L, 2L, 3L, 4L)

  private def initMem(dut: CacheMainMemory): Unit = {
    reset(dut)
    for (p <- Seq(dut.io.iMem, dut.io.dMem)) {
      p.request.poke(false.B)
      p.write.poke(false.B)
      p.address.poke(0.U)
      for (w <- 0 until W) p.writeLine(w).poke(0.U)
    }
  }

  /** Espera `ready` ou `fault`; devolve (ciclos, fault). */
  private def waitDone(dut: CacheMainMemory, p: CacheMemIO): (Int, Boolean) = {
    var c = 0
    while (!p.ready.peekBoolean() && !p.fault.peekBoolean()) {
      require(c < 50, "memoria nao respondeu (nem ready nem fault)")
      dut.clock.step(1)
      c += 1
    }
    require(
      !(p.ready.peekBoolean() && p.fault.peekBoolean()),
      "ready e fault juntos"
    )
    (c, p.fault.peekBoolean())
  }

  for (latency <- Seq(0, 3)) {
    behavior of s"CacheMainMemory (latencia $latency) com enderecos invalidos"

    it should s"responder fault no lugar de ready, sem escrever e sem travar (latencia $latency)" in {
      test(
        new CacheMainMemory(memWords, memData, latency = latency)
      ) { dut =>
        // leitura fora da capacidade pela ICache e pela DCache
        for (p <- Seq(dut.io.iMem, dut.io.dMem)) {
          initMem(dut)
          p.request.poke(true.B)
          p.address.poke(0x100.U)
          val (cycles, fault) = waitDone(dut, p)
          assert(fault, "pedido fora da capacidade deve dar fault")
          assert(
            cycles == latency,
            s"fault apos $cycles ciclos, esperado $latency"
          )
          p.ready.expect(false.B)
          for (w <- 0 until W) p.readLine(w).expect(0.U)
        }

        // escrita fora da capacidade: fault e nada e gravado no inicio da memoria
        initMem(dut)
        dut.io.dMem.request.poke(true.B)
        dut.io.dMem.write.poke(true.B)
        dut.io.dMem.address.poke(0x100.U)
        for (w <- 0 until W) dut.io.dMem.writeLine(w).poke(0xff.U)
        assert(waitDone(dut, dut.io.dMem)._2)
        dut.clock.step(1)
        dut.io.dMem.request.poke(false.B)
        dut.io.dMem.write.poke(false.B)
        dut.clock.step(1)

        // a memoria nao ficou travada e o inicio nao foi sobrescrito (wrap)
        dut.io.iMem.request.poke(true.B)
        dut.io.iMem.address.poke(0.U)
        val (_, fault) = waitDone(dut, dut.io.iMem)
        assert(!fault, "endereco valido nao pode dar fault")
        for (w <- 0 until W) dut.io.iMem.readLine(w).expect(memData(w).U)
      }
    }
  }

  // ------------------------------------------------------------------
  // Parte 3: L1Cache + memoria lenta (fault ate a CPU, sem deadlock)
  // ------------------------------------------------------------------

  private def initCpu(dut: L1WithMemory): Unit = {
    reset(dut)
    for (p <- Seq(dut.io.iCache, dut.io.dCache)) {
      p.request.poke(false.B)
      p.write.poke(false.B)
      p.address.poke(0.U)
      p.writeData.poke(0.U)
      p.memSize.poke(2.U)
      p.unsignedLoad.poke(false.B)
    }
    dut.io.fenceI.poke(false.B)
  }

  private def stepUntil(what: String, max: Int = 80)(
      cond: => Boolean
  )(dut: L1WithMemory): Int = {
    var c = 0
    while (!cond) {
      require(c < max, s"timeout: $what")
      dut.clock.step(1)
      c += 1
    }
    c
  }

  behavior of "L1Cache sobre memoria lenta com acesso invalido"

  it should "propagar fault a CPU, nao travar e atender acessos validos em seguida" in {
    val latency = 3
    test(
      new L1WithMemory(latency, memWords, Seq(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L))
    ) { dut =>
      // ICache: busca fora da memoria
      initCpu(dut)
      dut.io.iCache.request.poke(true.B)
      dut.io.iCache.address.poke(0x100.U)
      val cycles =
        stepUntil("fault da ICache")(dut.io.iCache.fault.peekBoolean())(dut)
      assert(cycles >= latency, "fault antes da latencia da memoria")
      dut.io.iCache.hit.expect(false.B)
      dut.clock.step(3)
      dut.io.iCache.fault
        .expect(true.B, "fault e nivel ate a CPU derrubar o pedido")
      dut.io.iCache.request.poke(false.B)
      dut.clock.step(2)
      dut.io.iCache.fault.expect(false.B)

      // busca valida logo depois
      dut.io.iCache.request.poke(true.B)
      dut.io.iCache.address.poke(0x0.U)
      stepUntil("hit da ICache")(dut.io.iCache.hit.peekBoolean())(dut)
      dut.io.iCache.readData.expect(1.U)
      dut.io.iCache.request.poke(false.B)
      dut.clock.step(2)

      // DCache: load e store fora da memoria
      for (write <- Seq(false, true)) {
        dut.io.dCache.request.poke(true.B)
        dut.io.dCache.write.poke(write.B)
        dut.io.dCache.address.poke(0x200.U)
        dut.io.dCache.writeData.poke(0x55.U)
        stepUntil(s"fault da DCache (write=$write)")(
          dut.io.dCache.fault.peekBoolean()
        )(dut)
        dut.io.dCache.hit.expect(false.B)
        dut.io.dCache.request.poke(false.B)
        dut.io.dCache.write.poke(false.B)
        dut.clock.step(2)
        dut.io.dCache.fault.expect(false.B)
      }

      // fault da DCache nao bloqueia a ICache: pedidos simultaneos
      dut.io.dCache.request.poke(true.B)
      dut.io.dCache.address.poke(0x200.U)
      dut.io.iCache.request.poke(true.B)
      dut.io.iCache.address.poke(0x10.U)
      stepUntil("hit da ICache com a DCache em fault")(
        dut.io.iCache.hit.peekBoolean()
      )(dut)
      dut.io.iCache.readData.expect(5.U)
      dut.io.dCache.fault.expect(true.B)
      dut.io.iCache.request.poke(false.B)
      dut.io.dCache.request.poke(false.B)
      dut.clock.step(2)

      // e a DCache continua funcionando para enderecos validos
      dut.io.dCache.request.poke(true.B)
      dut.io.dCache.address.poke(0x4.U)
      stepUntil("load valido da DCache")(dut.io.dCache.hit.peekBoolean())(dut)
      dut.io.dCache.readData.expect(2.U)
    }
  }
}
