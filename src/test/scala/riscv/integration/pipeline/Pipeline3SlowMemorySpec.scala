package riscv.integration.pipeline

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import riscv.cache.CacheConfig
import riscv.pipeline.Pipeline3

/** Pipeline3 completo sobre uma memoria principal lenta (`memoryLatency`).
  *
  * Exercita o `cacheWait` segurando o pipeline: misses de ICache a cada linha,
  * misses de DCache, load-use depois de miss, LR/SC e AMO com miss e traps de
  * desalinhamento chegando enquanto a busca da proxima instrucao ainda espera a
  * memoria. Todos os cenarios rodam num unico programa (uma compilacao por
  * latencia) e a latencia 0 serve de controle: se um cenario falha nas duas, o
  * erro e do teste; se so falha na lenta, e do pipeline/cache.
  */
class Pipeline3SlowMemorySpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Pipeline3 com memoria lenta"

  private val Latencies = Seq(0, 4)

  // ---- codificacao ------------------------------------------------------
  private def addi(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x13L
  private def csrrw(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x1L << 12) | (rd.toLong << 7) | 0x73L
  private def csrrs(rd: Int, csr: Int, rs1: Int): Long =
    (csr.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x73L
  private def lw(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) | (rd.toLong << 7) | 0x03L
  private def sw(rs2: Int, rs1: Int, imm: Int): Long =
    (((imm >> 5) & 0x7f).toLong << 25) | (rs2.toLong << 20) | (rs1.toLong << 15) |
      (0x2L << 12) | ((imm & 0x1f).toLong << 7) | 0x23L
  private def atomic(funct5: Int, rd: Int, rs1: Int, rs2: Int): Long =
    (funct5.toLong << 27) | (rs2.toLong << 20) | (rs1.toLong << 15) | (0x2L << 12) |
      (rd.toLong << 7) | 0x2fL
  private def jalr(rd: Int, rs1: Int, imm: Int): Long =
    ((imm & 0xfff).toLong << 20) | (rs1.toLong << 15) | (rd.toLong << 7) | 0x67L
  private def lui(rd: Int, imm20: Int): Long =
    ((imm20 & 0xfffff).toLong << 12) | (rd.toLong << 7) | 0x37L
  private def jal(rd: Int, off: Int): Long =
    (((off >> 20) & 1).toLong << 31) | (((off >> 1) & 0x3ff).toLong << 21) |
      (((off >> 11) & 1).toLong << 20) | (((off >> 12) & 0xff).toLong << 12) |
      (rd.toLong << 7) | 0x6fL
  private val FENCE_I = 0x0000100fL
  // RVC: c.addi rd, imm (c.nop = c.addi x0, 0) e parcelas de 16 bits
  private def cAddi(rd: Int, imm: Int): Int =
    (((imm >> 5) & 1) << 12) | (rd << 7) | ((imm & 0x1f) << 2) | 0x1
  private val cNop = cAddi(0, 0)
  private def cLi(rd: Int, imm: Int): Int = // c.li rd, imm
    (0x2 << 13) | (((imm >> 5) & 1) << 12) | (rd << 7) | ((imm & 0x1f) << 2) | 0x1
  private def parcels32(i: Long): Seq[Int] =
    Seq((i & 0xffff).toInt, ((i >> 16) & 0xffff).toInt)
  private def packParcels(p: Seq[Int]): Seq[Long] = {
    require(p.size % 2 == 0, "numero impar de parcelas")
    p.grouped(2).map(g => (g(0) & 0xffffL) | ((g(1) & 0xffffL) << 16)).toSeq
  }
  private val NOP = 0x00000013L
  private val jal0 = 0x0000006fL // jal x0, 0

  private def lrw(rd: Int, rs1: Int) = atomic(0x02, rd, rs1, 0)
  private def scw(rd: Int, rs1: Int, rs2: Int) = atomic(0x03, rd, rs1, rs2)
  private def amoadd(rd: Int, rs1: Int, rs2: Int) = atomic(0x00, rd, rs1, rs2)

  // ---- cenarios ---------------------------------------------------------
  /** Cenario reto: `code` e seguido do marcador (x9 = indice + 1). */
  private case class Plain(name: String, code: Seq[Long])

  /** Cenario com trap: `pre` prepara os registradores; a instrucao `tested` e
    * alinhada em PC % 16 == 12, de modo que a busca da instrucao seguinte cai
    * numa linha fria da ICache (miss) enquanto `tested` esta no EX. O handler
    * volta ao marcador via x11.
    */
  private case class Trapping(name: String, pre: Seq[Long], tested: Long)

  /** Cenario reto em que `tested` e alinhada em PC % 16 == `offset`, para
    * variar em que ponto da busca (linha fria da ICache) a instrucao cai.
    */
  private case class AlignedAt(
      name: String,
      pre: Seq[Long],
      tested: Long,
      post: Seq[Long],
      offset: Int
  )

  /** LR.W (carrega a linha na DCache e reserva) + SC.W alinhado em `offset`. O
    * hit da DCache do SC pode cair no meio de um miss da ICache (busca das
    * instrucoes seguintes), quando `exCanCommit` ainda e falso: a reserva nao
    * pode ser apagada pelo proprio SC antes do commit.
    */
  private def scAt(offset: Int, addr: Int) =
    AlignedAt(
      s"sc-align-$offset",
      Seq(addi(2, 0, addr), addi(6, 0, 0x77), lrw(3, 2)),
      scw(4, 2, 6),
      Seq(lw(5, 2, 0)),
      offset
    )

  /** Cenario gerado a partir do PC onde comeca (alinhado a `align` bytes). */
  private case class Placed(name: String, align: Int, build: Int => Seq[Long])

  /** RVC cruzando linhas de cache. Comeca alinhado a 16 bytes; instrucoes de 32
    * bits nos offsets 14, 30 e 110 (PC % 16 == 14) ocupam duas linhas, entao a
    * segunda metade so chega depois de um miss de ICache. O trecho final e
    * alcancado por jalr (redirect para PC % 16 == 14 com a ICache fria).
    */
  private val rvcTargetOffset = 0x6e
  private def rvcCross(start: Int): Seq[Long] = {
    val p = scala.collection.mutable.ArrayBuffer.empty[Int]
    p += cLi(10, 0) // x10 = 0 (os cenarios anteriores deixam lixo em x10)
    for (_ <- 0 until 6) p += cAddi(10, 1) // x10 = 1..6, termina no offset 14
    p ++= parcels32(addi(11, 0, 0x2a)) // cruza a fronteira da linha (14..17)
    for (_ <- 0 until 6) p += cAddi(10, 1) // x10 = 7..12, termina no offset 30
    p ++= parcels32(addi(12, 0, 0x3b)) // cruza a fronteira da linha (30..33)
    p ++= parcels32(addi(2, 0, start + rvcTargetOffset))
    p ++= parcels32(jalr(0, 2, 0)) // redirect para o offset 0x6e
    while (p.size * 2 < rvcTargetOffset) p += cNop // nunca executado
    p ++= parcels32(addi(13, 0, 0x55)) // 0x6e..0x71: cruza a linha
    if (p.size % 2 != 0) p += cNop
    packParcels(p.toSeq)
  }

  /** Codigo automodificavel: chama a sub-rotina em T, troca a instrucao dela
    * com SW (linha suja na DCache), FENCE.I e chama de novo. Sem o flush da
    * DCache e a invalidacao da ICache o 2o chamado devolveria o valor velho.
    */
  private def fenceISmc(start: Int): Seq[Long] = {
    val newInstr = addi(5, 0, 2) // 0x00200293
    require(
      (newInstr & 0xfff) < 0x800,
      "addi do imediato nao pode vir negativo"
    )
    val head = 8 // palavras antes do preenchimento
    val pad = ((16 - (start + head * 4) % 16) % 16) / 4
    val t = start + (head + pad) * 4 // sub-rotina alinhada a uma linha
    val skip = (t + 16) - (start + (head - 1) * 4) // jal sobre a linha de T
    Seq(
      lui(6, (newInstr >> 12).toInt),
      addi(6, 6, (newInstr & 0xfff).toInt),
      addi(2, 0, t),
      jalr(1, 2, 0), // 1a chamada: x5 = 1
      sw(6, 2, 0), // reescreve addi x5,x0,1 -> addi x5,x0,2
      FENCE_I,
      jalr(1, 2, 0), // 2a chamada: x5 = 2
      jal(0, skip)
    ) ++ Seq.fill(pad)(NOP) ++ Seq(
      addi(5, 0, 1), // T
      jalr(0, 1, 0),
      NOP,
      NOP
    )
  }

  private val DataAddr = 0x300 // regiao de dados, longe do codigo
  private val ChainSums = Seq(1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78)

  private val scenarios: Seq[Any] = Seq(
    Plain("cadeia", (1 to 12).map(i => addi(10, 10, i))),
    Plain(
      "sw-lw-loaduse",
      Seq(
        addi(2, 0, 0x300),
        addi(6, 0, 0x5a),
        sw(6, 2, 0),
        lw(3, 2, 0),
        addi(4, 3, 1) // load-use logo apos um miss de DCache
      )
    ),
    Plain(
      "duas-linhas",
      Seq(
        addi(2, 0, 0x310),
        addi(12, 0, 0x11),
        sw(12, 2, 0),
        addi(13, 0, 0x22),
        sw(13, 2, 16),
        lw(14, 2, 16),
        lw(15, 2, 0)
      )
    ),
    Plain(
      "amoadd",
      Seq(
        addi(2, 0, 0x330),
        addi(6, 0, 7),
        sw(6, 2, 0),
        addi(6, 0, 5),
        amoadd(3, 2, 6),
        lw(4, 2, 0)
      )
    ),
    Plain(
      "lr-sc",
      Seq(
        addi(2, 0, 0x340),
        addi(6, 0, 0x77),
        lrw(3, 2),
        scw(4, 2, 6),
        lw(5, 2, 0)
      )
    ),
    Plain(
      "sc-sem-reserva",
      Seq(addi(2, 0, 0x350), addi(6, 0, 0x66), scw(4, 2, 6), lw(5, 2, 0))
    ),
    Trapping("trap-lw", Seq(addi(2, 0, 0x361)), lw(3, 2, 0)),
    Trapping("trap-sw", Seq(addi(2, 0, 0x362), addi(6, 0, 0x99)), sw(6, 2, 0)),
    Plain("sw-nao-escreveu", Seq(addi(2, 0, 0x360), lw(3, 2, 0))),
    Trapping(
      "trap-amo",
      Seq(addi(2, 0, 0x372), addi(6, 0, 1)),
      amoadd(3, 2, 6)
    ),
    scAt(0, 0x380),
    scAt(4, 0x390),
    scAt(8, 0x3a0),
    scAt(12, 0x3b0),
    Placed("rvc-cruza-linha", 16, rvcCross),
    Placed("fence-i-smc", 4, fenceISmc)
  )

  private def nameOf(s: Any): String = s match {
    case p: Plain     => p.name
    case t: Trapping  => t.name
    case a: AlignedAt => a.name
    case x: Placed    => x.name
  }
  private def idx(name: String): Int = scenarios.indexWhere(nameOf(_) == name)

  private case class Layout(program: Seq[Long], testedPc: Map[String, Int])

  private def layout(handlerPc: Int): Layout = {
    val out = ArrayBuffer[Long](
      addi(1, 0, handlerPc), // mtvec = handler
      csrrw(0, 0x305, 1)
    )
    var testedPc = Map.empty[String, Int]
    def pc = out.size * 4
    for ((sc, k) <- scenarios.zipWithIndex) sc match {
      case Plain(_, code) =>
        out ++= code
        out += addi(9, 0, k + 1)
      case Placed(_, align, build) =>
        while (pc % align != 0) out += NOP
        out ++= build(pc)
        out += addi(9, 0, k + 1)
      case AlignedAt(_, pre, tested, post, offset) =>
        out ++= pre
        while (pc % 16 != offset) out += NOP
        out += tested
        out ++= post
        out += addi(9, 0, k + 1)
      case Trapping(name, pre, tested) =>
        out ++= pre
        val withX11 = pc + 4 // o addi de x11 conta
        val pad = (((12 - withX11 % 16) % 16) + 16) % 16 / 4
        val tPc = withX11 + pad * 4
        out += addi(11, 0, tPc + 8) // marcador = depois de tested + addi x4
        for (_ <- 0 until pad) out += NOP
        require(pc == tPc)
        out += tested
        out += addi(4, 0, 1) // nao pode executar apos o trap
        out += addi(9, 0, k + 1)
        testedPc += name -> tPc
    }
    out += jal0
    for (_ <- 0 until 3) out += NOP
    require(
      pc == handlerPc || handlerPc == 0,
      s"handler em 0x${pc.toHexString}"
    )
    out ++= Seq(
      csrrs(5, 0x342, 0), // x5 = mcause
      csrrs(7, 0x343, 0), // x7 = mtval
      csrrs(8, 0x341, 0), // x8 = mepc
      jalr(0, 11, 0)
    )
    Layout(out.toSeq, testedPc)
  }

  private lazy val built: Layout = {
    val first = layout(0) // 1a passada so descobre onde o handler cai
    val handlerPc = (first.program.size - 4) * 4
    val l = layout(handlerPc)
    require(l.program.size * 4 <= DataAddr, "codigo invade a regiao de dados")
    l
  }

  // ---- simulacao --------------------------------------------------------
  private type Segment = Seq[(Int, BigInt)] // writebacks (rd != 0) em ordem

  // O custo do Treadle cresce com o quadrado do tamanho do design, e estes
  // cenarios testam latencia de memoria, nao geometria: usam uma cache pequena
  // (1 KiB, 16 conjuntos). O codigo (< 0x300 B) cabe na ICache e os enderecos de
  // dados (0x300..0x3bf) caem em conjuntos distintos.
  private val SmallCache =
    CacheConfig(capacityBytes = 1024, ways = 4, lineBytes = 16)

  private def runAll(latency: Int, maxCycles: Int = 8000): Seq[Segment] = {
    val log = ArrayBuffer.empty[(Int, BigInt)]
    test(
      new Pipeline3(
        initialProgram = built.program,
        memoryWords = 256,
        memoryLatency = latency,
        cacheConfig = SmallCache
      )
    ) { dut =>
      var cycle = 0
      while (cycle < maxCycles && !log.contains((9, BigInt(scenarios.size)))) {
        val rd = dut.io.writebackRd.peekInt().toInt
        if (dut.io.writebackEnable.peekBoolean() && rd != 0)
          log += ((rd, dut.io.writebackData.peekInt()))
        dut.clock.step(1)
        cycle += 1
      }
    }
    val segments = ArrayBuffer.empty[Segment]
    var current = ArrayBuffer.empty[(Int, BigInt)]
    for ((rd, data) <- log) {
      if (rd == 9 && data == BigInt(segments.size + 1)) {
        segments += current.toSeq
        current = ArrayBuffer.empty
      } else current += ((rd, data))
    }
    require(
      segments.size == scenarios.size,
      s"latencia $latency: so ${segments.size} de ${scenarios.size} cenarios terminaram"
    )
    segments.toSeq
  }

  // Try memoriza a falha tambem: se a simulacao quebrar, nao roda de novo.
  private lazy val results: Map[Int, Try[Seq[Segment]]] =
    Latencies.map(l => l -> Try(runAll(l))).toMap
  private def segment(latency: Int, name: String): Segment =
    results(latency).get(idx(name))
  private def writesOf(latency: Int, name: String, rd: Int): Seq[BigInt] =
    segment(latency, name).collect { case (`rd`, d) => d }

  private def expectTrap(
      latency: Int,
      name: String,
      cause: Int,
      tval: Int
  ): Unit = {
    assert(writesOf(latency, name, 5) == Seq(BigInt(cause)), s"$name: mcause")
    assert(writesOf(latency, name, 7) == Seq(BigInt(tval)), s"$name: mtval")
    assert(
      writesOf(latency, name, 8) == Seq(BigInt(built.testedPc(name))),
      s"$name: mepc (esperado 0x${built.testedPc(name).toHexString})"
    )
    assert(
      writesOf(latency, name, 3).isEmpty,
      s"$name: rd nao pode ser escrito"
    )
    assert(
      writesOf(latency, name, 4).isEmpty,
      s"$name: instrucao apos o trap executou"
    )
  }

  // ---- testes -----------------------------------------------------------
  for (lat <- Latencies) {
    it should s"executar cadeia de addi atravessando varias linhas, sem perder nem duplicar writebacks (latencia $lat)" in {
      assert(writesOf(lat, "cadeia", 10) == ChainSums.map(BigInt(_)))
    }

    it should s"entregar load-use correto depois de miss de DCache (latencia $lat)" in {
      assert(writesOf(lat, "sw-lw-loaduse", 3) == Seq(BigInt(0x5a)))
      assert(writesOf(lat, "sw-lw-loaduse", 4) == Seq(BigInt(0x5b)))
    }

    it should s"ler de volta duas linhas distintas escritas com miss (latencia $lat)" in {
      assert(writesOf(lat, "duas-linhas", 14) == Seq(BigInt(0x22)))
      assert(writesOf(lat, "duas-linhas", 15) == Seq(BigInt(0x11)))
    }

    it should s"fazer AMOADD com miss: devolve o valor antigo e grava a soma (latencia $lat)" in {
      assert(writesOf(lat, "amoadd", 3) == Seq(BigInt(7)))
      assert(writesOf(lat, "amoadd", 4) == Seq(BigInt(12)))
    }

    it should s"fazer LR/SC com sucesso e SC sem reserva falhar sem gravar (latencia $lat)" in {
      assert(writesOf(lat, "lr-sc", 3) == Seq(BigInt(0)))
      assert(
        writesOf(lat, "lr-sc", 4) == Seq(BigInt(0)),
        "SC com reserva deve devolver 0"
      )
      assert(writesOf(lat, "lr-sc", 5) == Seq(BigInt(0x77)))
      assert(
        writesOf(lat, "sc-sem-reserva", 4) == Seq(BigInt(1)),
        "SC sem LR deve devolver 1"
      )
      assert(
        writesOf(lat, "sc-sem-reserva", 5) == Seq(BigInt(0)),
        "SC falho nao pode gravar"
      )
    }

    it should s"fazer SC ter sucesso e gravar mesmo com o hit da DCache durante miss da ICache (latencia $lat)" in {
      for (off <- Seq(0, 4, 8, 12)) {
        val n = s"sc-align-$off"
        assert(writesOf(lat, n, 3) == Seq(BigInt(0)), s"$n: LR")
        assert(
          writesOf(lat, n, 4) == Seq(BigInt(0)),
          s"$n: SC com reserva valida deve devolver 0 (devolveu ${writesOf(lat, n, 4)})"
        )
        assert(
          writesOf(lat, n, 5) == Seq(BigInt(0x77)),
          s"$n: SC com sucesso deve ter gravado 0x77"
        )
      }
    }

    it should s"executar RVC cruzando fronteiras de linha com miss de ICache (latencia $lat)" in {
      val n = "rvc-cruza-linha"
      assert(
        writesOf(lat, n, 10) == (0 to 12).map(BigInt(_)),
        "x10 deve contar 0..12 sem perder nem repetir c.li/c.addi"
      )
      assert(writesOf(lat, n, 11) == Seq(BigInt(0x2a)), "32 bits em PC%16==14")
      assert(writesOf(lat, n, 12) == Seq(BigInt(0x3b)), "32 bits em PC%16==14")
      assert(
        writesOf(lat, n, 13) == Seq(BigInt(0x55)),
        "32 bits cruzando linha apos redirect com ICache fria"
      )
    }

    it should s"ver codigo reescrito apos FENCE.I (flush da DCache + invalidacao da ICache) (latencia $lat)" in {
      assert(
        writesOf(lat, "fence-i-smc", 5) == Seq(BigInt(1), BigInt(2)),
        "1a chamada deve dar 1 e, depois do FENCE.I, a 2a deve dar 2"
      )
    }

    it should s"tomar trap de desalinhamento com a busca seguinte ainda em miss (latencia $lat)" in {
      expectTrap(lat, "trap-lw", 4, 0x361)
      expectTrap(lat, "trap-sw", 6, 0x362)
      expectTrap(lat, "trap-amo", 6, 0x372)
    }

    it should s"nao gravar na memoria quando o SW desalinhado da trap (latencia $lat)" in {
      assert(writesOf(lat, "sw-nao-escreveu", 3) == Seq(BigInt(0)))
    }
  }
}
