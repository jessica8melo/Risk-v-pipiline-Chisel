package riscv.elementosbasicos

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** Wrapper de teste para acomodar o RvcDecompressor (RawModule) dentro do
  * ChiselScalatestTester, que exige [T <: Module].
  * O RvcDecompressor permanece puramente combinacional (RawModule) sem clock ou reset.
  */
class RvcDecompressorTestWrapper extends Module {
  val io = IO(new Bundle {
    val inst_c   = Input(UInt(16.W))
    val inst_out = Output(UInt(32.W))
    val illegal  = Output(Bool())
  })

  val decompressor = Module(new RvcDecompressor)
  decompressor.io.inst_c := io.inst_c
  io.inst_out            := decompressor.io.inst_out
  io.illegal             := decompressor.io.illegal
}

object RvcGoldenModel {
  val NOP: Long = 0x00000013L // addi x0, x0, 0
  val EBREAK: Long = 0x00100073L

  val SP: Int = 2
  val RA: Int = 1
  val X0: Int = 0

  val OPC_LOAD   = 0x03
  val OPC_OP_IMM = 0x13
  val OPC_STORE  = 0x23
  val OPC_OP     = 0x33
  val OPC_LUI    = 0x37
  val OPC_BRANCH = 0x63
  val OPC_JALR   = 0x67
  val OPC_JAL    = 0x6f

  val F3_ADD_SUB = 0
  val F3_SLL     = 1
  val F3_SLT     = 2
  val F3_SLTU    = 3
  val F3_XOR     = 4
  val F3_SRL_SRA = 5
  val F3_OR      = 6
  val F3_AND     = 7

  val F3_BEQ     = 0
  val F3_BNE     = 1

  val F3_LW      = 2
  val F3_SW      = 2

  val F7_ZERO    = 0x00
  val F7_ALT     = 0x20

  def mkR(funct7: Int, rs2: Int, rs1: Int, funct3: Int, rd: Int, opcode: Int): Long = {
    ((funct7.toLong & 0x7f) << 25) |
    ((rs2.toLong & 0x1f) << 20) |
    ((rs1.toLong & 0x1f) << 15) |
    ((funct3.toLong & 0x7) << 12) |
    ((rd.toLong & 0x1f) << 7) |
    (opcode.toLong & 0x7f)
  }

  def mkI(imm12: Int, rs1: Int, funct3: Int, rd: Int, opcode: Int): Long = {
    ((imm12.toLong & 0xfff) << 20) |
    ((rs1.toLong & 0x1f) << 15) |
    ((funct3.toLong & 0x7) << 12) |
    ((rd.toLong & 0x1f) << 7) |
    (opcode.toLong & 0x7f)
  }

  def mkS(imm12: Int, rs2: Int, rs1: Int, funct3: Int, opcode: Int): Long = {
    val imm = imm12.toLong & 0xfff
    (((imm >> 5) & 0x7f) << 25) |
    ((rs2.toLong & 0x1f) << 20) |
    ((rs1.toLong & 0x1f) << 15) |
    ((funct3.toLong & 0x7) << 12) |
    ((imm & 0x1f) << 7) |
    (opcode.toLong & 0x7f)
  }

  def mkB(imm13: Int, rs2: Int, rs1: Int, funct3: Int, opcode: Int): Long = {
    val imm = imm13.toLong & 0x1fff
    (((imm >> 12) & 0x1) << 31) |
    (((imm >> 5) & 0x3f) << 25) |
    ((rs2.toLong & 0x1f) << 20) |
    ((rs1.toLong & 0x1f) << 15) |
    ((funct3.toLong & 0x7) << 12) |
    (((imm >> 1) & 0xf) << 8) |
    (((imm >> 11) & 0x1) << 7) |
    (opcode.toLong & 0x7f)
  }

  def mkU(imm20: Int, rd: Int, opcode: Int): Long = {
    ((imm20.toLong & 0xfffff) << 12) |
    ((rd.toLong & 0x1f) << 7) |
    (opcode.toLong & 0x7f)
  }

  def mkJ(imm21: Int, rd: Int, opcode: Int): Long = {
    val imm = imm21.toLong & 0x1fffff
    (((imm >> 20) & 0x1) << 31) |
    (((imm >> 1) & 0x3ff) << 21) |
    (((imm >> 11) & 0x1) << 20) |
    (((imm >> 12) & 0xff) << 12) |
    ((rd.toLong & 0x1f) << 7) |
    (opcode.toLong & 0x7f)
  }

  def expandReg(r3: Int): Int = (r3 & 0x7) + 8

  def sext(v: Int, bits: Int): Int = {
    val mask = (1 << bits) - 1
    val x = v & mask
    if ((x & (1 << (bits - 1))) != 0) x - (1 << bits) else x
  }

  def bit(c: Int, idx: Int): Int = (c >> idx) & 1
  def bits(c: Int, hi: Int, lo: Int): Int = (c >> lo) & ((1 << (hi - lo + 1)) - 1)

  def decode(c: Int): (Long, Boolean) = {
    val op = bits(c, 1, 0)
    val funct3 = bits(c, 15, 13)
    val rd3 = bits(c, 4, 2)
    val rs1_3 = bits(c, 9, 7)
    val rs2_3 = bits(c, 4, 2)
    val rd5 = bits(c, 11, 7)
    val rs2_5 = bits(c, 6, 2)

    op match {
      case 0 => // Q0
        funct3 match {
          case 0 => // C.ADDI4SPN
            val nzuimm = (bits(c, 10, 7) << 6) | (bits(c, 12, 11) << 4) | (bit(c, 5) << 3) | (bit(c, 6) << 2)
            if (nzuimm == 0) (NOP, true)
            else (mkI(nzuimm, SP, F3_ADD_SUB, expandReg(rd3), OPC_OP_IMM), false)

          case 2 => // C.LW
            val uimm = (bit(c, 5) << 6) | (bits(c, 12, 10) << 3) | (bit(c, 6) << 2)
            (mkI(uimm, expandReg(rs1_3), F3_LW, expandReg(rd3), OPC_LOAD), false)

          case 6 => // C.SW
            val uimm = (bit(c, 5) << 6) | (bits(c, 12, 10) << 3) | (bit(c, 6) << 2)
            (mkS(uimm, expandReg(rs2_3), expandReg(rs1_3), F3_SW, OPC_STORE), false)

          case _ => (NOP, true)
        }

      case 1 => // Q1
        funct3 match {
          case 0 => // C.NOP / C.ADDI
            val imm6 = (bit(c, 12) << 5) | bits(c, 6, 2)
            val imm12 = sext(imm6, 6)
            if (rd5 == 0 || imm6 == 0) {
              (NOP, false) // NOP ou HINT -> NOP silencioso
            } else {
              (mkI(imm12, rd5, F3_ADD_SUB, rd5, OPC_OP_IMM), false)
            }

          case 1 => // C.JAL (RV32)
            val offset = (bit(c, 12) << 11) | (bit(c, 8) << 10) | (bits(c, 10, 9) << 8) |
                         (bit(c, 6) << 7) | (bit(c, 7) << 6) | (bit(c, 2) << 5) |
                         (bit(c, 11) << 4) | (bits(c, 5, 3) << 1)
            val imm21 = sext(offset, 12)
            (mkJ(imm21, RA, OPC_JAL), false)

          case 2 => // C.LI
            val imm6 = (bit(c, 12) << 5) | bits(c, 6, 2)
            val imm12 = sext(imm6, 6)
            if (rd5 == 0) (NOP, false) // HINT
            else (mkI(imm12, X0, F3_ADD_SUB, rd5, OPC_OP_IMM), false)

          case 3 => // C.ADDI16SP (rd=2) / C.LUI (rd!=0,2)
            val imm6 = (bit(c, 12) << 5) | bits(c, 6, 2)
            if (rd5 == SP) {
              val imm10 = (bit(c, 12) << 9) | (bits(c, 4, 3) << 7) | (bit(c, 5) << 6) |
                          (bit(c, 2) << 5) | (bit(c, 6) << 4)
              if (imm10 == 0) (NOP, true) // Reservado
              else {
                val imm12 = sext(imm10, 10)
                (mkI(imm12, SP, F3_ADD_SUB, SP, OPC_OP_IMM), false)
              }
            } else if (imm6 == 0) {
              (NOP, true) // C.LUI com imediato zero é reservado
            } else if (rd5 == 0) {
              (NOP, false) // C.LUI com rd=x0 é HINT
            } else {
              val imm20 = sext(imm6, 6)
              (mkU(imm20, rd5, OPC_LUI), false)
            }

          case 4 => // Aritmético / Lógico
            val subOp = bits(c, 11, 10)
            val bit12 = bit(c, 12)
            val shamt = bits(c, 6, 2)
            subOp match {
              case 0 => // C.SRLI
                if (bit12 == 1) (NOP, true) // shamt[5]=1 no RV32
                else if (shamt == 0) (NOP, false) // HINT
                else (mkI(shamt, expandReg(rs1_3), F3_SRL_SRA, expandReg(rs1_3), OPC_OP_IMM), false)

              case 1 => // C.SRAI
                if (bit12 == 1) (NOP, true) // shamt[5]=1 no RV32
                else if (shamt == 0) (NOP, false) // HINT
                else {
                  val imm12 = (0x20 << 5) | shamt
                  (mkI(imm12, expandReg(rs1_3), F3_SRL_SRA, expandReg(rs1_3), OPC_OP_IMM), false)
                }

              case 2 => // C.ANDI
                val imm6 = (bit12 << 5) | shamt
                val imm12 = sext(imm6, 6)
                (mkI(imm12, expandReg(rs1_3), F3_AND, expandReg(rs1_3), OPC_OP_IMM), false)

              case 3 => // CA
                if (bit12 == 1) (NOP, true) // Reservado no RV32
                else {
                  val caOp = bits(c, 6, 5)
                  val rd = expandReg(rs1_3)
                  val rs2 = expandReg(rs2_3)
                  caOp match {
                    case 0 => (mkR(F7_ALT, rs2, rd, F3_ADD_SUB, rd, OPC_OP), false) // C.SUB
                    case 1 => (mkR(F7_ZERO, rs2, rd, F3_XOR, rd, OPC_OP), false)     // C.XOR
                    case 2 => (mkR(F7_ZERO, rs2, rd, F3_OR, rd, OPC_OP), false)      // C.OR
                    case 3 => (mkR(F7_ZERO, rs2, rd, F3_AND, rd, OPC_OP), false)     // C.AND
                  }
                }
            }

          case 5 => // C.J
            val offset = (bit(c, 12) << 11) | (bit(c, 8) << 10) | (bits(c, 10, 9) << 8) |
                         (bit(c, 6) << 7) | (bit(c, 7) << 6) | (bit(c, 2) << 5) |
                         (bit(c, 11) << 4) | (bits(c, 5, 3) << 1)
            val imm21 = sext(offset, 12)
            (mkJ(imm21, X0, OPC_JAL), false)

          case 6 => // C.BEQZ
            val offset = (bit(c, 12) << 8) | (bits(c, 6, 5) << 6) | (bit(c, 2) << 5) |
                         (bits(c, 11, 10) << 3) | (bits(c, 4, 3) << 1)
            val imm13 = sext(offset, 9)
            (mkB(imm13, X0, expandReg(rs1_3), F3_BEQ, OPC_BRANCH), false)

          case 7 => // C.BNEZ
            val offset = (bit(c, 12) << 8) | (bits(c, 6, 5) << 6) | (bit(c, 2) << 5) |
                         (bits(c, 11, 10) << 3) | (bits(c, 4, 3) << 1)
            val imm13 = sext(offset, 9)
            (mkB(imm13, X0, expandReg(rs1_3), F3_BNE, OPC_BRANCH), false)
        }

      case 2 => // Q2
        funct3 match {
          case 0 => // C.SLLI
            val bit12 = bit(c, 12)
            val shamt = bits(c, 6, 2)
            if (bit12 == 1) (NOP, true) // shamt[5]=1 no RV32
            else if (rd5 == 0 || shamt == 0) (NOP, false) // HINT
            else (mkI(shamt, rd5, F3_SLL, rd5, OPC_OP_IMM), false)

          case 2 => // C.LWSP
            val uimm = (bits(c, 3, 2) << 6) | (bit(c, 12) << 5) | (bits(c, 6, 4) << 2)
            if (rd5 == 0) (NOP, true) // Reservado
            else (mkI(uimm, SP, F3_LW, rd5, OPC_LOAD), false)

          case 4 => // CR: C.JR, C.MV, C.EBREAK, C.JALR, C.ADD
            val bit12 = bit(c, 12)
            if (bit12 == 0) {
              if (rs2_5 == 0) {
                if (rd5 == 0) (NOP, true) // C.JR com rs1=0 é Reservado
                else (mkI(0, rd5, F3_ADD_SUB, X0, OPC_JALR), false) // C.JR
              } else {
                if (rd5 == 0) (NOP, false) // HINT
                else (mkR(F7_ZERO, rs2_5, X0, F3_ADD_SUB, rd5, OPC_OP), false) // C.MV
              }
            } else {
              if (rs2_5 == 0) {
                if (rd5 == 0) (EBREAK, false) // C.EBREAK
                else (mkI(0, rd5, F3_ADD_SUB, RA, OPC_JALR), false) // C.JALR
              } else {
                if (rd5 == 0) (NOP, false) // HINT
                else (mkR(F7_ZERO, rs2_5, rd5, F3_ADD_SUB, rd5, OPC_OP), false) // C.ADD
              }
            }

          case 6 => // C.SWSP
            val uimm = (bits(c, 8, 7) << 6) | (bits(c, 12, 9) << 2)
            (mkS(uimm, rs2_5, SP, F3_SW, OPC_STORE), false)

          case _ => (NOP, true)
        }

      case 3 => // Q3 (instruções de 32 bits, inválidas para o decompressor de 16 bits)
        (NOP, true)
    }
  }
}

class RvcDecompressorSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RvcDecompressor"

  // --------------------------------------------------------------------------
  // Suíte 1: Testes Direcionados
  // --------------------------------------------------------------------------
  it should "decodificar C.ADDI4SPN corretamente e marcar nzcuimm=0 como illegal" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      // C.ADDI4SPN com nzuimm = 0 -> illegal
      dut.io.inst_c.poke(0x0000.U)
      dut.io.illegal.expect(true.B)
      dut.io.inst_out.expect(0x00000013.U)

      // C.ADDI4SPN: rd'=0 (x8), nzuimm=4 -> addi x8, x2, 4
      val inst_addi4spn = (1 << 6)
      dut.io.inst_c.poke(inst_addi4spn.U(16.W))
      dut.io.illegal.expect(false.B)
      val expected = RvcGoldenModel.mkI(4, 2, 0, 8, 0x13)
      dut.io.inst_out.expect(expected.U(32.W))
    }
  }

  it should "decodificar C.LW e C.SW com registradores expandidos e offset correto" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      // C.LW rd'=1 (x9), rs1'=2 (x10), offset=4 (bit6=1)
      val inst_clw = (2 << 13) | (2 << 7) | (1 << 6) | (1 << 2)
      dut.io.inst_c.poke(inst_clw.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedLw = RvcGoldenModel.mkI(4, 10, 2, 9, 0x03)
      dut.io.inst_out.expect(expectedLw.U(32.W))

      // C.SW rs2'=3 (x11), rs1'=2 (x10), offset=4 (bit6=1)
      val inst_csw = (6 << 13) | (2 << 7) | (1 << 6) | (3 << 2)
      dut.io.inst_c.poke(inst_csw.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedSw = RvcGoldenModel.mkS(4, 11, 10, 2, 0x23)
      dut.io.inst_out.expect(expectedSw.U(32.W))
    }
  }

  it should "marcar instrucoes FP do Q0 como illegal" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      // C.FLD (funct3=001)
      dut.io.inst_c.poke((1 << 13).U(16.W))
      dut.io.illegal.expect(true.B)
      dut.io.inst_out.expect(0x00000013.U)

      // C.FLW (funct3=011)
      dut.io.inst_c.poke((3 << 13).U(16.W))
      dut.io.illegal.expect(true.B)

      // C.FSD (funct3=101)
      dut.io.inst_c.poke((5 << 13).U(16.W))
      dut.io.illegal.expect(true.B)

      // C.FSW (funct3=111)
      dut.io.inst_c.poke((7 << 13).U(16.W))
      dut.io.illegal.expect(true.B)
    }
  }

  it should "decodificar C.NOP e C.ADDI com extensao de sinal no Q1" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      // C.NOP: 0x0001 (op=01, todos outros bits zero)
      dut.io.inst_c.poke(0x0001.U(16.W))
      dut.io.illegal.expect(false.B)
      dut.io.inst_out.expect(0x00000013.U)

      // C.ADDI: rd=5 (x5), imm=-1 (0x3F nos 6 bits)
      val inst_addi_neg = 1 | (1 << 12) | (5 << 7) | (0x1f << 2)
      dut.io.inst_c.poke(inst_addi_neg.U(16.W))
      dut.io.illegal.expect(false.B)
      val expected = RvcGoldenModel.mkI(-1, 5, 0, 5, 0x13)
      dut.io.inst_out.expect(expected.U(32.W))
    }
  }

  it should "decodificar C.JAL e C.J no Q1" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      // C.JAL offset=2 (bit 3=1 -> imm[1]=1)
      val inst_jal = 1 | (1 << 13) | (1 << 3)
      dut.io.inst_c.poke(inst_jal.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedJal = RvcGoldenModel.mkJ(2, 1, 0x6f)
      dut.io.inst_out.expect(expectedJal.U(32.W))

      // C.J offset=2
      val inst_j = 1 | (5 << 13) | (1 << 3)
      dut.io.inst_c.poke(inst_j.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedJ = RvcGoldenModel.mkJ(2, 0, 0x6f)
      dut.io.inst_out.expect(expectedJ.U(32.W))
    }
  }

  it should "decodificar instrucoes CA (C.SUB, C.XOR, C.OR, C.AND) e rejeitar bit12=1" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      val inst_sub = 1 | (4 << 13) | (3 << 10) | (1 << 7) | (0 << 5) | (2 << 2)
      dut.io.inst_c.poke(inst_sub.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedSub = RvcGoldenModel.mkR(0x20, 10, 9, 0, 9, 0x33)
      dut.io.inst_out.expect(expectedSub.U(32.W))

      val inst_sub_illegal = inst_sub | (1 << 12)
      dut.io.inst_c.poke(inst_sub_illegal.U(16.W))
      dut.io.illegal.expect(true.B)
    }
  }

  it should "decodificar C.BEQZ e C.BNEZ no Q1 com bits do offset corretos" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      val inst_beqz = 1 | (6 << 13) | (1 << 7) | (2 << 3)
      dut.io.inst_c.poke(inst_beqz.U(16.W))
      dut.io.illegal.expect(false.B)
      val (expOut, expIll) = RvcGoldenModel.decode(inst_beqz)
      dut.io.inst_out.expect(expOut.U(32.W))
      dut.io.illegal.expect(expIll.B)

      // inst[11:10] = 10b -> imm[4:3] = 10b -> offset +16.
      val inst_beqz_offset = 0xC801
      dut.io.inst_c.poke(inst_beqz_offset.U(16.W))
      dut.io.illegal.expect(false.B)
      dut.io.inst_out.expect(RvcGoldenModel.mkB(16, 0, 8, 0, 0x63).U(32.W))

      val inst_bnez_offset = 0xE801
      dut.io.inst_c.poke(inst_bnez_offset.U(16.W))
      dut.io.illegal.expect(false.B)
      dut.io.inst_out.expect(RvcGoldenModel.mkB(16, 0, 8, 1, 0x63).U(32.W))
    }
  }

  it should "decodificar C.EBREAK e rejeitar C.JR com rs1=0 e instruções FP no Q2" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      val inst_jr_zero = 2 | (4 << 13)
      dut.io.inst_c.poke(inst_jr_zero.U(16.W))
      dut.io.illegal.expect(true.B)
      dut.io.inst_out.expect(0x00000013.U)

      val inst_ebreak = 2 | (4 << 13) | (1 << 12)
      dut.io.inst_c.poke(inst_ebreak.U(16.W))
      dut.io.illegal.expect(false.B)
      dut.io.inst_out.expect(RvcGoldenModel.EBREAK.U(32.W))

      val inst_lwsp_zero = 2 | (2 << 13)
      dut.io.inst_c.poke(inst_lwsp_zero.U(16.W))
      dut.io.illegal.expect(true.B)
    }
  }

  it should "tratar C.LUI com rd=x0 e imediato não-zero como HINT" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      val inst_lui_hint = 1 | (3 << 13) | (1 << 2)
      dut.io.inst_c.poke(inst_lui_hint.U(16.W))
      dut.io.illegal.expect(false.B)
      dut.io.inst_out.expect(0x00000013.U)
    }
  }

  it should "decodificar C.JR, C.JALR, C.MV, C.ADD no Q2" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      val inst_jr = 2 | (4 << 13) | (5 << 7)
      dut.io.inst_c.poke(inst_jr.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedJr = RvcGoldenModel.mkI(0, 5, 0, 0, 0x67)
      dut.io.inst_out.expect(expectedJr.U(32.W))

      val inst_jalr = 2 | (4 << 13) | (1 << 12) | (5 << 7)
      dut.io.inst_c.poke(inst_jalr.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedJalr = RvcGoldenModel.mkI(0, 5, 0, 1, 0x67)
      dut.io.inst_out.expect(expectedJalr.U(32.W))

      val inst_mv = 2 | (4 << 13) | (5 << 7) | (6 << 2)
      dut.io.inst_c.poke(inst_mv.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedMv = RvcGoldenModel.mkR(0, 6, 0, 0, 5, 0x33)
      dut.io.inst_out.expect(expectedMv.U(32.W))

      val inst_add = 2 | (4 << 13) | (1 << 12) | (5 << 7) | (6 << 2)
      dut.io.inst_c.poke(inst_add.U(16.W))
      dut.io.illegal.expect(false.B)
      val expectedAdd = RvcGoldenModel.mkR(0, 6, 5, 0, 5, 0x33)
      dut.io.inst_out.expect(expectedAdd.U(32.W))
    }
  }

  it should "marcar qualquer instrucao do Quadrante 3 (op=11) como illegal" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      dut.io.inst_c.poke(0x0003.U(16.W))
      dut.io.illegal.expect(true.B)
      dut.io.inst_out.expect(0x00000013.U)

      dut.io.inst_c.poke(0xffff.U(16.W))
      dut.io.illegal.expect(true.B)
      dut.io.inst_out.expect(0x00000013.U)
    }
  }

  // --------------------------------------------------------------------------
  // Suíte 2: Teste Exaustivo de Força Bruta (65.536 iterações)
  // --------------------------------------------------------------------------
  it should "passar no teste exaustivo de forca bruta (65536 iteracoes contra o Golden Model)" in {
    test(new RvcDecompressorTestWrapper) { dut =>
      var mismatches = 0
      for (raw <- 0 until 65536) {
        val (expectedOut, expectedIllegal) = RvcGoldenModel.decode(raw)
        dut.io.inst_c.poke(raw.U(16.W))

        val gotOut = dut.io.inst_out.peek().litValue.toLong
        val gotIllegal = dut.io.illegal.peek().litToBoolean

        if (gotOut != expectedOut || gotIllegal != expectedIllegal) {
          mismatches += 1
          if (mismatches <= 10) {
            println(
              f"MISMATCH at 0x$raw%04X: " +
              f"expected(out=0x$expectedOut%08X, illegal=$expectedIllegal), " +
              f"got(out=0x$gotOut%08X, illegal=$gotIllegal)"
            )
          }
        }
      }
      assert(mismatches == 0, s"Foram encontrados $mismatches erros no teste exaustivo de 65536 combinacoes!")
    }
  }
}
