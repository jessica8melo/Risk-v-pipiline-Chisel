package riscv.elementosbasicos

import chisel3._
import chisel3.util._

/** Módulo de Descompressão de Instruções RVC (Extensão C) para RV32I.
  *
  * Este módulo é ESTRITAMENTE COMBINACIONAL (sem clock e sem reset).
  * Converte uma instrução comprimida de 16 bits (inst_c) em uma instrução
  * RV32I base de 32 bits (inst_out).
  *
  * Caso a instrução seja inválida, não suportada pelo core ou reservada,
  * o sinal `illegal` será acionado (true) e `inst_out` receberá NOP (ADDI x0, x0, 0).
  *
  * HINTs válidos da especificação são mapeados para NOP silencioso (illegal = false).
  */
class RvcDecompressor extends RawModule {
  val io = IO(new Bundle {
    val inst_c   = Input(UInt(16.W))
    val inst_out = Output(UInt(32.W))
    val illegal  = Output(Bool())
  })

  // NOP base RV32I: addi x0, x0, 0
  val NOP = "h00000013".U(32.W)
  val EBREAK = "h00100073".U(32.W)

  // Registrador x2 (sp) e x1 (ra)
  val SP = 2.U(5.W)
  val RA = 1.U(5.W)
  val X0 = 0.U(5.W)

  // Opcodes RV32I
  val OPC_LOAD   = "b0000011".U(7.W)
  val OPC_OP_IMM = "b0010011".U(7.W)
  val OPC_STORE  = "b0100011".U(7.W)
  val OPC_OP     = "b0110011".U(7.W)
  val OPC_LUI    = "b0110111".U(7.W)
  val OPC_BRANCH = "b1100011".U(7.W)
  val OPC_JALR   = "b1100111".U(7.W)
  val OPC_JAL    = "b1101111".U(7.W)

  // Funct3 RV32I
  val F3_ADD_SUB = "b000".U(3.W)
  val F3_SLL     = "b001".U(3.W)
  val F3_SLT     = "b010".U(3.W)
  val F3_SLTU    = "b011".U(3.W)
  val F3_XOR     = "b100".U(3.W)
  val F3_SRL_SRA = "b101".U(3.W)
  val F3_OR      = "b110".U(3.W)
  val F3_AND     = "b111".U(3.W)

  val F3_BEQ     = "b000".U(3.W)
  val F3_BNE     = "b001".U(3.W)

  val F3_LW      = "b010".U(3.W)
  val F3_SW      = "b010".U(3.W)

  // Funct7 RV32I
  val F7_ZERO    = "b0000000".U(7.W)
  val F7_ALT     = "b0100000".U(7.W)

  // Construtores de instruções RV32I de 32 bits
  private def mkR(funct7: UInt, rs2: UInt, rs1: UInt, funct3: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(funct7, rs2, rs1, funct3, rd, opcode)

  private def mkI(imm12: UInt, rs1: UInt, funct3: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(imm12(11, 0), rs1, funct3, rd, opcode)

  private def mkS(imm12: UInt, rs2: UInt, rs1: UInt, funct3: UInt, opcode: UInt): UInt =
    Cat(imm12(11, 5), rs2, rs1, funct3, imm12(4, 0), opcode)

  private def mkB(imm13: UInt, rs2: UInt, rs1: UInt, funct3: UInt, opcode: UInt): UInt =
    Cat(imm13(12), imm13(10, 5), rs2, rs1, funct3, imm13(4, 1), imm13(11), opcode)

  private def mkU(imm20: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(imm20(19, 0), rd, opcode)

  private def mkJ(imm21: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(imm21(20), imm21(10, 1), imm21(11), imm21(19, 12), rd, opcode)

  // Expansão de registrador comprimido de 3 bits para 5 bits (x8 a x15)
  private def expandReg(r3: UInt): UInt = Cat("b01".U(2.W), r3)

  // Campos comuns da instrução comprimida
  val op     = io.inst_c(1, 0)
  val funct3 = io.inst_c(15, 13)
  val rd3    = io.inst_c(4, 2)
  val rs1_3  = io.inst_c(9, 7)
  val rs2_3  = io.inst_c(4, 2)
  val rd5    = io.inst_c(11, 7)
  val rs2_5  = io.inst_c(6, 2)

  // Sinais de resultado padrão
  val outWire     = WireDefault(NOP)
  val illegalWire = WireDefault(true.B)

  switch(op) {
    // ------------------------------------------------------------------------
    // Quadrante 0: op = 00
    // ------------------------------------------------------------------------
    is("b00".U) {
      switch(funct3) {
        // C.ADDI4SPN: addi rd', x2, nzuimm[9:2]
        is("b000".U) {
          val nzuimm = Cat(
            io.inst_c(10, 7),
            io.inst_c(12, 11),
            io.inst_c(5),
            io.inst_c(6),
            0.U(2.W)
          ) // 10 bits
          when(nzuimm === 0.U) {
            // nzuimm = 0 é reservado na especificação
            illegalWire := true.B
            outWire     := NOP
          }.otherwise {
            illegalWire := false.B
            outWire     := mkI(Cat(0.U(2.W), nzuimm), SP, F3_ADD_SUB, expandReg(rd3), OPC_OP_IMM)
          }
        }

        // C.LW: lw rd', offset(rs1')
        is("b010".U) {
          val uimm = Cat(
            io.inst_c(5),
            io.inst_c(12, 10),
            io.inst_c(6),
            0.U(2.W)
          ) // 7 bits
          illegalWire := false.B
          outWire     := mkI(Cat(0.U(5.W), uimm), expandReg(rs1_3), F3_LW, expandReg(rd3), OPC_LOAD)
        }

        // C.SW: sw rs2', offset(rs1')
        is("b110".U) {
          val uimm = Cat(
            io.inst_c(5),
            io.inst_c(12, 10),
            io.inst_c(6),
            0.U(2.W)
          ) // 7 bits
          illegalWire := false.B
          outWire     := mkS(Cat(0.U(5.W), uimm), expandReg(rs2_3), expandReg(rs1_3), F3_SW, OPC_STORE)
        }

        // Demais (C.FLD, C.FLW, C.FSD, C.FSW, reservado funct3=100) -> ILLEGAL
      }
    }

    // ------------------------------------------------------------------------
    // Quadrante 1: op = 01
    // ------------------------------------------------------------------------
    is("b01".U) {
      switch(funct3) {
        // C.NOP / C.ADDI: addi rd, rd, nzimm[5:0]
        is("b000".U) {
          val imm6 = Cat(io.inst_c(12), io.inst_c(6, 2))
          val imm12 = Cat(Fill(6, imm6(5)), imm6)
          when(rd5 === 0.U) {
            // C.NOP (imm6 == 0) ou C.NOP HINT (imm6 != 0): ambos NOP silencioso
            illegalWire := false.B
            outWire     := NOP
          }.elsewhen(imm6 === 0.U) {
            // C.ADDI HINT (rd != 0, nzimm == 0): NOP silencioso
            illegalWire := false.B
            outWire     := NOP
          }.otherwise {
            illegalWire := false.B
            outWire     := mkI(imm12, rd5, F3_ADD_SUB, rd5, OPC_OP_IMM)
          }
        }

        // C.JAL: jal x1, offset[11:1] (Apenas RV32)
        is("b001".U) {
          val imm12 = Cat(
            io.inst_c(12),
            io.inst_c(8),
            io.inst_c(10, 9),
            io.inst_c(6),
            io.inst_c(7),
            io.inst_c(2),
            io.inst_c(11),
            io.inst_c(5, 3),
            0.U(1.W)
          ) // 12 bits com sinal
          val imm21 = Cat(Fill(9, imm12(11)), imm12)
          illegalWire := false.B
          outWire     := mkJ(imm21, RA, OPC_JAL)
        }

        // C.LI: addi rd, x0, imm[5:0]
        is("b010".U) {
          val imm6 = Cat(io.inst_c(12), io.inst_c(6, 2))
          val imm12 = Cat(Fill(6, imm6(5)), imm6)
          when(rd5 === 0.U) {
            // HINT: NOP silencioso
            illegalWire := false.B
            outWire     := NOP
          }.otherwise {
            illegalWire := false.B
            outWire     := mkI(imm12, X0, F3_ADD_SUB, rd5, OPC_OP_IMM)
          }
        }

        // C.ADDI16SP (rd == 2) / C.LUI (rd != 0, 2)
        is("b011".U) {
          val imm6 = Cat(io.inst_c(12), io.inst_c(6, 2))
          when(rd5 === SP) {
            // C.ADDI16SP: addi x2, x2, nzimm[9:4]
            val imm10 = Cat(
              io.inst_c(12),
              io.inst_c(4, 3),
              io.inst_c(5),
              io.inst_c(2),
              io.inst_c(6),
              0.U(4.W)
            ) // 10 bits
            when(imm10 === 0.U) {
              // nzimm = 0 é reservado
              illegalWire := true.B
              outWire     := NOP
            }.otherwise {
              val imm12 = Cat(Fill(2, imm10(9)), imm10)
              illegalWire := false.B
              outWire     := mkI(imm12, SP, F3_ADD_SUB, SP, OPC_OP_IMM)
            }
          }.elsewhen(imm6 === 0.U) {
            // C.LUI com imediato zero é reservado.
            illegalWire := true.B
            outWire     := NOP
          }.elsewhen(rd5 === 0.U) {
            // C.LUI com rd=x0 e imediato não-zero é HINT.
            illegalWire := false.B
            outWire     := NOP
          }.otherwise {
            // C.LUI: lui rd, nzimm[17:12]
            val imm20 = Cat(Fill(14, imm6(5)), imm6)
            illegalWire := false.B
            outWire     := mkU(imm20, rd5, OPC_LUI)
          }
        }

        // C.SRLI, C.SRAI, C.ANDI, C.SUB, C.XOR, C.OR, C.AND
        is("b100".U) {
          val subOp = io.inst_c(11, 10)
          val bit12 = io.inst_c(12)
          val shamt = io.inst_c(6, 2)

          switch(subOp) {
            // C.SRLI: srli rs1', rs1', shamt (RV32: bit12 deve ser 0)
            is("b00".U) {
              when(bit12 === 1.U) {
                // shamt[5]=1 é reservado para RV32
                illegalWire := true.B
                outWire     := NOP
              }.elsewhen(shamt === 0.U) {
                // shamt == 0 é HINT -> NOP silencioso
                illegalWire := false.B
                outWire     := NOP
              }.otherwise {
                illegalWire := false.B
                outWire     := mkI(Cat(0.U(7.W), shamt), expandReg(rs1_3), F3_SRL_SRA, expandReg(rs1_3), OPC_OP_IMM)
              }
            }

            // C.SRAI: srai rs1', rs1', shamt (RV32: bit12 deve ser 0)
            is("b01".U) {
              when(bit12 === 1.U) {
                illegalWire := true.B
                outWire     := NOP
              }.elsewhen(shamt === 0.U) {
                illegalWire := false.B
                outWire     := NOP
              }.otherwise {
                val imm12 = Cat("b0100000".U(7.W), shamt)
                illegalWire := false.B
                outWire     := mkI(imm12, expandReg(rs1_3), F3_SRL_SRA, expandReg(rs1_3), OPC_OP_IMM)
              }
            }

            // C.ANDI: andi rs1', rs1', imm[5:0]
            is("b10".U) {
              val imm6 = Cat(bit12, io.inst_c(6, 2))
              val imm12 = Cat(Fill(6, imm6(5)), imm6)
              illegalWire := false.B
              outWire     := mkI(imm12, expandReg(rs1_3), F3_AND, expandReg(rs1_3), OPC_OP_IMM)
            }

            // CA-format: C.SUB, C.XOR, C.OR, C.AND
            is("b11".U) {
              when(bit12 === 1.U) {
                // Reservado no RV32 (usado para C.SUBW/ADDW no RV64)
                illegalWire := true.B
                outWire     := NOP
              }.otherwise {
                val caOp = io.inst_c(6, 5)
                val rd   = expandReg(rs1_3)
                val rs2  = expandReg(rs2_3)
                illegalWire := false.B
                switch(caOp) {
                  is("b00".U) { outWire := mkR(F7_ALT,  rs2, rd, F3_ADD_SUB, rd, OPC_OP) } // C.SUB
                  is("b01".U) { outWire := mkR(F7_ZERO, rs2, rd, F3_XOR,     rd, OPC_OP) } // C.XOR
                  is("b10".U) { outWire := mkR(F7_ZERO, rs2, rd, F3_OR,      rd, OPC_OP) } // C.OR
                  is("b11".U) { outWire := mkR(F7_ZERO, rs2, rd, F3_AND,     rd, OPC_OP) } // C.AND
                }
              }
            }
          }
        }

        // C.J: jal x0, offset[11:1]
        is("b101".U) {
          val imm12 = Cat(
            io.inst_c(12),
            io.inst_c(8),
            io.inst_c(10, 9),
            io.inst_c(6),
            io.inst_c(7),
            io.inst_c(2),
            io.inst_c(11),
            io.inst_c(5, 3),
            0.U(1.W)
          )
          val imm21 = Cat(Fill(9, imm12(11)), imm12)
          illegalWire := false.B
          outWire     := mkJ(imm21, X0, OPC_JAL)
        }

        // C.BEQZ: beq rs1', x0, offset[8:1]
        is("b110".U) {
          val imm9 = Cat(
            io.inst_c(12),
            io.inst_c(6, 5),
            io.inst_c(2),
            io.inst_c(11, 10),
            io.inst_c(4, 3),
            0.U(1.W)
          ) // 9 bits
          val imm13 = Cat(Fill(4, imm9(8)), imm9)
          illegalWire := false.B
          outWire     := mkB(imm13, X0, expandReg(rs1_3), F3_BEQ, OPC_BRANCH)
        }

        // C.BNEZ: bne rs1', x0, offset[8:1]
        is("b111".U) {
          val imm9 = Cat(
            io.inst_c(12),
            io.inst_c(6, 5),
            io.inst_c(2),
            io.inst_c(11, 10),
            io.inst_c(4, 3),
            0.U(1.W)
          )
          val imm13 = Cat(Fill(4, imm9(8)), imm9)
          illegalWire := false.B
          outWire     := mkB(imm13, X0, expandReg(rs1_3), F3_BNE, OPC_BRANCH)
        }
      }
    }

    // ------------------------------------------------------------------------
    // Quadrante 2: op = 10
    // ------------------------------------------------------------------------
    is("b10".U) {
      switch(funct3) {
        // C.SLLI: slli rd, rd, shamt[5:0] (RV32: bit12 deve ser 0)
        is("b000".U) {
          val bit12 = io.inst_c(12)
          val shamt = io.inst_c(6, 2)
          when(bit12 === 1.U) {
            // shamt[5]=1 é reservado para RV32
            illegalWire := true.B
            outWire     := NOP
          }.elsewhen(rd5 === 0.U) {
            // HINT: NOP silencioso
            illegalWire := false.B
            outWire     := NOP
          }.elsewhen(shamt === 0.U) {
            // HINT: NOP silencioso
            illegalWire := false.B
            outWire     := NOP
          }.otherwise {
            illegalWire := false.B
            outWire     := mkI(Cat(0.U(7.W), shamt), rd5, F3_SLL, rd5, OPC_OP_IMM)
          }
        }

        // C.LWSP: lw rd, offset(x2)
        is("b010".U) {
          val uimm = Cat(
            io.inst_c(3, 2),
            io.inst_c(12),
            io.inst_c(6, 4),
            0.U(2.W)
          ) // 8 bits
          when(rd5 === 0.U) {
            // rd = 0 é reservado
            illegalWire := true.B
            outWire     := NOP
          }.otherwise {
            illegalWire := false.B
            outWire     := mkI(Cat(0.U(4.W), uimm), SP, F3_LW, rd5, OPC_LOAD)
          }
        }

        // CR-format: C.JR, C.MV, C.EBREAK, C.JALR, C.ADD
        is("b100".U) {
          val bit12 = io.inst_c(12)
          when(bit12 === 0.U) {
            when(rs2_5 === 0.U) {
              // C.JR: jalr x0, 0(rs1)
              when(rd5 === 0.U) {
                // rs1 = 0 é RESERVADO
                illegalWire := true.B
                outWire     := NOP
              }.otherwise {
                illegalWire := false.B
                outWire     := mkI(0.U(12.W), rd5, F3_ADD_SUB, X0, OPC_JALR)
              }
            }.otherwise {
              // C.MV: add rd, x0, rs2
              when(rd5 === 0.U) {
                // HINT: NOP silencioso
                illegalWire := false.B
                outWire     := NOP
              }.otherwise {
                illegalWire := false.B
                outWire     := mkR(F7_ZERO, rs2_5, X0, F3_ADD_SUB, rd5, OPC_OP)
              }
            }
          }.otherwise {
            when(rs2_5 === 0.U) {
              when(rd5 === 0.U) {
                // C.EBREAK: ebreak
                illegalWire := false.B
                outWire     := EBREAK
              }.otherwise {
                // C.JALR: jalr x1, 0(rs1)
                illegalWire := false.B
                outWire     := mkI(0.U(12.W), rd5, F3_ADD_SUB, RA, OPC_JALR)
              }
            }.otherwise {
              // C.ADD: add rd, rd, rs2
              when(rd5 === 0.U) {
                // HINT: NOP silencioso
                illegalWire := false.B
                outWire     := NOP
              }.otherwise {
                illegalWire := false.B
                outWire     := mkR(F7_ZERO, rs2_5, rd5, F3_ADD_SUB, rd5, OPC_OP)
              }
            }
          }
        }

        // C.SWSP: sw rs2, offset(x2)
        is("b110".U) {
          val uimm = Cat(
            io.inst_c(8, 7),
            io.inst_c(12, 9),
            0.U(2.W)
          ) // 8 bits
          illegalWire := false.B
          outWire     := mkS(Cat(0.U(4.W), uimm), rs2_5, SP, F3_SW, OPC_STORE)
        }

        // Demais (C.FLDSP, C.FLWSP, C.FSDSP, C.FSWSP) -> ILLEGAL
      }
    }

    // ------------------------------------------------------------------------
    // Quadrante 3: op = 11 (instruções de 32 bits, inválidas para decompressor)
    // ------------------------------------------------------------------------
    is("b11".U) {
      illegalWire := true.B
      outWire     := NOP
    }
  }

  io.inst_out := outWire
  io.illegal  := illegalWire
}
