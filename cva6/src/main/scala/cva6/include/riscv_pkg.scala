package cva6.include

import chisel3._
import chisel3.util.{Cat, log2Ceil}

object riscv_pkg {
  // --------------------
  // Privilege Spec
  // --------------------
  // privilege levels
  object priv_lvl_t extends ChiselEnum {
    val PRIV_LVL_M = Value(3.U(2.W))
    val PRIV_LVL_S = Value(3.U(1.W))
    val PRIV_LVL_U = Value(3.U(0.W))
  }

  // type which holds xlen
  object xlen_t extends ChiselEnum {
      val XLEN_32  = Value(1.U(2.W))
      val XLEN_64  = Value(2.U(2.W))
      val XLEN_128 = Value(3.U(2.W))
  }

  object xs_t extends ChiselEnum {
    val Off     = Value(0.U(2.W))
    val Initial = Value(1.U(2.W))
    val Clean   = Value(2.U(2.W))
    val Dirty   = Value(3.U(2.W))
  }

  class status_rv64_t extends Bundle {
    val sd      = Bool()        // signal dirty state - read-only
    val wpri4   = UInt(27.W)    // writes preserved reads ignored
    val sxl     = Bool()        // variable supervisor mode xlen - hardwired to zero
    val uxl     = Bool()        // variable user mode xlen - hardwired to zero
    val wpri3   = UInt(9.W)     // writes preserved reads ignored
    val tsr     = Bool()        // trap sret
    val tw      = Bool()        // time wait
    val tvm     = Bool()        // trap virtual memory
    val mxr     = Bool()        // make executable readable
    val sum     = Bool()        // permit supervisor user memory access
    val mprv    = Bool()        // modify privilege - privilege level for ld/st
    val xs      = xs_t()        // extension register - hardwired to zero
    val fs      = xs_t()        // floating point extension register
    val mpp     = priv_lvl_t()  // holds the previous privilege mode up to machine
    val wpri2   = UInt(2.W)     // writes preserved reads ignored
    val spp     = Bool()        // holds the previous privilege mode up to supervisor
    val mpie    = Bool()        // machine interrupts enable bit active prior to trap
    val wpri1   = Bool()        // writes preserved reads ignored
    val spie    = Bool()        // supervisor interrupts enable bit active prior to trap
    val upie    = Bool()        // user interrupts enable bit active prior to trap - hardwired to zero
    val mie     = Bool()        // machine interrupts enable
    val wpri0   = Bool()        // writes preserved reads ignored
    val sie     = Bool()        // supervisor interrupts enable
    val uie     = Bool()        // user interrupts enable - hardwired to zero
  }

  class status_rv32_t extends Bundle {
    val sd      = Bool()        // signal dirty - read-only - hardwired zero
    val wpri3   = UInt(8.W)     // writes preserved reads ignored
    val tsr     = Bool()        // trap sret
    val tw      = Bool()        // time wait
    val tvm     = Bool()        // trap virtual memory
    val mxr     = Bool()        // make executable readable
    val sum     = Bool()        // permit supervisor user memory access
    val mprv    = Bool()        // modify privilege - privilege level for ld/st
    val xs      = UInt(2.W)     // extension register - hardwired to zero
    val fs      = UInt(2.W)     // extension register - hardwired to zero
    val mpp     = priv_lvl_t()  // holds the previous privilege mode up to machine
    val wpri2   = UInt(2.W)     // writes preserved reads ignored
    val spp     = Bool()        // holds the previous privilege mode up to supervisor
    val mpie    = Bool()        // machine interrupts enable bit active prior to trap
    val wpri1   = Bool()        // writes preserved reads ignored
    val spie    = Bool()        // supervisor interrupts enable bit active prior to trap
    val upie    = Bool()        // user interrupts enable bit active prior to trap - hardwired to zero
    val mie     = Bool()        // machine interrupts enable
    val wpri0   = Bool()        // writes preserved reads ignored
    val sie     = Bool()        // supervisor interrupts enable
    val uie     = Bool()        // user interrupts enable - hardwired to zero
  }

  class satp_t extends Bundle {
    val mode    = UInt(4.W)
    val asid    = UInt(16.W)
    val ppn     = UInt(43.W)
  }

  // --------------------
  // Instruction Types
  // --------------------
  import chisel3._

  class rtype_t extends Bundle {
    val funct7 = UInt(7.W)
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class r4type_t extends Bundle {
    val rs3 = UInt(5.W)
    val funct2 = UInt(2.W)
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class rftype_t extends Bundle {
    val funct5 = UInt(5.W)
    val fmt = UInt(2.W)
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val rm = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class rvftype_t extends Bundle {
    val funct2 = UInt(2.W)
    val vecfltop = UInt(5.W)
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val repl = Bool()
    val vfmt = UInt(2.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class itype_t extends Bundle {
    val imm = UInt(12.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class stype_t extends Bundle {
    val imm = UInt(7.W)
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val imm0 = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class utype_t extends Bundle {
    val imm = UInt(20.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class atype_t extends Bundle {
    val funct5 = UInt(5.W)
    val aq = Bool()
    val rl = Bool()
    val rs2 = UInt(5.W)
    val rs1 = UInt(5.W)
    val funct3 = UInt(3.W)
    val rd = UInt(5.W)
    val opcode = UInt(7.W)
  }

  class instruction_t extends Bundle {
    val instr = UInt(32.W)
    val rtype = new rtype_t
    val r4type = new r4type_t
    val rftype = new rftype_t
    val rvftype = new rvftype_t
    val itype = new itype_t
    val stype = new stype_t
    val utype = new utype_t
    val atype = new atype_t
  }


  // --------------------
  // Opcodes
  // --------------------
  // RV32/64G listings:
  // Quadrant 0
  val OpcodeLoad = "b00_000_11".U
  val OpcodeLoadFp = "b00_001_11".U
  val OpcodeCustom0 = "b00_010_11".U
  val OpcodeMiscMem = "b00_011_11".U
  val OpcodeOpImm = "b00_100_11".U
  val OpcodeAuipc = "b00_101_11".U
  val OpcodeOpImm32 = "b00_110_11".U
  // Quadrant 1
  val OpcodeStore = "b01_000_11".U
  val OpcodeStoreFp = "b01_001_11".U
  val OpcodeCustom1 = "b01_010_11".U
  val OpcodeAmo = "b01_011_11".U
  val OpcodeOp = "b01_100_11".U
  val OpcodeLui = "b01_101_11".U
  val OpcodeOp32 = "b01_110_11".U
  // Quadrant 2
  val OpcodeMadd = "b10_000_11".U
  val OpcodeMsub = "b10_001_11".U
  val OpcodeNmsub = "b10_010_11".U
  val OpcodeNmadd = "b10_011_11".U
  val OpcodeOpFp = "b10_100_11".U
  val OpcodeRsrvd1 = "b10_101_11".U
  val OpcodeCustom2 = "b10_110_11".U
  // Quadrant 3
  val OpcodeBranch = "b11_000_11".U
  val OpcodeJalr = "b11_001_11".U
  val OpcodeRsrvd2 = "b11_010_11".U
  val OpcodeJal = "b11_011_11".U
  val OpcodeSystem = "b11_100_11".U
  val OpcodeRsrvd3 = "b11_101_11".U
  val OpcodeCustom3 = "b11_110_11".U

  // RV64C listings:
  // Quadrant 0
  val OpcodeC0 = "b00".U
  val OpcodeC0Addi4spn = "b000".U
  val OpcodeC0Fld = "b001".U
  val OpcodeC0Lw = "b010".U
  val OpcodeC0Ld = "b011".U
  val OpcodeC0Rsrvd = "b100".U
  val OpcodeC0Fsd = "b101".U
  val OpcodeC0Sw = "b110".U
  val OpcodeC0Sd = "b111".U
  // Quadrant 1
  val OpcodeC1 = "b01".U
  val OpcodeC1Addi = "b000".U
  val OpcodeC1Addiw = "b001".U
  val OpcodeC1Li = "b010".U
  val OpcodeC1LuiAddi16sp = "b011".U
  val OpcodeC1MiscAlu = "b100".U
  val OpcodeC1J = "b101".U
  val OpcodeC1Beqz = "b110".U
  val OpcodeC1Bnez = "b111".U
  // Quadrant 2
  val OpcodeC2 = "b10".U
  val OpcodeC2Slli = "b000".U
  val OpcodeC2Fldsp = "b001".U
  val OpcodeC2Lwsp = "b010".U
  val OpcodeC2Ldsp = "b011".U
  val OpcodeC2JalrMvAdd = "b100".U
  val OpcodeC2Fsdsp = "b101".U
  val OpcodeC2Swsp = "b110".U
  val OpcodeC2Sdsp = "b111".U

  // ----------------------
  // Virtual Memory
  // ----------------------
  // memory management, pte
  class pte_t extends Bundle {
    val reserved = UInt(10.W)
    val ppn      = UInt(44.W)
    val rsw      = UInt(2.W)
    val d        = Bool()
    val a        = Bool()
    val g        = Bool()
    val u        = Bool()
    val x        = Bool()
    val w        = Bool()
    val r        = Bool()
    val v        = Bool()
  }

  // ----------------------
  // Exception Cause Codes
  // ----------------------
  val INSTR_ADDR_MISALIGNED = 0.U(64.W)
  val INSTR_ACCESS_FAULT    = 1.U(64.W)
  val ILLEGAL_INSTR         = 2.U(64.W)
  val BREAKPOINT            = 3.U(64.W)
  val LD_ADDR_MISALIGNED    = 4.U(64.W)
  val LD_ACCESS_FAULT       = 5.U(64.W)
  val ST_ADDR_MISALIGNED    = 6.U(64.W)
  val ST_ACCESS_FAULT       = 7.U(64.W)
  val ENV_CALL_UMODE        = 8.U(64.W)  // environment call from user mode
  val ENV_CALL_SMODE        = 9.U(64.W)  // environment call from supervisor mode
  val ENV_CALL_MMODE        = 11.U(64.W) // environment call from machine mode
  val INSTR_PAGE_FAULT      = 12.U(64.W) // Instruction page fault
  val LOAD_PAGE_FAULT       = 13.U(64.W) // Load page fault
  val STORE_PAGE_FAULT      = 15.U(64.W) // Store page fault
  val DEBUG_REQUEST         = 24.U(64.W) // Debug request

  val IRQ_S_SOFT  = 1
  val IRQ_M_SOFT  = 3
  val IRQ_S_TIMER = 5
  val IRQ_M_TIMER = 7
  val IRQ_S_EXT   = 9
  val IRQ_M_EXT   = 11

  val MIP_SSIP = (1L << IRQ_S_SOFT).U(64.W)
  val MIP_MSIP = (1L << IRQ_M_SOFT).U(64.W)
  val MIP_STIP = (1L << IRQ_S_TIMER).U(64.W)
  val MIP_MTIP = (1L << IRQ_M_TIMER).U(64.W)
  val MIP_SEIP = (1L << IRQ_S_EXT).U(64.W)
  val MIP_MEIP = (1L << IRQ_M_EXT).U(64.W)

  val S_SW_INTERRUPT    = ((1L << 63) | IRQ_S_SOFT).U(64.W)
  val M_SW_INTERRUPT    = ((1L << 63) | IRQ_M_SOFT).U(64.W)
  val S_TIMER_INTERRUPT = ((1L << 63) | IRQ_S_TIMER).U(64.W)
  val M_TIMER_INTERRUPT = ((1L << 63) | IRQ_M_TIMER).U(64.W)
  val S_EXT_INTERRUPT   = ((1L << 63) | IRQ_S_EXT).U(64.W)
  val M_EXT_INTERRUPT   = ((1L << 63) | IRQ_M_EXT).U(64.W)

  // -----
  // CSRs
  // -----
  object csr_reg_t extends ChiselEnum {
    // Floating-Point CSRs
    val CSR_FFLAGS         = Value(0x001.U(12.W))
    val CSR_FRM            = Value(0x002.U(12.W))
    val CSR_FCSR           = Value(0x003.U(12.W))
    val CSR_FTRAN          = Value(0x800.U(12.W))
    // Supervisor Mode val CSRs
    val CSR_SSTATUS        = Value(0x100.U(12.W))
    val CSR_SIE            = Value(0x104.U(12.W))
    val CSR_STVEC          = Value(0x105.U(12.W))
    val CSR_SCOUNTEREN     = Value(0x106.U(12.W))
    val CSR_SSCRATCH       = Value(0x140.U(12.W))
    val CSR_SEPC           = Value(0x141.U(12.W))
    val CSR_SCAUSE         = Value(0x142.U(12.W))
    val CSR_STVAL          = Value(0x143.U(12.W))
    val CSR_SIP            = Value(0x144.U(12.W))
    val CSR_SAPT           = Value(0x180.U(12.W))
    // Machine Mode val CSRs
    val CSR_MSTATUS        = Value(0x300.U(12.W))
    val CSR_MISA           = Value(0x301.U(12.W))
    val CSR_MEDELEG        = Value(0x302.U(12.W))
    val CSR_MIDELEG        = Value(0x303.U(12.W))
    val CSR_MIE            = Value(0x304.U(12.W))
    val CSR_MTVEC          = Value(0x305.U(12.W))
    val CSR_MCOUNTEREN     = Value(0x306.U(12.W))
    val CSR_MSCRATCH       = Value(0x340.U(12.W))
    val CSR_MEPC           = Value(0x341.U(12.W))
    val CSR_MCAUSE         = Value(0x342.U(12.W))
    val CSR_MTVAL          = Value(0x343.U(12.W))
    val CSR_MIP            = Value(0x344.U(12.W))
    val CSR_PMPCFG0        = Value(0x3A0.U(12.W))
    val CSR_PMPADDR0       = Value(0x3B0.U(12.W))
    val CSR_MVENDORID      = Value(0xF11.U(12.W))
    val CSR_MARCHID        = Value(0xF12.U(12.W))
    val CSR_MIMPID         = Value(0xF13.U(12.W))
    val CSR_MHARTID        = Value(0xF14.U(12.W))
    val CSR_MCYCLE         = Value(0xB00.U(12.W))
    val CSR_MINSTRET       = Value(0xB02.U(12.W))
    // Performance counters (Machine Mode)
    val CSR_ML1_ICACHE_MISS = Value(0xB03.U(12.W))  // L1 Instr Cache Miss
    val CSR_ML1_DCACHE_MISS = Value(0xB04.U(12.W))  // L1 Data Cache Miss
    val CSR_MITLB_MISS      = Value(0xB05.U(12.W))  // ITLB Miss
    val CSR_MDTLB_MISS      = Value(0xB06.U(12.W))  // DTLB Miss
    val CSR_MLOAD           = Value(0xB07.U(12.W))  // Loads
    val CSR_MSTORE          = Value(0xB08.U(12.W))  // Stores
    val CSR_MEXCEPTION      = Value(0xB09.U(12.W))  // Taken exceptions
    val CSR_MEXCEPTION_RET  = Value(0xB0A.U(12.W))  // Exception return
    val CSR_MBRANCH_JUMP    = Value(0xB0B.U(12.W))  // Software change of PC
    val CSR_MCALL           = Value(0xB0C.U(12.W))  // Procedure call
    val CSR_MRET            = Value(0xB0D.U(12.W))  // Procedure Return
    val CSR_MMIS_PREDICT    = Value(0xB0E.U(12.W))  // Branch mis-predicted
    val CSR_MSB_FULL        = Value(0xB0F.U(12.W))  // Scoreboard full
    val CSR_MIF_EMPTY       = Value(0xB10.U(12.W))  // instruction fetch queue empty
    val CSR_MHPM_COUNTER_17 = Value(0xB11.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_18 = Value(0xB12.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_19 = Value(0xB13.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_20 = Value(0xB14.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_21 = Value(0xB15.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_22 = Value(0xB16.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_23 = Value(0xB17.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_24 = Value(0xB18.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_25 = Value(0xB19.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_26 = Value(0xB1A.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_27 = Value(0xB1B.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_28 = Value(0xB1C.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_29 = Value(0xB1D.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_30 = Value(0xB1E.U(12.W))  // reserved
    val CSR_MHPM_COUNTER_31 = Value(0xB1F.U(12.W))  // reserved
    // Cache Control (platform specifc)
    val CSR_DCACHE         = Value(0x701.U(12.W))
    val CSR_ICACHE         = Value(0x700.U(12.W))
    // Triggers
    val CSR_TSELECT        = Value(0x7A0.U(12.W))
    val CSR_TDATA1         = Value(0x7A1.U(12.W))
    val CSR_TDATA2         = Value(0x7A2.U(12.W))
    val CSR_TDATA3         = Value(0x7A3.U(12.W))
    val CSR_TINFO          = Value(0x7A4.U(12.W))
    // Debug val CSR
    val CSR_DCSR           = Value(0x7b0.U(12.W))
    val CSR_DPC            = Value(0x7b1.U(12.W))
    val CSR_DSCRATCH0      = Value(0x7b2.U(12.W)) // optional
    val CSR_DSCRATCH1      = Value(0x7b3.U(12.W)) // optional
    // Counters and Timers (User Mode - R/O Shadows)
    val CSR_CYCLE          = Value(0xC00.U(12.W))
    val CSR_TIME           = Value(0xC01.U(12.W))
    val CSR_INSTRET        = Value(0xC02.U(12.W))
    // Performance counters (User Mode - R/O Shadows)
    val CSR_L1_ICACHE_MISS = Value(0xC03.U(12.W))  // L1 Instr Cache Miss
    val CSR_L1_DCACHE_MISS = Value(0xC04.U(12.W))  // L1 Data Cache Miss
    val CSR_ITLB_MISS      = Value(0xC05.U(12.W))  // ITLB Miss
    val CSR_DTLB_MISS      = Value(0xC06.U(12.W))  // DTLB Miss
    val CSR_LOAD           = Value(0xC07.U(12.W))  // Loads
    val CSR_STORE          = Value(0xC08.U(12.W))  // Stores
    val CSR_EXCEPTION      = Value(0xC09.U(12.W))  // Taken exceptions
    val CSR_EXCEPTION_RET  = Value(0xC0A.U(12.W))  // Exception return
    val CSR_BRANCH_JUMP    = Value(0xC0B.U(12.W))  // Software change of PC
    val CSR_CALL           = Value(0xC0C.U(12.W))  // Procedure call
    val CSR_RET            = Value(0xC0D.U(12.W))  // Procedure Return
    val CSR_MIS_PREDICT    = Value(0xC0E.U(12.W))  // Branch mis-predicted
    val CSR_SB_FULL        = Value(0xC0F.U(12.W))  // Scoreboard full
    val CSR_IF_EMPTY       = Value(0xC10.U(12.W))  // instruction fetch queue empty
    val CSR_HPM_COUNTER_17 = Value(0xC11.U(12.W))  // reserved
    val CSR_HPM_COUNTER_18 = Value(0xC12.U(12.W))  // reserved
    val CSR_HPM_COUNTER_19 = Value(0xC13.U(12.W))  // reserved
    val CSR_HPM_COUNTER_20 = Value(0xC14.U(12.W))  // reserved
    val CSR_HPM_COUNTER_21 = Value(0xC15.U(12.W))  // reserved
    val CSR_HPM_COUNTER_22 = Value(0xC16.U(12.W))  // reserved
    val CSR_HPM_COUNTER_23 = Value(0xC17.U(12.W))  // reserved
    val CSR_HPM_COUNTER_24 = Value(0xC18.U(12.W))  // reserved
    val CSR_HPM_COUNTER_25 = Value(0xC19.U(12.W))  // reserved
    val CSR_HPM_COUNTER_26 = Value(0xC1A.U(12.W))  // reserved
    val CSR_HPM_COUNTER_27 = Value(0xC1B.U(12.W))  // reserved
    val CSR_HPM_COUNTER_28 = Value(0xC1C.U(12.W))  // reserved
    val CSR_HPM_COUNTER_29 = Value(0xC1D.U(12.W))  // reserved
    val CSR_HPM_COUNTER_30 = Value(0xC1E.U(12.W))  // reserved
    val CSR_HPM_COUNTER_31 = Value(0xC1F.U(12.W))  // reserved
  }

  val SSTATUS_UIE  : BigInt = 0x00000001L
  val SSTATUS_SIE  : BigInt = 0x00000002L
  val SSTATUS_SPIE : BigInt = 0x00000020L
  val SSTATUS_SPP  : BigInt = 0x00000100L
  val SSTATUS_FS   : BigInt = 0x00006000L
  val SSTATUS_XS   : BigInt = 0x00018000L
  val SSTATUS_SUM  : BigInt = 0x00040000L
  val SSTATUS_MXR  : BigInt = 0x00080000L
  val SSTATUS_UPIE : BigInt = 0x00000010L
  val SSTATUS_UXL  : BigInt = 0x0000000300000000L
  val SSTATUS64_SD : BigInt = 0x8000000000000000L
  val SSTATUS32_SD : BigInt = 0x80000000L

  val MSTATUS_UIE  : BigInt = 0x00000001L
  val MSTATUS_SIE  : BigInt = 0x00000002L
  val MSTATUS_HIE  : BigInt = 0x00000004L
  val MSTATUS_MIE  : BigInt = 0x00000008L
  val MSTATUS_UPIE : BigInt = 0x00000010L
  val MSTATUS_SPIE : BigInt = 0x00000020L
  val MSTATUS_HPIE : BigInt = 0x00000040L
  val MSTATUS_MPIE : BigInt = 0x00000080L
  val MSTATUS_SPP  : BigInt = 0x00000100L
  val MSTATUS_HPP  : BigInt = 0x00000600L
  val MSTATUS_MPP  : BigInt = 0x00001800L
  val MSTATUS_FS   : BigInt = 0x00006000L
  val MSTATUS_XS   : BigInt = 0x00018000L
  val MSTATUS_MPRV : BigInt = 0x00020000L
  val MSTATUS_SUM  : BigInt = 0x00040000L
  val MSTATUS_MXR  : BigInt = 0x00080000L
  val MSTATUS_TVM  : BigInt = 0x00100000L
  val MSTATUS_TW   : BigInt = 0x00200000L
  val MSTATUS_TSR  : BigInt = 0x00400000L
  val MSTATUS32_SD : BigInt = 0x80000000L
  val MSTATUS_UXL  : BigInt = 0x0000000300000000L
  val MSTATUS_SXL  : BigInt = 0x0000000C00000000L
  val MSTATUS64_SD : BigInt = 0x8000000000000000L

  object csr_op_t extends ChiselEnum {
    val CSRRW  = Value(0x1.U(3.W))
    val CSRRS  = Value(0x2.U(3.W))
    val CSRRC  = Value(0x3.U(3.W))
    val CSRRWI = Value(0x5.U(3.W))
    val CSRRSI = Value(0x6.U(3.W))
    val CSRRCI = Value(0x7.U(3.W))
  }

  // decoded CSR address
  class csr_addr_t extends Bundle {
    val rw       = UInt(2.W)
    val priv_lvl = priv_lvl_t()
    val address  = UInt(8.W)
  }

  //typedef union packed {
  //    csr_reg_t   address
  //    csr_addr_t  csr_decode
  //} csr_t
  val csr_t = UInt(12.W)

  // Floating-Point control and status register (32-bit!)
  class fcsr_t extends Bundle {
    val reserved  = UInt(16.W) // reserved for L extension, return 0 otherwise
    val fprec     = UInt(7.W)  // div/sqrt precision control
    val frm       = UInt(3.W)  // float rounding mode
    val fflags    = UInt(5.W)  // float exception flags
  }

  // -----
  // Debug
  // -----
  class dcsr_t extends Bundle {
    val xdebugver = UInt(4.W)
    val zero2     = UInt(8.W)
    val ebreakm   = Bool()
    val zero1     = Bool()
    val ebreaks   = Bool()
    val ebreaku   = Bool()
    val stepie    = Bool()
    val stopcount = Bool()
    val stoptime  = Bool()
    val cause     = UInt(3.W)
    val zero0     = Bool()
    val mprven    = Bool()
    val nmip      = Bool()
    val step      = Bool()
    val prv       = priv_lvl_t()
  }

  // Instruction Generation Helpers
  def jal(rd: UInt, imm: UInt): UInt = {
    require(rd.getWidth == 5 && imm.getWidth == 21)
    Cat(imm(20), imm(10, 1), imm(11), imm(19, 12), rd, "b1101111".U)
  }

  def jalr(rd: UInt, rs1: UInt, offset: UInt): UInt = {
    require(rd.getWidth == 5 && rs1.getWidth == 5 && offset.getWidth == 12)
    Cat(offset, rs1, "b000".U, rd, "b1100111".U)
  }

  def andi(rd: UInt, rs1: UInt, imm: UInt): UInt = {
    require(rd.getWidth == 5 && rs1.getWidth == 5 && imm.getWidth == 12)
    Cat(imm, rs1, "b111".U, rd, "b0010011".U)
  }

  def slli(rd: UInt, rs1: UInt, shamt: UInt): UInt = {
    require(rd.getWidth == 5 && rs1.getWidth == 5 && shamt.getWidth == 6)
    Cat("b000000".U, shamt, rs1, "b001".U, rd, "b0010011".U)
  }

  def srli(rd: UInt, rs1: UInt, shamt: UInt): UInt = {
    require(rd.getWidth == 5 && rs1.getWidth == 5 && shamt.getWidth == 6)
    Cat("b000000".U, shamt, rs1, "b101".U, rd, "b0010011".U)
  }

  def load(size: UInt, dest: UInt, base: UInt, offset: UInt): UInt = {
    require(size.getWidth == 3 && dest.getWidth == 5 && base.getWidth == 5 && offset.getWidth == 12)
    Cat(offset, base, size, dest, "b0000011".U)
  }

  def auipc(rd: UInt, imm: UInt): UInt = {
    require(rd.getWidth == 5 && imm.getWidth == 21)
    Cat(imm(20), imm(10, 1), imm(11), imm(19, 12), rd, "b0010111".U)
  }

  def store(size: UInt, src: UInt, base: UInt, offset: UInt): UInt = {
    require(size.getWidth == 3 && src.getWidth == 5 && base.getWidth == 5 && offset.getWidth == 12)
    Cat(offset(11, 5), src, base, size, offset(4, 0), "b0100011".U)
  }

  def floatLoad(size: UInt, dest: UInt, base: UInt, offset: UInt): UInt = {
    require(size.getWidth == 3 && dest.getWidth == 5 && base.getWidth == 5 && offset.getWidth == 12)
    Cat(offset, base, size, dest, "b0000111".U)
  }

  def floatStore(size: UInt, src: UInt, base: UInt, offset: UInt): UInt = {
    require(size.getWidth == 3 && src.getWidth == 5 && base.getWidth == 5 && offset.getWidth == 12)
    Cat(offset(11, 5), src, base, size, offset(4, 0), "b0100111".U)
  }

  def csrw(csr: UInt, rs1: UInt): UInt = {
    require(csr.getWidth == 12 && rs1.getWidth == 5)
    Cat(csr, rs1, "b001".U, "b00000".U, "b1110011".U)
  }

  def csrr(csr: UInt, dest: UInt): UInt = {
    require(csr.getWidth == 12 && dest.getWidth == 5)
    Cat(csr, "b00000".U, "b010".U, dest, "b1110011".U)
  }

  def branch(src2: UInt, src1: UInt, funct3: UInt, offset: UInt): UInt = {
    require(src2.getWidth == 5 && src1.getWidth == 5 && funct3.getWidth == 3 && offset.getWidth == 12)
    Cat(offset(11), offset(9, 4), src2, src1, funct3, offset(3, 0), offset(10), "b1100011".U)
  }

  def ebreak: UInt = {
    32.U(32.W) // 32-bit constant for ebreak instruction
  }

  def wfi: UInt = {
    32.U(32.W) // 32-bit constant for wfi instruction
  }

  def nop: UInt = {
    32.U(32.W) // 32-bit constant for nop instruction
  }

  def illegal: UInt = {
    0.U(32.W) // 32-bit constant for illegal instruction
  }

  // trace log compatible to spikes commit log feature
  //function string spikeCommitLog(logic [63:0] pc, priv_lvl_t priv_lvl, logic [31:0] instr, logic [4:0] rd, logic [63:0] result, logic rd_fpr);
  //    string rd_s;
  //    string instr_word;
  //
  //    automatic string rf_s = rd_fpr ? "f" : "x";
  //
  //    if (instr[1:0] != 2'b11) begin
  //      instr_word = $sformatf("(0x%h)", instr[15:0]);
  //    end else begin
  //      instr_word = $sformatf("(0x%h)", instr);
  //    end
  //
  //    if (rd < 10) rd_s = $sformatf("%s %0d", rf_s, rd);
  //    else rd_s = $sformatf("%s%0d", rf_s, rd);
  //
  //    if (rd_fpr || rd != 0) begin
  //        // 0 0x0000000080000118 (0xeecf8f93) x31 0x0000000080004000
  //        return $sformatf("%d 0x%h %s %s 0x%h\n", priv_lvl, pc, instr_word, rd_s, result);
  //    end else begin
  //        // 0 0x000000008000019c (0x0040006f)
  //        return $sformatf("%d 0x%h %s\n", priv_lvl, pc, instr_word);
  //    end
  //endfunction
  // pragma translate_off
  //
  //typedef struct packed {
  //  byte priv;
  //  longint unsigned pc;
  //  byte is_fp;
  //  byte rd;
  //  longint unsigned data;
  //  int unsigned instr;
  //  byte was_exception;
  //} commit_log_t
}