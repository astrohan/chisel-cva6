package cva6.include

import chisel3._
import chisel3.util.{Cat, log2Ceil}

object dm {
  val DbgVersion013 = 2
  // size of program buffer in junks of 32-bit words
  val ProgBufSize   = 8

  // amount of data count registers implemented
  val DataCount     = 2

  // address to which a hart should jump when it was requested to halt
  val HaltAddress = 8
  val ResumeAddress = HaltAddress + 4
  val ExceptionAddress = HaltAddress + 8

  // address where data0-15 is shadowed or if shadowed in a CSR
  // address of the first CSR used for shadowing the data
  val DataAddr = 0x380; // we are aligned with Rocket here

  // debug registers
  object dm_csr_e extends ChiselEnum {
    val Data0        = Value(0x04.U(8.W))
    val Data1        = Value(0x05.U(8.W))
    val Data2        = Value(0x06.U(8.W))
    val Data3        = Value(0x07.U(8.W))
    val Data4        = Value(0x08.U(8.W))
    val Data5        = Value(0x09.U(8.W))
    val Data6        = Value(0x0A.U(8.W))
    val Data7        = Value(0x0B.U(8.W))
    val Data8        = Value(0x0C.U(8.W))
    val Data9        = Value(0x0D.U(8.W))
    val Data10       = Value(0x0E.U(8.W))
    val Data11       = Value(0x0F.U(8.W))
    val DMControl    = Value(0x10.U(8.W))
    val DMStatus     = Value(0x11.U(8.W)) // r/o
    val Hartinfo     = Value(0x12.U(8.W))
    val HaltSum1     = Value(0x13.U(8.W))
    val HAWindowSel  = Value(0x14.U(8.W))
    val HAWindow     = Value(0x15.U(8.W))
    val AbstractCS   = Value(0x16.U(8.W))
    val Command      = Value(0x17.U(8.W))
    val AbstractAuto = Value(0x18.U(8.W))
    val DevTreeAddr0 = Value(0x19.U(8.W))
    val DevTreeAddr1 = Value(0x1A.U(8.W))
    val DevTreeAddr2 = Value(0x1B.U(8.W))
    val DevTreeAddr3 = Value(0x1C.U(8.W))
    val NextDM       = Value(0x1D.U(8.W))
    val ProgBuf0     = Value(0x20.U(8.W))
    val ProgBuf15    = Value(0x2F.U(8.W))
    val AuthData     = Value(0x30.U(8.W))
    val HaltSum2     = Value(0x34.U(8.W))
    val HaltSum3     = Value(0x35.U(8.W))
    val SBAddress3   = Value(0x37.U(8.W))
    val SBCS         = Value(0x38.U(8.W))
    val SBAddress0   = Value(0x39.U(8.W))
    val SBAddress1   = Value(0x3A.U(8.W))
    val SBAddress2   = Value(0x3B.U(8.W))
    val SBData0      = Value(0x3C.U(8.W))
    val SBData1      = Value(0x3D.U(8.W))
    val SBData2      = Value(0x3E.U(8.W))
    val SBData3      = Value(0x3F.U(8.W))
    val HaltSum0     = Value(0x40.U(8.W))
  }

  // debug causes
  val CauseBreakpoint = 1
  val CauseTrigger    = 2
  val CauseRequest    = 3
  val CauseSingleStep = 4

  class dmstatus_t extends Bundle{
    val zero1           = UInt(9.W)
    val impebreak       = Bool()
    val zero0           = UInt(2.W)
    val allhavereset    = Bool()
    val anyhavereset    = Bool()
    val allresumeack    = Bool()
    val anyresumeack    = Bool()
    val allnonexistent  = Bool()
    val anynonexistent  = Bool()
    val allunavail      = Bool()
    val anyunavail      = Bool()
    val allrunning      = Bool()
    val anyrunning      = Bool()
    val allhalted       = Bool()
    val anyhalted       = Bool()
    val authenticated   = Bool()
    val authbusy        = Bool()
    val hasresethaltreq = Bool()
    val devtreevalid    = Bool()
    val version         = UInt(4.W)
  }

  class dmcontrol_t extends Bundle {
    val haltreq         = Bool()
    val resumereq       = Bool()
    val hartreset       = Bool()
    val ackhavereset    = Bool()
    val zero1           = Bool()
    val hasel           = Bool()
    val hartsello       = UInt(10.W)
    val hartselhi       = UInt(10.W)
    val zero0           = UInt(2.W)
    val setresethaltreq = Bool()
    val clrresethaltreq = Bool()
    val ndmreset        = Bool()
    val dmactive        = Bool()
  }

  class hartinfo_t extends Bundle {
    val zero1      = UInt(8.W)
    val nscratch   = UInt(4.W)
    val zero0      = UInt(3.W)
    val dataaccess = Bool()
    val datasize   = UInt(4.W)
    val dataaddr   = UInt(12.W)
  }

  object cmderr_e extends ChiselEnum {
    val CmdErrNone,
        CmdErrBusy,
        CmdErrNotSupported,
        CmdErrorException,
        CmdErrorHaltResume,
        CmdErrorBus,
        CmdErrorOther = Value(7.U(3.W))
  }

  class abstractcs_t extends Bundle {
    val zero3       = UInt(3.W)
    val progbufsize = UInt(5.W)
    val zero2       = UInt(11.W)
    val busy        = Bool()
    val zero1       = Bool()
    val cmderr      = cmderr_e()
    val zero0       = UInt(4.W)
    val datacount   = UInt(4.W)
  }

  object cmd_e extends ChiselEnum {
    val AccessRegister,
        QuickAccess,
        AccessMemory = Value(2.U(8.W))
  }

  class command_t extends Bundle {
    val cmdType = cmd_e()
    val control = UInt(24.W)
  }

  class abstractauto_t extends Bundle {
    val autoexecprogbuf = UInt(16.W)
    val zero0           = UInt(4.W)
    val autoexecdata    = UInt(12.W)
  }

  class ac_ar_cmd_t extends Bundle {
    val zero1            = Bool()
    val aarsize          = UInt(3.W)
    val aarpostincrement = Bool()
    val postexec         = Bool()
    val transfer         = Bool()
    val write            = Bool()
    val regno            = UInt(16.W)
  }

  // DTM
  object dtm_op_e extends ChiselEnum {
    val DTM_NOP   ,
        DTM_READ  ,
        DTM_WRITE = Value(2.U)
  }

  class sbsc_t extends Bundle {
    val sbversion       = UInt(2.W)
    val zero0           = UInt(4.W)
    val sbbusyerror     = Bool()
    val sbbusy          = Bool()
    val sbreadonaddr    = Bool()
    val sbaccess        = UInt(3.W)
    val sbautoincrement = Bool()
    val sbreadondata    = Bool()
    val sberror         = UInt(3.W)
    val sbasize         = UInt(7.W)
    val sbaccess128     = Bool()
    val sbaccess64      = Bool()
    val sbaccess32      = Bool()
    val sbaccess16      = Bool()
    val sbaccess8       = Bool()
  }

  val DTM_SUCCESS = 0

  class dmi_req_t extends Bundle {
    val addr = UInt(7.W)
    val op = dtm_op_e()
    val data = UInt(32.W)
  }

  class dmi_resp_t extends Bundle {
    val data = UInt(32.W)
    val resp = UInt(2.W)
  }

  // privilege levels
  object priv_lvl_t extends ChiselEnum {
    val PRIV_LVL_M = Value(3.U(2.W))
    val PRIV_LVL_S = Value(3.U(1.W))
    val PRIV_LVL_U = Value(3.U(0.W))
  }

  // debugregs in core
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

  // CSRs
  object csr_reg_t extends ChiselEnum {
    // Floating-Point CSRs
    val  CSR_FFLAGS         = Value(0x001.U(12.W))
    val  CSR_FRM            = Value(0x002.U(12.W))
    val  CSR_FCSR           = Value(0x003.U(12.W))
    val  CSR_FTRAN          = Value(0x800.U(12.W))

    // Supervisor Mode CSRs
    val  CSR_SSTATUS        = Value(0x100.U(12.W))
    val  CSR_SIE            = Value(0x104.U(12.W))
    val  CSR_STVEC          = Value(0x105.U(12.W))
    val  CSR_SCOUNTEREN     = Value(0x106.U(12.W))
    val  CSR_SSCRATCH       = Value(0x140.U(12.W))
    val  CSR_SEPC           = Value(0x141.U(12.W))
    val  CSR_SCAUSE         = Value(0x142.U(12.W))
    val  CSR_STVAL          = Value(0x143.U(12.W))
    val  CSR_SIP            = Value(0x144.U(12.W))
    val  CSR_SATP           = Value(0x180.U(12.W))

    // Machine Mode CSRs
    val  CSR_MSTATUS        = Value(0x300.U(12.W))
    val  CSR_MISA           = Value(0x301.U(12.W))
    val  CSR_MEDELEG        = Value(0x302.U(12.W))
    val  CSR_MIDELEG        = Value(0x303.U(12.W))
    val  CSR_MIE            = Value(0x304.U(12.W))
    val  CSR_MTVEC          = Value(0x305.U(12.W))
    val  CSR_MCOUNTEREN     = Value(0x306.U(12.W))
    val  CSR_MSCRATCH       = Value(0x340.U(12.W))
    val  CSR_MEPC           = Value(0x341.U(12.W))
    val  CSR_MCAUSE         = Value(0x342.U(12.W))
    val  CSR_MTVAL          = Value(0x343.U(12.W))
    val  CSR_MIP            = Value(0x344.U(12.W))
    val  CSR_PMPCFG0        = Value(0x3A0.U(12.W))
    val  CSR_PMPADDR0       = Value(0x3B0.U(12.W))
    val  CSR_MVENDORID      = Value(0xF11.U(12.W))
    val  CSR_MARCHID        = Value(0xF12.U(12.W))
    val  CSR_MIMPID         = Value(0xF13.U(12.W))
    val  CSR_MHARTID        = Value(0xF14.U(12.W))
    val  CSR_MCYCLE         = Value(0xB00.U(12.W))
    val  CSR_MINSTRET       = Value(0xB02.U(12.W))
    // Cache Control(custom)
    val  CSR_DCACHE         = Value(0x701.U(12.W))
    val  CSR_ICACHE         = Value(0x700.U(12.W))
    // Triggers
    val  CSR_TSELECT        = Value(0x7A0.U(12.W))
    val  CSR_TDATA1         = Value(0x7A1.U(12.W))
    val  CSR_TDATA2         = Value(0x7A2.U(12.W))
    val  CSR_TDATA3         = Value(0x7A3.U(12.W))
    val  CSR_TINFO          = Value(0x7A4.U(12.W))
    // Debug CSR
    val  CSR_DCSR           = Value(0x7b0.U(12.W))
    val  CSR_DPC            = Value(0x7b1.U(12.W))
    val  CSR_DSCRATCH0      = Value(0x7b2.U(12.W)) // optional
    val  CSR_DSCRATCH1      = Value(0x7b3.U(12.W)) // optional
    // Counters and Timers
    val  CSR_CYCLE          = Value(0xC00.U(12.W))
    val  CSR_TIME           = Value(0xC01.U(12.W))
    val  CSR_INSTRET        = Value(0xC02.U(12.W))
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
}