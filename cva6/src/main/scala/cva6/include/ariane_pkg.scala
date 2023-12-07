package cva6.include

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.{Cat, Fill, MuxLookup, log2Ceil}
import cva6.include.ariane_pkg.fu_op.{VFCPKAB_D, VFCPKAB_S, VFCPKCD_S}
import org.chipsalliance.cde.config.Field

case class ArianeParamsKey(hartId: Int = 0) extends Field[ariane_pkg.ArianeParams](ariane_pkg.ArianeParams())

object ariane_pkg {
  val NrMaxRules: Int = 16
  val WT_DCACHE: Boolean = false
  val PITON_ARIANE: Boolean = false
  val SPIKE_TANDEM: Boolean = false

  case class ArianeParams(
    RASDepth: Int = 2,
    BTBEntries: Int = 32,
    BHTEntries: Int = 128,
    // PMP: idempotent regions
    NrNonIdempotentRules: Int = 2,
    NonIdempotentAddrBase: Seq[BigInt] = Seq.fill(2)(0),
    NonIdempotentLength: Seq[BigInt] = Seq.fill(2)(0),
    // PMA: exe regions
    NrExecuteRegionRules: Int = 3,
    ExecuteRegionAddrBase: Seq[BigInt] = Seq(0x80000000L, 0x10000L, 0x0L), // DRAM, BootRom, DebugModule
    ExecuteRegionLength: Seq[BigInt] = Seq(0x40000000L, 0x10000L, 0x1000L),
    // PMA: cached regions
    NrCachedRegionRules: Int = 1,
    CachedRegionAddrBase: Seq[BigInt] = Seq(0x80000000L),
    CachedRegionLength: Seq[BigInt] = Seq(0x40000000L),
    // cache config
    Axi64bitCompliant: Boolean = true,
    SwapEndianess: Boolean = false,
    // debug
    DmBaseAddress: BigInt = 0x0L,
  ) {
    require(RASDepth > 0)
    require(1 << log2Ceil(BTBEntries) == BTBEntries)
    require(1 << log2Ceil(BHTEntries) == BHTEntries)
    require(NrNonIdempotentRules <= NrMaxRules)
    require(NrExecuteRegionRules <= NrMaxRules)
    require(NrCachedRegionRules <= NrMaxRules)
  }

  def range_check(base: BigInt, len: BigInt, address: BigInt): Boolean = {
    (address >= base) && (address < (base + len))
  }

  def is_inside_nonidempotent_regions(params: ArianeParams, address: BigInt): Boolean = {
    Seq.tabulate(params.NrNonIdempotentRules){ k =>
      range_check(params.NonIdempotentAddrBase(k), params.NonIdempotentLength(k), address)
    }.exists(identity)
  }

  def is_inside_execute_regions(params: ArianeParams, address: BigInt): Boolean = {
    Seq.tabulate(params.NrExecuteRegionRules){ k =>
      range_check(params.ExecuteRegionAddrBase(k), params.ExecuteRegionLength(k), address)
    }.exists(identity)
  }

  def is_inside_cacheable_regions(params: ArianeParams, address: BigInt): Boolean = {
    Seq.tabulate(params.NrCachedRegionRules){ k =>
      range_check(params.CachedRegionAddrBase(k), params.CachedRegionLength(k), address)
    }.exists(identity)
  }


  // TODO: Slowly move those parameters to the new system.
  val NR_SB_ENTRIES = 8 // number of scoreboard entries
  val TRANS_ID_BITS = log2Ceil(NR_SB_ENTRIES) // depending on the number of scoreboard entries we need that many bits
                                              // to uniquely identify the entry in the scoreboard
  val ASID_WIDTH    = 1
  val BITS_SATURATION_COUNTER = 2
  val NR_COMMIT_PORTS = 2

  val ENABLE_RENAME = true

  val ISSUE_WIDTH = 1
  // amount of pipeline registers inserted for load/store return path
  // this can be tuned to trade-off IPC vs. cycle time
  val NR_LOAD_PIPE_REGS = 1
  val NR_STORE_PIPE_REGS = 0

  // depth of store-buffers, this needs to be a power of two
  val DEPTH_SPEC   = 4

  // in this case we can use a small commit queue since we have a write buffer in the dcache
  // we could in principle do without the commit queue in this case, but the timing degrades if we do that due
  // to longer paths into the commit stage
  // if not WT_DCACHE, allocate more space for the commit buffer to be on the save side, this needs to be a power of two
  val DEPTH_COMMIT = if(WT_DCACHE) 4 else 8

  // Floating-point extensions configuration
  val RVF = true // Is F extension enabled
  val RVD = true // Is D extension enabled
  val RVA = true // Is A extension enabled

  // Transprecision floating-point extensions configuration
  val XF16    = false // Is half-precision float extension (Xf16) enabled
  val XF16ALT = false // Is alternative half-precision float extension (Xf16alt) enabled
  val XF8     = false // Is quarter-precision float extension (Xf8) enabled
  val XFVEC   = false // Is vectorial float extension (Xfvec) enabled

  // Transprecision float unit
  val LAT_COMP_FP32    = 2
  val LAT_COMP_FP64    = 3
  val LAT_COMP_FP16    = 1
  val LAT_COMP_FP16ALT = 1
  val LAT_COMP_FP8     = 1
  val LAT_DIVSQRT      = 2
  val LAT_NONCOMP      = 1
  val LAT_CONV         = 2

  // --------------------------------------
  // vvvv Don't change these by hand! vvvv
  val FP_PRESENT = RVF | RVD | XF16 | XF16ALT | XF8

  // Length of widest floating-point format
  val FLEN: Int = (RVD, RVF, XF16, XF16ALT, XF8) match {
    case (true, _, _, _, _) => 64 // D extension
    case (false, true, _, _, _) => 32 // F extension
    case (false, false, true, _, _) => 16 // Xf16 extension
    case (false, false, false, true, _) => 16 // Xf16alt extension
    case (false, false, false, false, true) => 8 // Xf8 extension
    case _ => 0 // Unused in case of no FP
  }

  val NSX = XF16 | XF16ALT | XF8 | XFVEC // Are non-standard extensions present?

  val RVFVEC     = RVF     & XFVEC & FLEN>32 // FP32 vectors available if vectors and larger fmt enabled
  val XF16VEC    = XF16    & XFVEC & FLEN>16 // FP16 vectors available if vectors and larger fmt enabled
  val XF16ALTVEC = XF16ALT & XFVEC & FLEN>16 // FP16ALT vectors available if vectors and larger fmt enabled
  val XF8VEC     = XF8     & XFVEC & FLEN>8  // FP8 vectors available if vectors and larger fmt enabled
  // ^^^^ until here ^^^^
  // ---------------------

  val ARIANE_MARCHID = 3.U(64.W)

  val ISA_CODE = (({if(RVA) 1 else 0} << 0)  // A - Atomic Instructions extension
               + (1   <<  2)  // C - Compressed extension
               + ({if(RVD) 1 else 0} <<  3)  // D - Double precsision floating-point extension
               + ({if(RVF) 1 else 0} <<  5)  // F - Single precsision floating-point extension
               + (1   <<  8)  // I - RV32I/64I/128I base ISA
               + (1   << 12)  // M - Integer Multiply/Divide extension
               + (0   << 13)  // N - User level interrupts supported
               + (1   << 18)  // S - Supervisor mode implemented
               + (1   << 20)  // U - User mode implemented
               + ({if(NSX) 1 else 0} << 23)  // X - Non-standard extensions present
               + (1   << 63)).U(64.W) // RV64

  // 32 registers + 1 bit for re-naming = 6
  val REG_ADDR_SIZE = 6
  val NR_WB_PORTS = 4

  // static debug hartinfo
  val DebugHartInfo = (new dm.hartinfo_t()).Lit(
    _.nscratch -> 2.U, // Debug module needs at least two scratch regs
    _.dataaccess -> true.B, // data registers are memory mapped in the debugger
    _.datasize -> dm.DataCount.U,
    _.dataaddr -> dm.DataAddr.U
  )

  // enables a commit log which matches spikes commit log format for easier trace comparison
  val ENABLE_SPIKE_COMMIT_LOG = true

  // ------------- Dangerouse -------------
  // if set to zero a flush will not invalidate the cache-lines, in a single core environment
  // where coherence is not necessary this can improve performance. This needs to be switched on
  // when more than one core is in a system
  val INVALIDATE_ON_FLUSH = true

  val ENABLE_CYCLE_COUNT = if(SPIKE_TANDEM) false else true
  val ENABLE_WFI = if(SPIKE_TANDEM) false else true
  val ZERO_TVAL = if(SPIKE_TANDEM) true else false

  // read mask for SSTATUS over MMSTATUS
  val SMODE_STATUS_READ_MASK = (riscv_pkg.SSTATUS_UIE  |
                                riscv_pkg.SSTATUS_SIE  |
                                riscv_pkg.SSTATUS_SPIE |
                                riscv_pkg.SSTATUS_SPP  |
                                riscv_pkg.SSTATUS_FS   |
                                riscv_pkg.SSTATUS_XS   |
                                riscv_pkg.SSTATUS_SUM  |
                                riscv_pkg.SSTATUS_MXR  |
                                riscv_pkg.SSTATUS_UPIE |
                                riscv_pkg.SSTATUS_SPIE |
                                riscv_pkg.SSTATUS_UXL  |
                                riscv_pkg.SSTATUS64_SD).U(64.W)

  val SMODE_STATUS_WRITE_MASK = (riscv_pkg.SSTATUS_SIE  |
                                 riscv_pkg.SSTATUS_SPIE |
                                 riscv_pkg.SSTATUS_SPP  |
                                 riscv_pkg.SSTATUS_FS   |
                                 riscv_pkg.SSTATUS_SUM  |
                                 riscv_pkg.SSTATUS_MXR).U(64.W)

  // ---------------
  // Fetch Stage
  // ---------------

  // leave as is (fails with >8 entries and wider fetch width)
  val FETCH_FIFO_DEPTH  = 4
  val FETCH_WIDTH       = 32
  // maximum instructions we can fetch on one request (we support compressed instructions)
  val INSTR_PER_FETCH = FETCH_WIDTH / 16

  // Only use struct when signals have same direction
  // exception
  class exception_t extends Bundle {
    val cause = UInt(64.W) // cause of exception
    val tval  = UInt(64.W) // additional information of causing exception (e.g.: instruction causing it),
                           // address of LD/ST fault
    val valid = Bool()
  }

  object cf_t extends ChiselEnum {
    val NoCF,   // No control flow prediction
        Branch, // Branch
        Jump,   // Jump to address from immediate
        JumpR,  // Jump to address from registers
        Return  = Value(UInt(2.W))// Return Address Prediction
  }
  import cf_t._

  // branch-predict
  // this is the struct we get back from ex stage and we will use it to update
  // all the necessary data structures
  // bp_resolve_t
  class bp_resolve_t extends Bundle {
    val valid           = Bool() // prediction with all its values is valid
    val pc              = UInt(64.W) // PC of predict or mis-predict
    val target_address  = UInt(64.W) // target address at which to jump, or not
    val is_mispredict   = Bool() // set if this was a mis-predict
    val is_taken        = Bool() // branch is taken
    val cf_type         = cf_t() // Type of control flow change
  }

  // branchpredict scoreboard entry
  // this is the struct which we will inject into the pipeline to guide the various
  // units towards the correct branch decision and resolve
  class branchpredict_sbe_t extends Bundle {
    val cf              = cf_t() // type of control flow prediction
    val predict_address = UInt(64.W) // target address at which to jump, or not
  }

  class btb_update_t extends Bundle {
    val valid          = Bool()
    val pc             = UInt(64.W) // update at PC
    val target_address = UInt(64.W)
  }

  class btb_prediction_t extends Bundle {
    val valid          = Bool()
    val target_address = UInt(64.W)
  }

  class ras_t extends Bundle {
    val valid   = Bool()
    val ra      = UInt(64.W)
  }

  class bht_update_t extends Bundle {
    val valid  = Bool()
    val pc     = UInt(64.W) // update at PC
    val taken  = Bool()
  }

  class bht_prediction_t extends Bundle {
    val valid = Bool()
    val taken = Bool()
  }

  object fu_t extends ChiselEnum {
    val NONE       = Value(0.U(3.W))
    val LOAD       = Value(1.U(3.W))
    val STORE      = Value(2.U(3.W))
    val ALU        = Value(3.U(3.W))
    val CTRL_FLOW  = Value(4.U(3.W))
    val MULT       = Value(5.U(3.W))
    val CSR        = Value(6.U(3.W))
    val FPU        = Value(7.U(3.W))
    val FPU_VEC    = Value(8.U(3.W))
  }
  import fu_t._

  val EXC_OFF_RST  = 0x80

  val SupervisorIrq = 1
  val MachineIrq = 0

  // All information needed to determine whether we need to associate an interrupt
  // with the corresponding instruction or not.
  class irq_ctrl_t extends Bundle {
    val mie           = UInt(64.W)
    val mip           = UInt(64.W)
    val mideleg       = UInt(64.W)
    val sie           = Bool()
    val global_enable = Bool()
  }

  // ---------------
  // Cache config
  // ---------------
  // align to openpiton for the time being (this should be more configurable in the future)
   // I$
  val ICACHE_INDEX_WIDTH = 12  // in bit
  val ICACHE_TAG_WIDTH   = 44  // in bit
  val ICACHE_LINE_WIDTH  = 128 // in bit
  val ICACHE_SET_ASSOC   = 4
  // D$
  val DCACHE_INDEX_WIDTH = 12  // in bit
  val DCACHE_TAG_WIDTH   = 44  // in bit
  val DCACHE_LINE_WIDTH  = 128 // in bit
  val DCACHE_SET_ASSOC   = 8

  // ---------------
  // EX Stage
  // ---------------
  object fu_op extends ChiselEnum {
    // basic ALU op
    val ADD, SUB, ADDW, SUBW,
    // logic operations
    XORL, ORL, ANDL,
    // shifts
    SRA, SRL, SLL, SRLW, SLLW, SRAW,
    // comparisons
    LTS, LTU, GES, GEU, EQ, NE,
    // jumps
    JALR, BRANCH,
    // set lower than operations
    SLTS, SLTU,
    // CSR functions
    MRET, SRET, DRET, ECALL, WFI, FENCE, FENCE_I, SFENCE_VMA, CSR_WRITE, CSR_READ, CSR_SET, CSR_CLEAR,
    // LSU functions
    LD, SD, LW, LWU, SW, LH, LHU, SH, LB, SB, LBU,
    // Atomic Memory Operations
    AMO_LRW, AMO_LRD, AMO_SCW, AMO_SCD,
    AMO_SWAPW, AMO_ADDW, AMO_ANDW, AMO_ORW, AMO_XORW, AMO_MAXW, AMO_MAXWU, AMO_MINW, AMO_MINWU,
    AMO_SWAPD, AMO_ADDD, AMO_ANDD, AMO_ORD, AMO_XORD, AMO_MAXD, AMO_MAXDU, AMO_MIND, AMO_MINDU,
    // Multiplications
    MUL, MULH, MULHU, MULHSU, MULW,
    // Divisions
    DIV, DIVU, DIVW, DIVUW, REM, REMU, REMW, REMUW,
    // Floating-Point Load and Store Instructions
    FLD, FLW, FLH, FLB, FSD, FSW, FSH, FSB,
    // Floating-Point Computational Instructions
    FADD, FSUB, FMUL, FDIV, FMIN_MAX, FSQRT, FMADD, FMSUB, FNMSUB, FNMADD,
    // Floating-Point Conversion and Move Instructions
    FCVT_F2I, FCVT_I2F, FCVT_F2F, FSGNJ, FMV_F2X, FMV_X2F,
    // Floating-Point Compare Instructions
    FCMP,
    // Floating-Point Classify Instruction
    FCLASS,
    // Vectorial Floating-Point Instructions that don't directly map onto the scalar ones
    VFMIN, VFMAX, VFSGNJ, VFSGNJN, VFSGNJX, VFEQ, VFNE, VFLT, VFGE, VFLE, VFGT, VFCPKAB_S, VFCPKCD_S, VFCPKAB_D, VFCPKCD_D = Value
  }
  import fu_op._

  class fu_data_t extends Bundle {
    val fu        = fu_t()
    val operator  = fu_op()
    val operand_a = UInt(64.W)
    val operand_b = UInt(64.W)
    val imm       = UInt(64.W)
    val trans_id  = UInt(TRANS_ID_BITS.W)
  }

  def is_branch (op: fu_op.Type) = op match {
      case EQ | NE | LTS | GES | LTU | GEU => true.B
      case _ => false.B
    }

  // -------------------------------
  // Extract Src/Dst FP Reg from Op
  // -------------------------------
  def is_rs1_fpr(op: fu_op.Type) = if(FP_PRESENT) {
      op match {
        case FMUL | FDIV | FMIN_MAX | FSQRT | FMADD | FMSUB | FNMSUB | FNMADD => true.B // Computational Operations (except ADD/SUB)
        case FCVT_F2I => true.B // Float-Int Casts
        case FCVT_F2F => true.B // Float-Float Casts
        case FSGNJ => true.B // Sign Injections
        case FMV_F2X => true.B // FPR-GPR Moves
        case FCMP => true.B // Comparisons
        case FCLASS => true.B // Classifications[VFMIN: VFCPKCD_D]: return 1 'b1 // Additional Vectorial FP ops
        case _ => false.B // all other ops
      }
    } else false.B

  def is_rs2_fpr (op: fu_op.Type) = if (FP_PRESENT) { // makes function static for non-fp case
      op match {
        case FSD | FSW | FSH | FSB => true.B // FP Stores
        case FADD | FSUB | FMUL | FDIV | FMIN_MAX | FSQRT | FMADD | FMSUB | FNMSUB | FNMADD => true.B // Computational Operations (no sqrt)
        case FMADD | FMSUB | FNMSUB | FNMADD => true.B // Fused Computational Operations
        case FCVT_F2F => true.B // Vectorial F2F Conversions requrie target
        case FSGNJ | FMV_F2X | FMV_X2F => true.B // Sign Injections and moves mapped to SGNJ
        case FCMP => true.B // Comparisons
        case VFMIN | VFMAX | VFSGNJ | VFSGNJN | VFSGNJX | VFEQ | VFNE | VFLT | VFGE | VFLE | VFGT | VFCPKAB_S | VFCPKCD_S | VFCPKAB_D | VFCPKCD_D => true.B
        case _ => false.B // all other ops
      }
    }
    else false.B

  // ternary operations encode the rs3 address in the imm field, also add/sub
  def is_imm_fpr(op: fu_op.Type) = if(FP_PRESENT) {
      op match {
        case FADD | FSUB => true.B // ADD/SUB need inputs as Operand B/C
        case FMADD | FMSUB | FNMSUB | FNMADD => true.B // Fused Computational Operations
        case VFCPKAB_S | VFCPKCD_S | VFCPKAB_D | VFCPKCD_D => true.B // Vectorial FP cast and pack ops
        case _ => false.B // all other ops
      }
    } else false.B

  def is_rd_fpr(op: fu_op.Type) = if(FP_PRESENT) {
    op match {
      case FLD | FLW | FLH | FLB => true.B // FP Loads
      case FADD | FSUB | FMUL | FDIV | FMIN_MAX | FSQRT | FMADD | FMSUB | FNMSUB | FNMADD => true.B // Computational Operations
      case FCVT_I2F => true.B // Int-Float Casts
      case FCVT_F2F => true.B // Float-Float Casts
      case FSGNJ => true.B // Sign Injections
      case FMV_X2F => true.B // GPR-FPR Moves
      case VFMIN | VFMAX | VFSGNJ | VFSGNJN | VFSGNJX => true.B // Vectorial MIN/MAX and SGNJ
      case VFCPKAB_S | VFCPKCD_S | VFCPKAB_D | VFCPKCD_D => true.B  // Vectorial FP cast and pack ops
      case _ => false.B // all other ops
    }
  } else false.B

  def is_rd_fpr(op: fu_op.Type) = if(FP_PRESENT) {
    op match {
      case FLD | FLH | FLB | FLW => true.B // FP Loads
      case FADD | FSUB | FMUL | FDIV | FMIN_MAX | FSQRT | FMADD | FMSUB | FNMSUB | FNMADD => true.B // Computational Operations
      case FCVT_I2F => true.B // Int-Float Casts
      case FCVT_F2F => true.B // Float-Float Casts
      case FSGNJ => true.B // Sign Injections
      case FMV_X2F => true.B // GPR-FPR Moves
      case VFMIN | VFMAX | VFSGNJ | VFSGNJN | VFSGNJX => true.B // Vectorial MIN/MAX and SGNJ
      case VFCPKAB_S | VFCPKCD_S | VFCPKAB_D | VFCPKCD_D => true.B // Vectorial FP cast and pack ops
      case _ => false.B // all other ops
    }
  } else false.B

  def is_amo(op: fu_op.Type) = ((op.litValue >= AMO_LRW.litValue) && (op.litValue <= AMO_MINDU.litValue)).asBool

  class lsu_ctrl_t extends Bundle {
    val valid = Bool()
    val vaddr = UInt(64.W)
    val data = UInt(64.W)
    val be = UInt(8.W)
    val fu = fu_t()
    val operator = fu_op()
    val trans_id = UInt(TRANS_ID_BITS.W)
  }

  // ---------------
  // IF/ID Stage
  // ---------------
  // store the decompressed instruction
  class fetch_entry_t extends Bundle {
    val address = UInt(64.W) // the address of the instructions from below
    val instruction = UInt(32.W) // instruction word
    val branch_predict = new branchpredict_sbe_t // this field contains branch prediction information regarding the forward branch path
    val ex = new exception_t // this field contains exceptions which might have happened earlier, e.g.: fetch exceptions
  }

  // ---------------
  // ID/EX/WB Stage
  // ---------------
  class scoreboard_entry_t extends Bundle {
    val pc = UInt(64.W) // PC of instruction
    val trans_id = UInt(TRANS_ID_BITS.W) // this can potentially be simplified, we could index the scoreboard entry
    val // with the transaction id in any case make the width more generic
    val fu = fu_t() // functional unit to use
    val op = fu_op() // operation to perform in each functional unit
    val rs1 = UInt(REG_ADDR_SIZE.W) // register source address 1
    val rs2 = UInt(REG_ADDR_SIZE.W) // register source address 2
    val rd = UInt(REG_ADDR_SIZE.W) // register destination address
    val result = UInt(64.W) // for unfinished instructions this field also holds the immediate,
                            // for unfinished floating-point that are partly encoded in rs2, this field also holds rs2
                            // for unfinished floating-point fused operations (FMADD, FMSUB, FNMADD, FNMSUB)
                            // this field holds the address of the third operand from the floating-point register file
    val valid = Bool() // is the result valid
    val use_imm = Bool() // should we use the immediate as operand b?
    val use_zimm = Bool() // use zimm as operand a
    val use_pc = Bool() // set if we need to use the PC as operand a, PC from exception
    val ex = new exception_t // exception has occurred
    val bp = new branchpredict_sbe_t // branch predict scoreboard data structure
    val is_compressed = Bool() // signals a compressed instructions, we need this information at the commit stage if
                               // we want jump accordingly e.g.: +4, +2
  }

  // --------------------
  // Atomics
  // --------------------
  object amo_t extends ChiselEnum {
    val AMO_NONE = Value("b0000".U(4.W))
    val AMO_LR   = Value("b0001".U(4.W))
    val AMO_SC   = Value("b0010".U(4.W))
    val AMO_SWAP = Value("b0011".U(4.W))
    val AMO_ADD  = Value("b0100".U(4.W))
    val AMO_AND  = Value("b0101".U(4.W))
    val AMO_OR   = Value("b0110".U(4.W))
    val AMO_XOR  = Value("b0111".U(4.W))
    val AMO_MAX  = Value("b1000".U(4.W))
    val AMO_MAXU = Value("b1001".U(4.W))
    val AMO_MIN  = Value("b1010".U(4.W))
    val AMO_MINU = Value("b1011".U(4.W))
    val AMO_CAS1 = Value("b1100".U(4.W)) // unused, not part of riscv spec, but provided in OpenPiton
    val AMO_CAS2 = Value("b1101".U(4.W)) // unused, not part of riscv spec, but provided in OpenPiton
  }

  class tlb_update_t extends Bundle {
    val valid = Bool() // valid flag
    val is_2M = Bool()
    val is_1G = Bool()
    val vpn = UInt(27.W)
    val asid = UInt(ASID_WIDTH.W)
    val content = new riscv_pkg.pte_t
  }

  val MODE_SV39 = 0x8.U(4.W)
  val MODE_OFF = 0x0.U(4.W)

  // Bits required for representation of physical address space as 4K pages
  // (e.g. 27*4K == 39bit address space).
  val PPN4K_WIDTH = 38

  // ----------------------
  // cache request ports
  // ----------------------
  // I$ address translation requests
  class icache_areq_i_t extends Bundle {
    val fetch_valid = Bool() // address translation valid
    val fetch_paddr = UInt(64.W) // physical address in
    val fetch_exception = new exception_t // exception occurred during fetch
  }

  class icache_areq_o_t extends Bundle {
    val fetch_req = Bool() // address translation request
    val fetch_vaddr = UInt(64.W) // virtual address out
  }

  // I$ data requests
  class icache_dreq_i_t extends Bundle {
    val req = Bool() // we request a new word
    val kill_s1 = Bool() // kill the current request
    val kill_s2 = Bool() // kill the last request
    val vaddr = UInt(64.W) // 1st cycle: 12 bit index is taken for lookup
  }

  class icache_dreq_o_t extends Bundle {
    val ready = Bool() // icache is ready
    val valid = Bool() // signals a valid read
    val data = UInt(FETCH_WIDTH.W) // 2+ cycle out: tag
    val vaddr = UInt(64.W) // virtual address out
    val ex = new exception_t // we've encountered an exception
  }

  // AMO request going to cache. this request is unconditionally valid as soon
  // as request goes high.
  // Furthermore, those signals are kept stable until the response indicates
  // completion by asserting ack.
  class amo_req_t extends Bundle {
    val req = Bool() // this request is valid
    val amo_op = amo_t() // atomic memory operation to perform
    val size = UInt(2.W) // 2'b10 --> word operation, 2'b11 --> double word operation
    val operand_a = UInt(64.W) // address
    val operand_b = UInt(64.W) // data as layuoted in the register
  }

  // AMO response coming from cache.
  class amo_resp_t extends Bundle {
    val ack = Bool() // response is valid
    val result = UInt(64.W) // sign-extended, result
  }

  // D$ data requests
  class dcache_req_i_t extends Bundle {
    val address_index = UInt(DCACHE_INDEX_WIDTH.W)
    val address_tag = UInt(DCACHE_TAG_WIDTH.W)
    val data_wdata = UInt(64.W)
    val data_req = Bool()
    val data_we = Bool()
    val data_be = UInt(8.W)
    val data_size = UInt(2.W)
    val kill_req = Bool()
    val tag_valid = Bool()
  }

  class dcache_req_o_t extends Bundle {
    val data_gnt = Bool()
    val data_rvalid = Bool()
    val data_rdata = UInt(64.W)
  }

  // ----------------------
  // Arithmetic Functions
  // ----------------------
  def sext32(operand: UInt) = Fill(32, operand(31)) ## operand(31,0)

  // ----------------------
  // Immediate functions
  // ----------------------
  def uj_imm(instruction_i: UInt) = Fill(44, instruction_i(31)) ## instruction_i(19,12) ## instruction_i(20) ## instruction_i(30,21) ## 0.U(1.W)
  def i_imm(instruction_i: UInt) =  Fill(52, instruction_i(31)) ## instruction_i(31,20)
  def sb_imm(instruction_i: UInt) = Fill(51, instruction_i(31)) ## instruction_i(31) ## instruction_i(7) ## instruction_i(30,25) ## instruction_i(11,8) ## 0.U(1.W)

  // ----------------------
  // LSU Functions
  // ----------------------
  // align data to address e.g.: shift data to be naturally 64
  def data_align (addr: UInt, data: UInt) = MuxLookup(addr, data)(Seq(
    "b000".U(3.W) -> data,
    "b001".U(3.W) -> data(55, 0) ## data(63, 56),
    "b010".U(3.W) -> data(47, 0) ## data(63, 48),
    "b011".U(3.W) -> data(39, 0) ## data(63, 40),
    "b100".U(3.W) -> data(31, 0) ## data(63, 32),
    "b101".U(3.W) -> data(23, 0) ## data(63, 24),
    "b110".U(3.W) -> data(15, 0) ## data(63, 16),
    "b111".U(3.W) -> data(7, 0) ## data(63, 8)
  ))

  // generate byte enable mask
  def be_gen(addr: UInt, size: UInt) = MuxLookup(size, 0.U(8.W))(Seq(
    "b11".U -> "b1111_1111".U,
    "b10".U -> MuxLookup(addr, 0.U(8.W))(Seq(
      "b000".U -> "b0000_1111".U,
      "b001".U -> "b0001_1110".U,
      "b010".U -> "b0011_1100".U,
      "b011".U -> "b0111_1000".U,
      "b100".U -> "b1111_0000".U
    )),
    "b01".U -> MuxLookup(addr, 0.U(8.W))(Seq(
      "b000".U -> "b0000_0011".U,
      "b001".U -> "b0000_0110".U,
      "b010".U -> "b0000_1100".U,
      "b011".U -> "b0001_1000".U,
      "b100".U -> "b0011_0000".U,
      "b101".U -> "b0110_0000".U,
      "b110".U -> "b1100_0000".U
    )),
    "b00".U -> MuxLookup(addr, 0.U(8.W))(Seq(
      "b000".U -> "b0000_0001".U,
      "b001".U -> "b0000_0010".U,
      "b010".U -> "b0000_0100".U,
      "b011".U -> "b0000_1000".U,
      "b100".U -> "b0001_0000".U,
      "b101".U -> "b0010_0000".U,
      "b110".U -> "b0100_0000".U,
      "b111".U -> "b1000_0000".U
    ))
  ))

  // ----------------------
  // Extract Bytes from Op
  // ----------------------
  def extract_transfer_size(op: fu_op.Type) = op match {
    case LD | SD | FLD | FSD |
         AMO_LRD | AMO_SCD |
         AMO_SWAPD | AMO_ADDD |
         AMO_ANDD | AMO_ORD |
         AMO_XORD | AMO_MAXD |
         AMO_MAXDU | AMO_MIND |
         AMO_MINDU => 3.U(2.W)
    case LW | LWU | SW | FLW | FSW |
         AMO_LRW | AMO_SCW |
         AMO_SWAPW | AMO_ADDW |
         AMO_ANDW | AMO_ORW |
         AMO_XORW | AMO_MAXW |
         AMO_MAXWU | AMO_MINW |
         AMO_MINWU => 2.U(2.W)
    case LH | LHU | SH | FLH | FSH => 1.U(2.W)
    case LB | LBU | SB | FLB | FSB => 0.U(2.W)
    case _ => 3.U(2.W)
  }
}