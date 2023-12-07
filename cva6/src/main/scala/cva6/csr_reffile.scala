package cva6

import chisel3._
import chisel3.experimental.FlatIO
import chisel3.experimental.conversions.tuple2hwtuple
import chisel3.util.{Fill, MuxCase, MuxLookup}
import cva6.include._
import cva6.include.ariane_pkg.{PITON_ARIANE, _}
import cva6.include.ariane_pkg.fu_op._
import cva6.include.ariane_pkg.fu_t.CTRL_FLOW
import cva6.include.dm.{CauseBreakpoint, CauseRequest, CauseSingleStep, ExceptionAddress}
import cva6.include.riscv_pkg.csr_reg_t._
import cva6.include.riscv_pkg.priv_lvl_t.{PRIV_LVL_M, PRIV_LVL_S, PRIV_LVL_U}
import cva6.include.riscv_pkg.xlen_t.XLEN_64
import cva6.include.riscv_pkg.{BREAKPOINT, DEBUG_REQUEST, ENV_CALL_MMODE, ENV_CALL_SMODE, ENV_CALL_UMODE, ILLEGAL_INSTR, INSTR_ADDR_MISALIGNED, INSTR_PAGE_FAULT, IRQ_M_EXT, IRQ_M_SOFT, IRQ_M_TIMER, IRQ_S_EXT, LOAD_PAGE_FAULT, MIP_MSIP, MIP_MTIP, MIP_SEIP, MIP_SSIP, MIP_STIP, STORE_PAGE_FAULT, csr_addr_t, csr_reg_t, csr_t, dcsr_t, fcsr_t, priv_lvl_t, satp_t, status_rv64_t, xs_t}
import freechips.rocketchip.util.UIntIsOneOf
import org.chipsalliance.cde.config.Parameters
import roma.utils.RomaUtils.{UIntInside, UtilsForSeq}

import scala.collection.mutable.LinkedHashMap

abstract class ArianParamBundle(implicit p: Parameters) extends Bundle


class csrIntf(implicit p: Parameters) extends ArianParamBundle {
  val AsidWidth = ASID_WIDTH
  val NrCommitPorts = NR_COMMIT_PORTS

  val time_irq_i = Input(Bool()) // Timer threw a interrupt
  // s} a flush request out if a CSR with a side effect has changed (e.g. written)
  val flush_o = Output(Bool())
  val halt_csr_o = Output(Bool()) // halt requested
  // commit acknowledge
  val commit_instr_i = Input(VecInit(Seq.fill(NrCommitPorts)(new scoreboard_entry_t))) // the instruction we want to commit
  val commit_ack_i = Input(VecInit(Seq.fill(NrCommitPorts)(Bool()))) // Commit acknowledged a instruction -> increase instret CSR
  // Core and Cluster ID
  val boot_addr_i = Input(UInt(64.W)) // Address from which to start booting, mtvec is set to the same address
  val hart_id_i = Input(UInt(64.W)) // Hart id in a multicore environment (reflected in a CSR)
  // we are taking an exception
  val ex_i = Input(new exception_t) // We've got an exception from the commit stage, take it

  val csr_op_i = Input(fu_op()) // Operation to perform on the CSR file
  val csr_addr_i = Input(UInt(12.W)) // Address of the register to read/write
  val csr_wdata_i = Input(UInt(64.W)) // Write data in
  val csr_rdata_o = Output(UInt(64.W)) // Read data out
  val dirty_fp_state_i = Input(Bool()) // Mark the FP sate as dirty
  val csr_write_fflags_i = Input(Bool()) // Write fflags register e.g.: we are retiring a floating point instruction
  val pc_i = Input(UInt(64.W)) // PC of instruction accessing the CSR
  val csr_exception_o = Output(new exception_t) // attempts to access a CSR without appropriate privilege
  // level or to write  a read-only register also
  // raises illegal instruction exceptions.
  // Interrupts/Exceptions
  val epc_o = Output(UInt(64.W)) // Output the exception PC to PC Gen, the correct CSR (mepc, sepc) is set accordingly
  val eret_o = Output(Bool()) // Return from exception, set the PC of epc_o
  val trap_vector_base_o = Output(UInt(64.W)) // Output base of exception vector, correct CSR is output (mtvec, stvec)
  val priv_lvl_o = priv_lvl_t() // Current privilege level the CPU is in
  // FPU
  val fs_o = Output(xs_t()) // Floating point extension status
  val fflags_o = Output(UInt(5.W)) // Floating-Point Accured Exceptions
  val frm_o = Output(UInt(3.W)) // Floating-Point Dynamic Rounding Mode
  val fprec_o = Output(UInt(7.W)) // Floating-Point Precision Control
  // Decoder
  val irq_ctrl_o = Output(new irq_ctrl_t) // interrupt management to id stage
  // MMU
  val en_translation_o = Output(Bool()) // enable VA translation
  val en_ld_st_translation_o = Output(Bool()) // enable VA translation for load and stores
  val ld_st_priv_lvl_o = priv_lvl_t() // Privilege level at which load and stores should happen
  val sum_o = Output(Bool())
  val mxr_o = Output(Bool())
  val satp_ppn_o = Output(UInt(44.W))
  val asid_o = Output(UInt(AsidWidth.W))
  // external interrupts
  val irq_i = Input(UInt(2.W)) // external interrupt in
  val ipi_i = Input(Bool()) // inter processor interrupt -> connected to machine mode sw
  val debug_req_i = Input(Bool()) // debug request in
  val set_debug_pc_o = Output(Bool())
  // Virtualization Support
  val tvm_o = Output(Bool()) // trap virtual memory
  val tw_o = Output(Bool()) // timeout wait
  val tsr_o = Output(Bool()) // trap sret
  val debug_mode_o = Output(Bool()) // we are in debug mode -> that will change some decoding
  val single_step_o = Output(Bool()) // we are in single-step mode
  // Caches
  val icache_en_o = Output(Bool()) // L1 ICache Enable
  val dcache_en_o = Output(Bool()) // L1 DCache Enable
  // Performance Counter
  val perf_addr_o = Output(UInt(5.W)) // read/write address to performance counter module (up to 29 aux counters possible in riscv encoding.h)
  val perf_data_o = Output(UInt(64.W)) // write data to performance counter module
  val perf_data_i = Input(UInt(64.W)) // read data from performance counter module
  val perf_we_o = Output(Bool())
}


class csr_regfile()(implicit p: Parameters) extends Module {
  val DmBaseAddress = p(ArianeParamsKey(0)).DmBaseAddress
  val AsidWidth = ASID_WIDTH
  val NrCommitPorts = NR_COMMIT_PORTS
  val io = IO(new csrIntf)

  // Define a generic method to create a register and a wire of the same type
  def regWirePair[T <: Data](gen: T): (T, T) = {
    val d = Wire(gen)
    val q = RegNext(d, 0.U.asTypeOf(gen))
    d := q
    q -> d
  }

  val csr_addr = io.csr_addr_i.asTypeOf(new csr_addr_t)
  val csr_rdata = Wire(UInt(64.W))

  val mstatus_q -> mstatus_d = regWirePair(new status_rv64_t)
  val satp_q -> satp_d = regWirePair(new satp_t)
  val dcsr_q -> dcsr_d = regWirePair(new dcsr_t)

  val priv_lvl_d -> priv_lvl_q = regWirePair(priv_lvl_t())
  val debug_mode_q -> debug_mode_d = regWirePair(Bool())
  val mtvec_rst_load_q = RegNext(reset.asBool)

  val dpc_q -> dpc_d = regWirePair(UInt(64.W))
  val dscratch0_q -> dscratch0_d = regWirePair(UInt(64.W))
  val dscratch1_q -> dscratch1_d = regWirePair(UInt(64.W))
  val mtvec_q -> mtvec_d = regWirePair(UInt(64.W))
  val medeleg_q -> medeleg_d = regWirePair(UInt(64.W))
  val mideleg_q -> mideleg_d = regWirePair(UInt(64.W))
  val mip_q -> mip_d = regWirePair(UInt(64.W))
  val mie_q -> mie_d = regWirePair(UInt(64.W))
  val mscratch_q -> mscratch_d = regWirePair(UInt(64.W))
  val mepc_q -> mepc_d = regWirePair(UInt(64.W))
  val mcause_q -> mcause_d = regWirePair(UInt(64.W))
  val mtval_q -> mtval_d = regWirePair(UInt(64.W))

  val stvec_q -> stvec_d = regWirePair(UInt(64.W))
  val sscratch_q -> sscratch_d = regWirePair(UInt(64.W))
  val sepc_q -> sepc_d = regWirePair(UInt(64.W))
  val scause_q -> scause_d = regWirePair(UInt(64.W))
  val stval_q -> stval_d = regWirePair(UInt(64.W))
  val dcache_q -> dcache_d = regWirePair(UInt(64.W))
  val icache_q -> icache_d = regWirePair(UInt(64.W))

  val wfi_d -> wfi_q = regWirePair(Bool())

  val cycle_q -> cycle_d   = regWirePair(UInt(64.W))
  val instret_q -> instret_d = regWirePair(UInt(64.W))

  val fcsr_q -> fcsr_d = regWirePair(new fcsr_t)


  // ---------------------------
  // CSR OP Select Logic
  // ---------------------------
  val privilege_violation = WireInit(false.B)
  val csrOpWdata = Seq(
    CSR_WRITE -> io.csr_wdata_i,
    CSR_SET -> (io.csr_wdata_i | csr_rdata),
    CSR_CLEAR -> ((~io.csr_wdata_i).asUInt & csr_rdata))
  val csr_wdata = MuxLookup(io.csr_op_i, io.csr_wdata_i)(csrOpWdata)
  val csr_we = Mux(privilege_violation, false.B, MuxLookup(io.csr_op_i, false.B)(Seq(csrOpWdata.map { case (k, v) => (k, true.B) }, Seq((CSR_READ) -> true.B)).flatten))
  val csr_read = Mux(privilege_violation, false.B, MuxLookup(io.csr_op_i, false.B)(csrOpWdata.map { case (k, v) => (k, true.B) }))

  // register for enabling load store address translation, this is critical, hence the register
  val en_ld_st_translation_q -> en_ld_st_translation_d = regWirePair(Bool())

  val mret = io.csr_op_i === MRET
  val sret = io.csr_op_i === SRET
  val dret = io.csr_op_i === DRET

  // determine if mprv needs to be considered if in debug mode
  val mprv = Mux(debug_mode_q && !dcsr_q.mprven, false.B, mstatus_q.mprv)
  io.debug_mode_o := debug_mode_q
  io.single_step_o := dcsr_q.step

  // ----------------
  // CSR Read logic
  // ----------------
  val debug_csrs = Seq[(csr_reg_t.Type, UInt)](
    CSR_DPC -> dpc_q,
    CSR_DSCRATCH0 -> dscratch0_q,
    CSR_DSCRATCH1 -> dscratch1_q,
  )
  val fp_csrs = if(FP_PRESENT) Seq[(csr_reg_t.Type, UInt)](
    CSR_FFLAGS -> Fill(59, false.B) ## fcsr_q.fflags,
    CSR_FRM -> Fill(61, false.B) ## fcsr_q.frm,
    CSR_FCSR -> Fill(56, false.B) ## fcsr_q.frm ## fcsr_q.fflags,
    CSR_FTRAN -> Fill(57, false.B) ## fcsr_q.fprec, // non-standard extension
  ) else Nil

  val xcpt_csrs = Seq(
    CSR_FFLAGS -> (mstatus_q.fs === xs_t.Off),
    CSR_FRM -> (mstatus_q.fs === xs_t.Off),
    CSR_FCSR -> (mstatus_q.fs === xs_t.Off),
    CSR_FTRAN -> (mstatus_q.fs === xs_t.Off),
    CSR_SAPT -> (io.priv_lvl_o === PRIV_LVL_S && mstatus_q.tvm),
  )

  val base_csrs = Seq[(csr_reg_t.Type, UInt)](
    CSR_DCSR -> Fill(32, false.B) ## dcsr_q.asUInt,
    CSR_TSELECT -> 0.U, // not implemented
    CSR_TDATA1 -> 0.U, // not implemented
    CSR_TDATA2 -> 0.U, // not implemented
    CSR_TDATA3 -> 0.U, // not implemented
    CSR_SSTATUS -> (mstatus_q.asUInt & SMODE_STATUS_READ_MASK.asUInt),
    CSR_SIE -> (mie_q & mideleg_q),
    CSR_SIP -> (mip_q & mideleg_q),
    CSR_STVEC -> stvec_q,
    CSR_SCOUNTEREN -> 0.U, // not implemented
    CSR_SSCRATCH -> sscratch_q,
    CSR_SEPC -> sepc_q,
    CSR_SCAUSE -> scause_q,
    CSR_STVAL -> stval_q,
    CSR_SAPT -> Mux(io.priv_lvl_o === PRIV_LVL_S && mstatus_q.tvm, 0.U, satp_q.asUInt), // intercept reads to SATP if in S-Mode and TVM is enabled
    // machine mode registers
    CSR_MSTATUS -> mstatus_q.asUInt,
    CSR_MISA -> ISA_CODE,
    CSR_MEDELEG -> medeleg_q,
    CSR_MIDELEG -> mideleg_q,
    CSR_MIE -> mie_q,
    CSR_MTVEC -> mtvec_q,
    CSR_MCOUNTEREN -> 0.U, // not implemented
    CSR_MSCRATCH -> mscratch_q,
    CSR_MEPC -> mepc_q,
    CSR_MCAUSE -> mcause_q,
    CSR_MTVAL -> mtval_q,
    CSR_MIP -> mip_q,
    CSR_MVENDORID -> 0.U, // not implemented
    CSR_MARCHID -> ARIANE_MARCHID,
    CSR_MIMPID -> 0.U, // not implemented
    CSR_MHARTID -> io.hart_id_i,
    CSR_MCYCLE -> cycle_q,
    CSR_MINSTRET -> instret_q,
    // Counters and Timers
    CSR_ML1_ICACHE_MISS -> io.perf_data_i,
    CSR_ML1_DCACHE_MISS -> io.perf_data_i,
    CSR_MITLB_MISS -> io.perf_data_i,
    CSR_MDTLB_MISS -> io.perf_data_i,
    CSR_MLOAD -> io.perf_data_i,
    CSR_MSTORE -> io.perf_data_i,
    CSR_MEXCEPTION -> io.perf_data_i,
    CSR_MEXCEPTION_RET -> io.perf_data_i,
    CSR_MBRANCH_JUMP -> io.perf_data_i,
    CSR_MCALL -> io.perf_data_i,
    CSR_MRET -> io.perf_data_i,
    CSR_MMIS_PREDICT -> io.perf_data_i,
    CSR_MSB_FULL -> io.perf_data_i,
    CSR_MIF_EMPTY -> io.perf_data_i,
    CSR_MHPM_COUNTER_17 -> io.perf_data_i,
    CSR_MHPM_COUNTER_18 -> io.perf_data_i,
    CSR_MHPM_COUNTER_19 -> io.perf_data_i,
    CSR_MHPM_COUNTER_20 -> io.perf_data_i,
    CSR_MHPM_COUNTER_21 -> io.perf_data_i,
    CSR_MHPM_COUNTER_22 -> io.perf_data_i,
    CSR_MHPM_COUNTER_23 -> io.perf_data_i,
    CSR_MHPM_COUNTER_24 -> io.perf_data_i,
    CSR_MHPM_COUNTER_25 -> io.perf_data_i,
    CSR_MHPM_COUNTER_26 -> io.perf_data_i,
    CSR_MHPM_COUNTER_27 -> io.perf_data_i,
    CSR_MHPM_COUNTER_28 -> io.perf_data_i,
    CSR_MHPM_COUNTER_29 -> io.perf_data_i,
    CSR_MHPM_COUNTER_30 -> io.perf_data_i,
    CSR_MHPM_COUNTER_31 -> io.perf_data_i,
    // custom (non RISC-V) cache control
    CSR_DCACHE -> dcache_q,
    CSR_ICACHE -> icache_q,
  )
  val csrs_mapping = base_csrs ++ fp_csrs ++ debug_csrs
  val read_access_exception, update_access_exception = WireInit(false.B)
  val dirty_fp_state_csr = WireInit(false.B)

  io.perf_addr_o := csr_addr.address

  csr_rdata := 0.U
  when(csr_read) {
    csr_rdata := MuxLookup(csr_addr.address.asTypeOf(csr_reg_t()), 0.U)(csrs_mapping)
    read_access_exception := MuxLookup(csr_addr.address.asTypeOf(csr_reg_t()), true.B)(Seq(
      xcpt_csrs, base_csrs.map{case (k,v) => k -> false.B}, debug_csrs.map{case (k,v) => k -> false.B},
    ).flatten)
  }


  // ---------------------------
  // CSR Write and update logic
  // ---------------------------
  csrs_mapping.foreach{case(k,v) =>
    when(csr_we && k === csr_addr.address) {
      k match {
        case CSR_MCYCLE => csr_wdata
        case CSR_FFLAGS => {
          when(mstatus_q.fs === xs_t.Off) {
            update_access_exception := true.B
          }.otherwise {
            dirty_fp_state_csr := true.B
            fcsr_d.fflags := csr_wdata(4, 0)
            io.flush_o := true.B // this instruction has side-effect
          }
        }
        case CSR_FRM => {
          when(mstatus_q.fs === xs_t.Off) {
            update_access_exception := true.B
          }.otherwise {
            dirty_fp_state_csr := true.B
            fcsr_d.frm := csr_wdata(7, 5)
            fcsr_d.fflags := csr_wdata(4, 0)
            io.flush_o := true.B // this instruction has side-effect
          }
        }
        case CSR_FTRAN => {
          when(mstatus_q.fs === xs_t.Off) {
            update_access_exception := true.B
          }.otherwise {
            dirty_fp_state_csr := true.B
            fcsr_d.fprec := csr_wdata(6, 0)
            io.flush_o := true.B // this instruction has side-effect
          }
        }
        case CSR_DCSR => {
          dcsr_d := csr_wdata
          // debug is implemented
          dcsr_d.xdebugver := 4.U
          // privilege level
          dcsr_d.prv := priv_lvl_q
          // currently not supported
          dcsr_d.nmip := false.B
          dcsr_d.stopcount := false.B
          dcsr_d.stoptime := false.B
        }
        case CSR_DPC => dpc_d := csr_wdata
        case CSR_DSCRATCH0 => dscratch0_d := csr_wdata
        case CSR_DSCRATCH1 => dscratch1_d := csr_wdata
        case CSR_SSTATUS => {
          val mask = SMODE_STATUS_WRITE_MASK
          mstatus_d := (mstatus_q.asUInt & (~mask).asUInt) | (csr_wdata & mask)
          if (!FP_PRESENT) mstatus_d.fs := xs_t.Off
          io.flush_o := true.B
        }
        case CSR_SIE => mie_d := (mie_q & (~mideleg_q).asUInt) | (csr_wdata & mideleg_q)
        case CSR_SIP => {
          val mask = MIP_SSIP & mideleg_q
          mip_d := (mip_q & (~mask).asUInt) | (csr_wdata& mask)
        }
        case CSR_STVEC =>    stvec_d     := csr_wdata(63,2) ## 0.U(1.W) ## csr_wdata(0)
        case CSR_SSCRATCH => sscratch_d  := csr_wdata
        case CSR_SEPC =>     sepc_d      := csr_wdata(63,1) ## 0.U(1.W)
        case CSR_SCAUSE =>   scause_d    := csr_wdata
        case CSR_STVAL =>    stval_d     := csr_wdata
        // supervisor address translation and protection
        case CSR_SAPT => {
          // intercept SATP writes if in S-Mode and TVM is enabled
          when(io.priv_lvl_o === PRIV_LVL_S && mstatus_q.tvm) {
            update_access_exception := true.B
          }.otherwise {
            val satp = WireInit(csr_wdata.asTypeOf(new satp_t))
            // only make ASID_LEN - 1 bit stick, that way software can figure out how many ASID bits are supported
            satp.asid := satp.asid & (Fill(16 - AsidWidth, false.B) ## Fill(AsidWidth, true.B))
            // only update if we actually support this mode
            satp_d := Mux(satp.mode === MODE_OFF || satp.mode === MODE_SV39, satp, satp_q)
          }
          // changing the mode can have side-effects on address translation (e.g.: other instructions), re-fetch
          // the next instruction by executing a flush
          io.flush_o := true.B
        }
        case CSR_MSTATUS => {
          mstatus_d := csr_wdata
          mstatus_d.xs := xs_t.Off
          if (!FP_PRESENT) {
            mstatus_d.fs := xs_t.Off
          }
          mstatus_d.upie := false.B
          mstatus_d.uie := false.B
          // this register has side-effects on other registers, flush the pipeline
          io.flush_o := true.B
        }
        // MISA is WARL (Write Any Value, Reads Legal Value)
        case CSR_MISA =>
        // machine exception delegation register
        // 0 - 15 exceptions supported
        case CSR_MEDELEG => {
          val mask = (1.U << INSTR_ADDR_MISALIGNED).asUInt |
                     (1.U << BREAKPOINT).asUInt |
                     (1.U << ENV_CALL_UMODE).asUInt |
                     (1.U << INSTR_PAGE_FAULT).asUInt |
                     (1.U << LOAD_PAGE_FAULT).asUInt |
                     (1.U << STORE_PAGE_FAULT).asUInt
          medeleg_d := (medeleg_q & (~mask).asUInt) | (csr_wdata & mask)
        }
        // machine interrupt delegation register
        // we do not support user interrupt delegation
        case CSR_MIDELEG => {
          val mask = MIP_SSIP | MIP_STIP | MIP_SEIP
          mideleg_d := (mideleg_q & (~mask).asUInt) | (csr_wdata & mask)
        }
        // mask the register so that unsupported interrupts can never be set
        case CSR_MIE => {
          val mask = MIP_SSIP | MIP_STIP | MIP_SEIP | MIP_MSIP | MIP_MTIP
          mie_d := (mie_q & (~mask).asUInt) | (csr_wdata & mask) // we only support supervisor and M-mode interrupts
        }
        case CSR_MTVEC => {
          // we are in vector mode, this implementation requires the additional
          // alignment constraint of 64 * 4 bytes
          mtvec_d := Mux(csr_wdata(0), csr_wdata(63, 8) ## 0.U(7.W), csr_wdata(63, 2) ## 0.U(1.W)) ## csr_wdata(0)
        }
        case CSR_MCOUNTEREN =>
        case CSR_MSCRATCH => mscratch_d := csr_wdata
        case CSR_MEPC =>     mepc_d     := csr_wdata(63,1) ## 0.U(1.W)
        case CSR_MCAUSE =>   mcause_d   := csr_wdata
        case CSR_MTVAL =>    mtval_d    := csr_wdata
        case CSR_MIP => {
          val mask = MIP_SSIP | MIP_STIP | MIP_SEIP
          mip_d := (mip_q & (~mask).asUInt) | (csr_wdata & mask)
        }
        // performance counters
        case CSR_MCYCLE =>   cycle_d := csr_wdata
        case CSR_MINSTRET =>
        case CSR_ML1_ICACHE_MISS |
             CSR_ML1_DCACHE_MISS |
             CSR_MITLB_MISS |
             CSR_MDTLB_MISS |
             CSR_MLOAD |
             CSR_MSTORE |
             CSR_MEXCEPTION |
             CSR_MEXCEPTION_RET |
             CSR_MBRANCH_JUMP |
             CSR_MCALL |
             CSR_MRET |
             CSR_MMIS_PREDICT |
             CSR_MSB_FULL |
             CSR_MIF_EMPTY |
             CSR_MHPM_COUNTER_17 |
             CSR_MHPM_COUNTER_18 |
             CSR_MHPM_COUNTER_19 |
             CSR_MHPM_COUNTER_20 |
             CSR_MHPM_COUNTER_21 |
             CSR_MHPM_COUNTER_22 |
             CSR_MHPM_COUNTER_23 |
             CSR_MHPM_COUNTER_24 |
             CSR_MHPM_COUNTER_25 |
             CSR_MHPM_COUNTER_26 |
             CSR_MHPM_COUNTER_27 |
             CSR_MHPM_COUNTER_28 |
             CSR_MHPM_COUNTER_29 |
             CSR_MHPM_COUNTER_30 |
             CSR_MHPM_COUNTER_31 => {
          io.perf_data_o := csr_wdata
          io.perf_we_o := true.B
        }
        case CSR_DCACHE => dcache_d := csr_wdata(0) // enable bit
        case CSR_ICACHE => icache_d := csr_wdata(0) // enable bit
        case _ => update_access_exception := true.B
      }
    }
  }
  mstatus_d.sxl := XLEN_64
  mstatus_d.uxl := XLEN_64

  // mark the floating point extension register as dirty
  when (FP_PRESENT.asBool && (dirty_fp_state_csr || io.dirty_fp_state_i)) {
    mstatus_d.fs := xs_t.Dirty
  }
  // hardwired extension registers
  mstatus_d.sd := (mstatus_q.xs === xs_t.Dirty) || (mstatus_q.fs === xs_t.Dirty)

  // write the floating point status register
  when(io.csr_write_fflags_i) {
    fcsr_d.fflags := io.csr_wdata_i(4, 0) | fcsr_q.fflags
  }

  // ---------------------
  // External Interrupts
  // ---------------------
  // Machine Mode External Interrupt P}ing
  mip_d(IRQ_M_EXT) := io.irq_i(0)
  // Machine software interrupt
  mip_d(IRQ_M_SOFT) := io.ipi_i
  // Timer interrupt p}ing, coming from platform timer
  mip_d(IRQ_M_TIMER) := io.time_irq_i

  // -----------------------
  // Manage Exception Stack
  // -----------------------
  // update exception CSRs
  // we got an exception update cause, pc and stval register
  val trap_to_priv_lvl = WireInit(PRIV_LVL_M)
  // Exception is taken and we are not in debug mode
  // exceptions in debug mode don't update any fields
  when(!debug_mode_q && io.ex_i.cause =/= DEBUG_REQUEST && io.ex_i.valid) {
    // do not flush, flush is reserved for CSR writes with side effects
    io.flush_o := false.B
    // figure out where to trap to
    // a m-mode trap might be delegated if we are taking it in S mode
    // first figure out if this was an exception or an interrupt e.g.: look at bit 63
    // the cause register can only be 6 bits long (as we only support 64 exceptions)
    when((io.ex_i.cause(63) && mideleg_q(io.ex_i.cause(5, 0))) ||
      (!io.ex_i.cause(63) && medeleg_q(io.ex_i.cause(5, 0)))) {
      // traps never transition from a more-privileged mode to a less privileged mode
      // so if we are already in M mode, stay there
      trap_to_priv_lvl := Mux(io.priv_lvl_o === PRIV_LVL_M, PRIV_LVL_M, PRIV_LVL_S)
    }

    // trap to supervisor mode
    when(trap_to_priv_lvl === PRIV_LVL_S) {
      // update sstatus
      mstatus_d.sie := false.B
      mstatus_d.spie := mstatus_q.sie
      // this can either be user or supervisor mode
      mstatus_d.spp := priv_lvl_q[0]
      // set cause
      scause_d := io.ex_i.cause
      // set epc
      sepc_d := io.pc_i
      // set mtval or stval
      stval_d := Mux(ZERO_TVAL.asBool &&
        (io.ex_i.cause.inside(
          ILLEGAL_INSTR,
          BREAKPOINT,
          ENV_CALL_UMODE,
          ENV_CALL_SMODE,
          ENV_CALL_MMODE
        ) || io.ex_i.cause[63]), false.B, io.ex_i.tval)
      // trap to machine mode
    }.otherwise {
      // update mstatus
      mstatus_d.mie := true.B
      'b0
      mstatus_d.mpie := mstatus_q.mie
      // save the previous privilege mode
      mstatus_d.mpp := priv_lvl_q
      mcause_d := io.ex_i.cause
      // set epc
      mepc_d := io.pc_i
      // set mtval or stval
      mtval_d := Mux(ZERO_TVAL.asBool &&
        (io.ex_i.cause.inside(
          ILLEGAL_INSTR,
          BREAKPOINT,
          ENV_CALL_UMODE,
          ENV_CALL_SMODE,
          ENV_CALL_MMODE
        ) || io.ex_i.cause[63]), false.B, io.ex_i.tval)
    }

    priv_lvl_d := trap_to_priv_lvl
  }

  // ------------------------------
  // Debug
  // ------------------------------
  // Explains why Debug Mode was entered.
  // When there are multiple reasons to enter Debug Mode in a single cycle, hardware should set cause to the cause with the highest priority.
  // 1: An ebreak instruction was executed. (priority 3)
  // 2: The Trigger Module caused a breakpoint exception. (priority 4)
  // 3: The debugger requested entry to Debug Mode. (priority 2)
  // 4: The hart single stepped because step was set. (priority 1)
  // we are currently not in debug mode and could potentially enter
  when(!debug_mode_q) {
      dcsr_d.prv := io.priv_lvl_o
      // trigger module fired

      // caused by a breakpoint
      when(io.ex_i.valid && io.ex_i.cause === BREAKPOINT) {
        // check that we actually want to enter debug depending on the privilege level we are currently in
        io.priv_lvl_o match {
          case PRIV_LVL_M => {
            debug_mode_d := dcsr_q.ebreakm
            io.set_debug_pc_o := dcsr_q.ebreakm
          }
          case PRIV_LVL_S => {
            debug_mode_d := dcsr_q.ebreaks
            io.set_debug_pc_o := dcsr_q.ebreaks
          }
          case PRIV_LVL_U => {
            debug_mode_d := dcsr_q.ebreaku
            io.set_debug_pc_o := dcsr_q.ebreaku
          }
        }
        // save PC of next this instruction e.g.: the next one to be executed
        dpc_d := io.pc_i
        dcsr_d.cause := CauseBreakpoint.asUInt
      }

      // we've got a debug request
      when (io.ex_i.valid && io.ex_i.cause === DEBUG_REQUEST) {
        // save the PC
        dpc_d := io.pc_i
        // enter debug mode
        debug_mode_d := true.B
        // jump to the base address
        io.set_debug_pc_o := true.B
        // save the cause as external debug request
        dcsr_d.cause := CauseRequest.asUInt
      }

      // single step enable and we just retired an instruction
      when (dcsr_q.step && io.commit_ack_i(0)) {
        // valid CTRL flow change
        when (io.commit_instr_i(0).fu === CTRL_FLOW) {
          // we saved the correct target address during execute
          dpc_d := io.commit_instr_i(0).bp.predict_address
          // exception valid
        }.elsewhen (io.ex_i.valid) {
          dpc_d := io.trap_vector_base_o
          // return from environment
        }.elsewhen (io.eret_o) {
          dpc_d := io.epc_o
          // consecutive PC
        }.otherwise {
          dpc_d := io.commit_instr_i(0).pc + Mux(io.commit_instr_i(0).is_compressed, 2.U, 4.U)
        }
        debug_mode_d := true.B
        io.set_debug_pc_o := true.B
        dcsr_d.cause := CauseSingleStep.asUInt
      }
  }
  // go in halt-state again when we encounter an exception
  when (debug_mode_q && io.ex_i.valid && io.ex_i.cause === BREAKPOINT) {
      io.set_debug_pc_o := true.B
  }

  // ------------------------------
  // MPRV - Modify Privilege Level
  // ------------------------------
  // Set the address translation at which the load and stores should occur
  // we can use the previous values since changing the address translation will always involve a pipeline flush
  when(mprv && satp_q.mode === MODE_SV39 && (mstatus_q.mpp =/= PRIV_LVL_M)) {
    en_ld_st_translation_d := true.B
  }.otherwise { // otherwise we go with the regular settings
    en_ld_st_translation_d := io.en_translation_o
  }

  io.ld_st_priv_lvl_o := Mux(mprv, mstatus_q.mpp, io.priv_lvl_o)
  io.en_ld_st_translation_o := en_ld_st_translation_q
  // ------------------------------
  // Return from Environment
  // ------------------------------
  // When executing an xRET instruction, supposing xPP holds the value y, xIE is set to xPIE the privilege
  // mode is changed to y xPIE is set to 1 and xPP is set to U
  when (mret) {
    // return from exception, IF doesn't care from where we are returning
    io.eret_o := true.B
    // return to the previous privilege level and restore all enable flags
    // get the previous machine interrupt enable flag
    mstatus_d.mie := mstatus_q.mpie
    // restore the previous privilege level
    priv_lvl_d := mstatus_q.mpp
    // set mpp to user mode
    mstatus_d.mpp := PRIV_LVL_U
    // set mpie to 1
    mstatus_d.mpie := true.B
  }

  when (sret) {
    // return from exception, IF doesn't care from where we are returning
    io.eret_o := true.B
    // return the previous supervisor interrupt enable flag
    mstatus_d.sie := mstatus_q.spie
    // restore the previous privilege level
    priv_lvl_d := mstatus_q.spp
    // set spp to user mode
    mstatus_d.spp := false.B
    // set spie to 1
    mstatus_d.spie := true.B
  }

  // return from debug mode
  when (dret) {
    // return from exception, IF doesn't care from where we are returning
    io.eret_o := true.B
    // restore the previous privilege level
    priv_lvl_d := dcsr_q.prv
    // actually return from debug mode
    debug_mode_d := false.B
  }

  io.irq_ctrl_o.mie := mie_q
  io.irq_ctrl_o.mip := mip_q
  io.irq_ctrl_o.sie := mstatus_q.sie
  io.irq_ctrl_o.mideleg := mideleg_q
  io.irq_ctrl_o.global_enable := (!debug_mode_q) && (!dcsr_q.step || dcsr_q.stepie) && // interrupts are enabled during single step or we are not stepping
                                 ((mstatus_q.mie && (io.priv_lvl_o === PRIV_LVL_M)) ||
                                 (io.priv_lvl_o =/= PRIV_LVL_M))


  // -----------------
  // Privilege Check
  // -----------------
  privilege_violation := false.B
  // if we are reading or writing, check for the correct privilege level this has
  // precedence over interrupts
  when (io.csr_op_i.isOneOf(CSR_WRITE, CSR_SET, CSR_CLEAR, CSR_READ)) {
    when((io.priv_lvl_o.asUInt & csr_addr.priv_lvl.asUInt) =/= csr_addr.priv_lvl.asUInt) {
      privilege_violation := true.B
    }

    // check access to debug mode only CSRs
    when (!debug_mode_q && (io.csr_addr_i(11, 4) === 0x7b)) {
      privilege_violation := true.B
    }
  }

  // ----------------------
  // CSR Exception Control
  // ----------------------
  io.csr_exception_o := 0.U(64.W) ## 0.U(64.W) ## 0.U(1.W) // 64'b0, 64'b0, 1'b0
  // ----------------------------------
  // Illegal Access (decode exception)
  // ----------------------------------
  // we got an exception in one of the processes above
  // throw an illegal instruction exception
  when (update_access_exception || read_access_exception) {
    io.csr_exception_o.cause := ILLEGAL_INSTR
    // we don't set the tval field as this will be set by the commit stage
    // this spares the extra wiring from commit to CSR and back to commit
    io.csr_exception_o.valid := true.B
  }

  when (privilege_violation) {
    io.csr_exception_o.cause := ILLEGAL_INSTR
    io.csr_exception_o.valid := true.B
  }

  // -------------------
  // Wait for Interrupt
  // -------------------
  // wait for interrupt register
  wfi_d := wfi_q
  // if there is any interrupt pending un-stall the core
  // also un-stall if we want to enter debug mode
  when (mip_q.orR || io.debug_req_i || io.irq_i(1)) {
    wfi_d := false.B
    // or alternatively if there is no exception pending and we are not in debug mode wait here
    // for the interrupt
  }.elsewhen(!debug_mode_q && io.csr_op_i === WFI && !io.ex_i.valid) {
    wfi_d := true.B
  }

  // output assignments dependent on privilege mode
  io.trap_vector_base_o := mtvec_q(63,2) ## 0.U(2.W)
  // output user mode stvec
  when (trap_to_priv_lvl === PRIV_LVL_S) {
    io.trap_vector_base_o := stvec_q(63, 2) ## 0.U(2.W)
  }

  // if we are in debug mode jump to a specific address
  when (debug_mode_q) {
    io.trap_vector_base_o := DmBaseAddress + ExceptionAddress.asUInt
  }

  // check if we are in vectored mode, if yes then do BASE + 4 * cause
  // we are imposing an additional alignment-constraint of 64 * 4 bytes since
  // we want to spare the costly addition
  when ((mtvec_q(0) || stvec_q(0)) && io.ex_i.cause(63)) {
    io.trap_vector_base_o(7,2) := io.ex_i.cause(5,0)
  }

  io.epc_o := mepc_q
  // we are returning from supervisor mode, so take the sepc register
  when (sret) {
    io.epc_o := sepc_q
  }
  // we are returning from debug mode, to take the dpc register
  when (dret) {
    io.epc_o := dpc_q
  }

  // -------------------
  // Output Assignments
  // -------------------
  // When the SEIP bit is read with a CSRRW, CSRRS, or CSRRC instruction, the value
  // returned in the rd destination register contains the logical-OR of the software-writable
  // bit and the interrupt signal from the interrupt controller.
  io.csr_rdata_o := MuxLookup(csr_rdata, csr_addr.address)(Seq(
    CSR_MIP.asUInt -> (csr_rdata | (io.irq_i(1) << IRQ_S_EXT).asUInt),
    // in supervisor mode we also need to check whether we delegated this bit
    CSR_SIP.asUInt -> (csr_rdata | ((io.irq_i(1) && mideleg_q(IRQ_S_EXT))<<IRQ_S_EXT).asUInt),
  ))

  // in debug mode we execute with privilege level M
  io.priv_lvl_o       := Mux(debug_mode_q, PRIV_LVL_M, priv_lvl_q)
  // FPU outputs
  io.fflags_o         := fcsr_q.fflags
  io.frm_o            := fcsr_q.frm
  io.fprec_o          := fcsr_q.fprec
  // MMU outputs
  io.satp_ppn_o       := satp_q.ppn
  io.asid_o           := satp_q.asid(AsidWidth-1,0)
  io.sum_o            := mstatus_q.sum
  // we support bare memory addressing and SV39
  io.en_translation_o := Mux(satp_q.mode === 8.U && io.priv_lvl_o =/= PRIV_LVL_M, true.B, false.B)
  io.mxr_o            := mstatus_q.mxr
  io.tvm_o            := mstatus_q.tvm
  io.tw_o             := mstatus_q.tw
  io.tsr_o            := mstatus_q.tsr
  io.halt_csr_o       := wfi_q

  io.icache_en_o      := { if(PITON_ARIANE) icache_q(0) else icache_q(0) && !debug_mode_q }
  io.dcache_en_o      := dcache_q(0)

  // determine if mprv needs to be considered if in debug mode
  mprv                := Mux(debug_mode_q && !dcsr_q.mprven, false.B, mstatus_q.mprv)
  io.debug_mode_o     := debug_mode_q
  io.single_step_o    := dcsr_q.step
}
