import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._


class PipelineController(stageCount: Int) extends Component {
  val io = new Bundle {
    val flush = in Bool
    val stall = in Vec(Bool, stageCount)
    val allowInput = out Vec(Bool, stageCount)
    val allowOutput = out Vec(Bool, stageCount)
  }

  val stageInputAllowReg = Reg(Vec(Bool, stageCount))
  val stageOutputAllowReg = Reg(Vec(Bool, stageCount))

  // Initialize allow input/output registers
  stageInputAllowReg.init(True)
  stageOutputAllowReg.init(True)

  // Update the allow input/output registers
  for (i <- 0 until stageCount) {
    stageInputAllowReg(i) := !io.stall(i) && stageOutputAllowReg(i) && !io.flush
    stageOutputAllowReg(i) := !io.stall(i) && stageInputAllowReg(i) && !io.flush
  }

  // Assign the output signals
  io.allowInput := stageInputAllowReg
  io.allowOutput := stageOutputAllowReg
}


class RV64IMA_Core extends Component {
  val io = new Bundle {
    val imem = master Flow (Bits(64 bits))
    val dmem = master Stream (MemCmd(64))
  }

  // IF (Instruction Fetch) stage
  class IF_Stage extends Stage {
    val pc = Reg(UInt(64 bits)) init 0
    val pc_next = pc + 4
    pc := pc_next
  }

 class ID_Stage extends Stage {
  val instr = io.imem.payload
  val opcode = instr(6 downto 0)
  val funct3 = instr(14 downto 12)
  val funct7 = instr(31 downto 25)

  val rd = instr(11 downto 7)
  val rs1 = instr(19 downto 15)
  val rs2 = instr(24 downto 20)

  val immI = instr(31 downto 20).asSInt.resize(64)  // I-Type
  val immS = (instr(31 downto 25) @@ instr(11 downto 7)).asSInt.resize(64)  // S-Type
  val immB = (instr(31) @@ instr(7) @@ instr(30 downto 25) @@ instr(11 downto 8) @@ U"b0").asSInt.resize(64)  // B-Type
  val immU = (instr(31 downto 12) @@ U"b000000000000").asSInt.resize(64)  // U-Type
  val immJ = (instr(31) @@ instr(19 downto 12) @@ instr(20) @@ instr(30 downto 21) @@ U"b0").asSInt.resize(64)  // J-Type

  val imm = MuxLookup(opcode, S(0, 64 bits), Array(
    OpCode.LOAD -> immI,
    OpCode.OP_IMM -> immI,
    OpCode.STORE -> immS,
    OpCode.OP_IMM_32 -> immI,
    OpCode.BRANCH -> immB,
    OpCode.LUI -> immU,
    OpCode.AUIPC -> immU,
    OpCode.JAL -> immJ,
    OpCode.JALR -> immI
  ))
}


class EX_Stage extends Stage {
  
  val id = getPrev[ID_Stage]
  
  val regFile = Mem(UInt(64 bits), 32)
  val id = getPrev[ID_Stage]

  val src1 = regFile.readAsync(id.rs1)
  val src2 = regFile.readAsync(id.rs2)

  val pc = getPrev[IF_Stage].pc
  val imm = id.imm
  val op = id.op
  val funct3 = id.funct3
  val funct7 = id.funct7

  // ALU operations
  val aluResult = UInt(64 bits)
  val aluOp = ALUOperation()
  aluOp.assignFromBits(funct7 @@ funct3)

  switch(aluOp) {
    // ... (handle ALU operations, e.g., ADD, SUB, XOR, etc.) ...
  }

  // Branch operations
  val branchTaken = Bool()
  val branchTarget = pc + imm

  switch(funct3) {
    // ... (handle branch operations, e.g., BEQ, BNE, etc.) ...
  }

  // Load/store operations
  val memAccess = Bool()
  val memWrite = Bool()
  val memData = UInt(64 bits)

  switch(op) {
    // ... (handle load/store operations, e.g., LB, LH, LW, LD, SB, SH, SW, SD) ...
  }

  // Jump operations
  val jumpTaken = Bool()
  val jumpTarget = pc + imm

  switch(op) {
    // ... (handle jump operations, e.g., JAL, JALR) ...
  }

  // Other operations (e.g., LUI, AUIPC)
  switch(op) {
    // ... (handle other operations) ...
    
    
    // Branch control logic
  val branchSrc1 = id.rs1Data
  val branchSrc2 = id.rs2Data
  val branchCond = id.imm(0)
  val branchTaken = id.branch & MuxLookup(id.funct3, False, Array(
    Funct3.BEQ  -> (branchSrc1 === branchSrc2),
    Funct3.BNE  -> (branchSrc1 =/= branchSrc2),
    Funct3.BLT  -> (branchSrc1.asSInt < branchSrc2.asSInt),
    Funct3.BGE  -> (branchSrc1.asSInt >= branchSrc2.asSInt),
    Funct3.BLTU -> (branchSrc1 < branchSrc2),
    Funct3.BGEU -> (branchSrc1 >= branchSrc2)
  ))

  // Jump control logic
  val jump = id.jump
  val jumpTaken = jump & ~id.jumpAndLink // JALR is handled in WB stage
  val jumpAddr = id.rs1Data.asUInt + id.imm.asUInt

    
  }

  // Final output signals
  val aluOutput = aluResult
  val branchOutput = (branchTaken ? branchTarget | pc) + 4
  val memOutput = memAccess ? memData | aluResult
  val jumpOutput = jumpTaken ? jumpTarget | pc

  val outputData = UInt(64 bits)
  outputData := (op === OpCode.BRANCH) ? branchOutput |
                (op === OpCode.LOAD || op === OpCode.STORE) ? memOutput |
                (op === OpCode.JAL || op === OpCode.JALR) ? jumpOutput |
                aluOutput
}


// MEM (Memory Access) stage
class MEM_Stage extends Stage {
  val ex = getPrev[EX_Stage]

  // Memory access signals from the EX stage
  val memAccess = ex.memAccess
  val memWrite = ex.memWrite
  val memData = ex.memData
  val memAddr = ex.outputData

  val dmemCmd = MemCmd(64)
  dmemCmd.address := memAddr
  dmemCmd.write := memWrite
  dmemCmd.data := memData
  io.dmem.payload := dmemCmd

  // Control signals for memory stage
  io.dmem.valid := memAccess
  input.ready := !memAccess || io.dmem.ready
}

// WB (Write Back) stage
class WB_Stage extends Stage {
  
  val mem = getPrev[MEM_Stage]
  val ex = mem.getPrev[EX_Stage]
  val id = ex.getPrev[ID_Stage]

  // Load/store data from MEM stage
  val loadData = io.dmem.payload.data

  // Control signals from EX stage
  val jumpTaken = ex.jumpTaken
  val jumpOutput = ex.jumpOutput
  val branchTaken = ex.branchTaken
  val branchOutput = ex.branchOutput

  // Control signals for WB stage
  output.valid := input.valid && !mem.memAccess || io.dmem.ready
  
  // Control signals from EX stage
  val jumpTaken = ex.jumpTaken
  val jumpOutput = ex.jumpAddr
  val jalTaken = id.jumpAndLink
  val jalOutput = id.pc + 4

  // Update the PC
  when(jumpTaken) {
    ex.pc := jumpOutput
  }.elsewhen(ex.branchTaken) {
    ex.pc := ex.branchAddr
  }.elsewhen(jalTaken) {
    ex.pc := id.rs1Data.asUInt + id.imm.asUInt
  }.otherwise {
    ex.pc := ex.pc + 4
  }
  
    // Write back to register file
  when(input.valid && !mem.memAccess) {
    ex.regFile(id.rd) := ex.aluOutput
  }.elsewhen(input.valid && mem.memAccess && !mem.memWrite) {
    ex.regFile(id.rd) := loadData
  }.elsewhen(input.valid && jalTaken) {
    ex.regFile(id.rd) := jalOutput
  }

}


  // Pipeline setup
  val if_stage = new IF_Stage
  val id_stage = new ID_Stage
  val ex_stage = new EX_Stage
  val mem_stage = new MEM_Stage
  val wb_stage = new WB_Stage

  // Connecting pipeline stages
  id_stage.input.valid := if_stage.output.valid
  ex_stage.input.valid := id_stage.output.valid
  mem_stage.input.valid := ex_stage.output.valid
  wb_stage.input.valid := mem_stage.output.valid
  
   // Pipeline setup
  val stageCount = 5
  val pipelineController = new PipelineController(stageCount)
  val stages = List(if_stage, id_stage, ex_stage, mem_stage, wb_stage)

  // Connecting pipeline stages and PipelineController
  for (i <- 0 until stageCount - 1) {
    stages(i + 1).input.valid := stages(i).output.valid && pipelineController.io.allowInput(i + 1)
    stages(i).output.ready := pipelineController.io.allowOutput(i)
  }

  // Set the pipeline controller's stall signals
  pipelineController.io.stall := Vec(False, False, False, False, False) // Modify this based on your stall conditions

  // Flush the pipeline if necessary
  pipelineController.io.flush := False // Set to True when a flush is needed (e.g., branch taken)
}


// Helper case class for memory commands
case class MemCmd(size: Int) extends Bundle {
  val address = UInt(size bits)
  val write = Bool()
  val data = UInt(size bits)
}

// Create the spinalHDL configuration and generate the RTL
object RV64IMA_Core_Gen extends App {
  val config = SpinalConfig()
  config.generateVerilog(new RV64IMA_Core)
}
