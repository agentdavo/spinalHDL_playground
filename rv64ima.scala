import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

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

  // ID (Instruction Decode) stage
  class ID_Stage extends Stage {
    val instr = io.imem.payload
    val op = instr(6 downto 0)
    val rd = instr(11 downto 7)
    val rs1 = instr(19 downto 15)
    val rs2 = instr(24 downto 20)
    val imm = Mux(op(6), instr(31 downto 20), instr(31 downto 12) @@ U"b0")
  }

  // EX (Execution) stage
  class EX_Stage extends Stage {
    val regFile = Mem(UInt(64 bits), 32)
    val src1 = regFile.readAsync(getPrev[ID_Stage].rs1)
    val src2 = regFile.readAsync(getPrev[ID_Stage].rs2)

    val result = UInt(64 bits)
    val op3 = getPrev[ID_Stage].op(3 downto 0)

    switch(op3) {
      is(B"0000") { result := src1 + src2 } // Add
      is(B"0001") { result := src1 - src2 } // Subtract
      // Other ALU operations (and, or, xor, sll, srl, etc.)
    }
  }

  // MEM (Memory Access) stage
  class MEM_Stage extends Stage {
    val memCmd = MemCmd(64)
    memCmd.address := getPrev[EX_Stage].result
    memCmd.write := False // Set to True for store instructions
    memCmd.data := getPrev[EX_Stage].src2
    io.dmem.payload := memCmd
  }

  // WB (Write Back) stage
  class WB_Stage extends Stage {
    when(io.dmem.fire) {
      getPrev[MEM_Stage].getPrev[EX_Stage].regFile(getPrev[MEM_Stage].getPrev[ID_Stage].rd) := io.dmem.payload.data
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
