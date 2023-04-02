// In the example below, the HazardDetection component checks for data hazards based on register 
// dependencies and load/store operations. The ForwardingUnit component is a simple multiplexer that 
// selects between two input values based on a control signal. The ReservationStations component is 
// a placeholder for your specific implementation, which should handle instruction scheduling and other related tasks.


// Here's a summary of the SimplifiedOutOfOrderExecutionPipeline:

//    A PipelineConfiguration object is introduced to centralize the configuration of parameters such as the number 
// of pipeline stages, the sizes of the Reorder Buffer and Reservation Stations, etc. This makes it easier to experiment 
// with different pipeline configurations.

//    The HazardDetection component is responsible for identifying data hazards. It detects dependencies between instructions 
// and signals when a hazard is present.

//    The ForwardingUnit component is a simple multiplexer that selects between multiple input values based on a control 
// signal. This component is reusable and can be shared among different pipeline stages or pipelines.

//    The ReservationStations component handles instruction scheduling and other related tasks. This component may vary 
// depending on your specific pipeline requirements, but the general idea is to provide a reusable and modular component 
// for managing Reservation Stations.

//    The pipeline leverages spinal.lib.pipeline._ for handling stalling, flushing, and managing stage transitions. This s
// implifies the pipeline control logic and reduces the chances of introducing bugs.














package pipelinecomponents

import spinal.core._
import spinal.lib._

class HazardDetection extends Component {
  val io = new Bundle {
    val rs1 = in UInt(5 bits)
    val rs2 = in UInt(5 bits)
    val rd = in UInt(5 bits)
    val isLoad = in Bool
    val isStore = in Bool
    val hazard = out Bool
  }

  // DependencyResolver functionality
  val rs1Dependent = io.rd =/= 0 && io.rd === io.rs1
  val rs2Dependent = io.rd =/= 0 && io.rd === io.rs2

  io.hazard := (io.isLoad && (rs1Dependent || rs2Dependent)) || (io.isStore && rs1Dependent)
}

class ForwardingUnit extends Component {
  val io = new Bundle {
    val in1 = in Bits(32 bits)
    val in2 = in Bits(32 bits)
    val select = in UInt(2 bits)
    val out = out Bits(32 bits)
  }

  io.out := MuxOH(io.select, List(io.in1, io.in2))
}










// Here's a simple example of a ReservationStations component. This example assumes a generic reservation 
// station entry with an opcode, two source operands, and a destination operand. 
// This example ReservationStations component has a memory to store reservation station entries and handles 
// writing new entries from the pipeline stages. It finds the first free entry in the reservation station and 
// writes the new entry there. The component reads the reservation station entries and outputs them.



import spinal.core._
import spinal.lib._

case class ReservationStationEntry() extends Bundle {
  val valid = Bool
  val opcode = UInt(8 bits)
  val src1 = UInt(32 bits)
  val src2 = UInt(32 bits)
  val dst = UInt(32 bits)
}

class ReservationStations(config: PipelineConfiguration) extends Component {
  val io = new Bundle {
    val fetchOut = in(ReservationStationEntry())
    val decodeOut = in(ReservationStationEntry())
    val executeOut = in(ReservationStationEntry())
    val writebackOut = in(ReservationStationEntry())
    val entries = out Vec(ReservationStationEntry(), config.reservationStationSize)
  }

  val reservationStation = Mem(ReservationStationEntry(), config.reservationStationSize)

  // Write new entries from the stages to the reservation station
  when(io.fetchOut.valid) {
    reservationStation.write(
      address = findFirstFreeEntry(),
      data = io.fetchOut,
      enable = True
    )
  }

  when(io.decodeOut.valid) {
    reservationStation.write(
      address = findFirstFreeEntry(),
      data = io.decodeOut,
      enable = True
    )
  }

  when(io.executeOut.valid) {
    reservationStation.write(
      address = findFirstFreeEntry(),
      data = io.executeOut,
      enable = True
    )
  }

  when(io.writebackOut.valid) {
    reservationStation.write(
      address = findFirstFreeEntry(),
      data = io.writebackOut,
      enable = True
    )
  }

  // Read entries from the reservation station and output them
  io.entries := reservationStation.readSync(io.entries.indices)

  // Find the first free entry in the reservation station
  def findFirstFreeEntry(): UInt = {
    val freeEntry = UInt(log2Up(config.reservationStationSize) bits)
    freeEntry := 0
    for (i <- 0 until config.reservationStationSize) {
      when(!reservationStation(i).valid) {
        freeEntry := i
        break()
      }
    }
    freeEntry
  }
}












package pipelinecontroller

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

























import spinal.core._
import spinal.lib._

class SimplifiedOutOfOrderExecutionPipeline(config: PipelineConfiguration) extends Component {
  val io = new Bundle {
    val flush = in Bool
    val stall = in Vec(Bool, config.stageCount)
  }

  // Instantiate the pipeline stages and PipelineController
  val stages = Vec(PipelineStage(), config.stageCount)
  val pipelineController = new PipelineController(config.stageCount)

  // Connect the stages in the pipeline
  for (i <- 0 until config.stageCount - 1) {
    stages(i + 1).io.input << stages(i).io.output
  }

  // Connect the pipeline controller to the pipeline stages
  for (i <- 0 until config.stageCount) {
    stages(i).stageAllowInput := pipelineController.io.allowInput(i)
    stages(i).stageAllowOutput := pipelineController.io.allowOutput(i)
  }

  // Set the stall and flush signals
  pipelineController.io.flush := io.flush
  pipelineController.io.stall := io.stall

  val fetch = new FetchStage(config)
  val decode = new DecodeStage(config)
  val execute = new ExecuteStage(config)
  val writeback = new WritebackStage(config)

  val hazardDetection = new HazardDetection(config)
  val forwardingUnit = new ForwardingUnit(config)
  val reservationStations = new ReservationStations(config)

  // Connect pipeline stages
  decode.io.input << fetch.io.output
  execute.io.input << decode.io.output
  writeback.io.input << execute.io.output

  // Connect hazard detection to the stages
  hazardDetection.io.fetchOut := fetch.io.output
  hazardDetection.io.decodeOut := decode.io.output
  hazardDetection.io.executeOut := execute.io.output
  hazardDetection.io.writebackOut := writeback.io.output

  // Connect forwarding unit to the stages
  forwardingUnit.io.decodeOut := decode.io.output
  forwardingUnit.io.executeOut := execute.io.output
  forwardingUnit.io.writebackOut := writeback.io.output

  // Connect reservation stations to the stages
  reservationStations.io.fetchOut := fetch.io.output
  reservationStations.io.decodeOut := decode.io.output
  reservationStations.io.executeOut := execute.io.output
  reservationStations.io.writebackOut := writeback.io.output

  // Handle stalling and flushing based on hazard detection
  fetch.io.flush := hazardDetection.io.flush
  fetch.io.stall := hazardDetection.io.stall
  decode.io.flush := hazardDetection.io.flush
  decode.io.stall := hazardDetection.io.stall
  execute.io.flush := hazardDetection.io.flush
  execute.io.stall := hazardDetection.io.stall
  writeback.io.flush := hazardDetection.io.flush
}
