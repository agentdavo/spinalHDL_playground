import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.crc._
import spinal.lib.enumeration._
import spinal.lib.fsm._
import spinal.lib.misc._

/**
  * =============================================================================
  * Final APB3 Register Mapping
  *
  * | Register Name           | Address (Hex) | Read/Write | Description                                         |
  * |-------------------------|---------------|------------|-----------------------------------------------------|
  * | **Position Data**       | `0x00`        | Read-only  | 32/36-bit position data                             |
  * | **Error Status**        | `0x04`        | Read-only  | Error flag                                          |
  * | **Warning Status**      | `0x08`        | Read-only  | Warning flag                                        |
  * | **CRC Error Status**    | `0x0C`        | Read-only  | CRC error flag                                      |
  * | **Reset Control**       | `0x10`        | Write-only | Resets the controller                               |
  * | **Start Request**       | `0x14`        | Write-only | Initiates data reception                            |
  * | **Speed Selection**     | `0x18`        | Write-only | Configures data reception speed                     |
  * | **Resolution Selection**| `0x1C`        | Write-only | Configures encoder resolution (18, 26, 32, 36-bit)  |
  * | **Interrupt Status**    | `0x20`        | Read-only  | Indicates data ready or error conditions            |
  *
  * =============================================================================
  *
  * **Register Descriptions:**
  * - **Position Data (`0x00`)**: Provides the current position from the encoder.
  *   - Supports both 32-bit and 36-bit resolutions based on the resolution configuration.
  *
  * - **Error Status (`0x04`)**: Indicates if an error has occurred during data reception.
  *
  * - **Warning Status (`0x08`)**: Signals warning conditions that may require attention.
  *
  * - **CRC Error Status (`0x0C`)**: Flags a CRC mismatch, ensuring data integrity.
  *
  * - **Reset Control (`0x10`)**: Writing to this register resets the controller to its initial state.
  *
  * - **Start Request (`0x14`)**: Initiates the data reception process when written to.
  *
  * - **Speed Selection (`0x18`)**: Allows configuration of the data reception speed.
  *   - **Values:**
  *     - `0`: 1 MHz
  *     - `1`: 2 MHz
  *     - `2`: 5 MHz
  *     - `3`: 10 MHz
  *
  * - **Resolution Selection (`0x1C`)**: Sets the encoder resolution.
  *   - **Values:**
  *     - `0`: 18-bit
  *     - `1`: 26-bit
  *     - `2`: 32-bit
  *     - `3`: 36-bit
  *
  * - **Interrupt Status (`0x20`)**: Reflects the interrupt conditions, such as data readiness or errors.
  *
  * **Usage Notes:**
  * - Ensure that the **Resolution Selection** is configured before initiating data reception.
  * - The **CRC Error Status** should be checked to verify data integrity after each reception.
  * - Writing to the **Reset Control** will clear all registers and reset the state machine.
  *
  * =============================================================================
  */

object BisscResolution extends SpinalEnum(binarySequential) {
  val BIT_18, BIT_26, BIT_32, BIT_36 = newElement()
}

object BisscResolutionHelper {
  def getResolutionBits(resolution: BisscResolution.type#Value): Int = resolution match {
    case BisscResolution.BIT_18 => 18
    case BisscResolution.BIT_26 => 26
    case BisscResolution.BIT_32 => 32
    case BisscResolution.BIT_36 => 36
  }

  def getResolution(resolutionCode: UInt): BisscResolution.type#Value = resolutionCode.toInt match {
    case 0 => BisscResolution.BIT_18
    case 1 => BisscResolution.BIT_26
    case 2 => BisscResolution.BIT_32
    case 3 => BisscResolution.BIT_36
    case _ => BisscResolution.BIT_18 // Default to 18-bit for invalid codes
  }
}

// Generic configuration for BiSS-C controller
case class BisscGenerics(resolutionBits: Int, maxClockFreq: HertzNumber, crcBits: Int = 6)

// Configuration for BiSS-C slave control
case class BisscSlaveCtrlMemoryMappedConfig(ctrlGenerics: BisscGenerics)

// Configuration for APB3 interface
object Apb3BisscSlaveCtrl {
  def getApb3Config = Apb3Config(
    addressWidth = 16,
    dataWidth = 32
  )
}

// BiSS-C Interface Definition (Master Role)
case class BissCInterface() extends Bundle with IMasterSlave {
  val ma  = Bool()  // Master Clock (output from master)
  val slo = Bool()  // Slave Output (input to master)

  override def asMaster(): Unit = {
    out(ma)
    in(slo)
  }
}

// BiSS-C Slave Controller with APB3 Integration
class Apb3BisscSlaveCtrl(generics: BisscSlaveCtrlMemoryMappedConfig) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3BisscSlaveCtrl.getApb3Config))
    val bissc = master(BissCInterface())
    val interrupt = out Bool()
  }

  // Instantiate BiSS-C Control Logic
  val bisscCtrl = new BisscSlaveCtrl(generics.ctrlGenerics)
  io.bissc <> bisscCtrl.io.bissc

  val busCtrl = Apb3SlaveFactory(io.apb)

  // Status Registers (Read-Only)
  busCtrl.read(bisscCtrl.io.position,      0x00, "Position data")
  busCtrl.read(bisscCtrl.io.error,         0x04, "Error status")
  busCtrl.read(bisscCtrl.io.warning,       0x08, "Warning status")
  busCtrl.read(bisscCtrl.io.crcError,      0x0C, "CRC Error status")

  // Control Registers (Write-Only)
  busCtrl.drive(bisscCtrl.io.reset,        0x10, "Reset control")
  busCtrl.drive(bisscCtrl.io.startRequest, 0x14, "Start request")

  // BiSS-C Speed Selection Register (Write-Only)
  // 0x18: Speed Selection (0: 1 MHz, 1: 2 MHz, 2: 5 MHz, 3: 10 MHz)
  busCtrl.drive(bisscCtrl.io.speedConfig,  0x18, "BiSS-C Speed Selection")

  // BiSS-C Resolution Selection Register (Write-Only)
  // 0x1C: Resolution Selection (0: 18-bit, 1: 26-bit, 2: 32-bit, 3: 36-bit)
  busCtrl.drive(bisscCtrl.io.resolutionConfig, 0x1C, "BiSS-C Resolution Selection")

  // Interrupt Logic
  val interruptSignal = bisscCtrl.io.dataReady || bisscCtrl.io.error || bisscCtrl.io.crcError
  io.interrupt := interruptSignal
  busCtrl.read(interruptSignal,              0x20, "Interrupt status")
}

// BiSS-C Slave Controller Logic (Master Implementation)
class BisscSlaveCtrl(generics: BisscGenerics) extends Component {
  import BisscResolutionHelper._

  val io = new Bundle {
    val bissc = master(BissCInterface())
    val position = out SInt (generics.resolutionBits bits)
    val error = out Bool()
    val warning = out Bool()
    val crcError = out Bool()
    val reset = in Bool()
    val startRequest = in Bool()
    val dataReady = out Bool()
    val speedConfig = in UInt(2 bits) // BiSS-C Speed Selection
    val resolutionConfig = in UInt(2 bits) // BiSS-C Resolution Selection
  }

  // Instantiate the BiSS-C Receiver
  val bissReceiver = new BissCReceiver(generics)
  io.bissc <> bissReceiver.io.bissc

  // Connect Signals
  io.position     := bissReceiver.io.position
  io.error        := bissReceiver.io.errorFlag
  io.warning      := bissReceiver.io.warningFlag
  io.crcError     := bissReceiver.io.crcErrorFlag
  io.dataReady    := bissReceiver.io.newDataReady

  // Control Signals
  bissReceiver.io.reset          := io.reset
  bissReceiver.io.startRequest   := io.startRequest
  bissReceiver.io.speedConfig    := io.speedConfig
  bissReceiver.io.resolutionConfig:= io.resolutionConfig
}

// BiSS-C Receiver Logic with Internal Clock Divider and Variable Resolutions
class BissCReceiver(generics: BisscGenerics) extends Component {
  import BisscResolutionHelper._

  val io = new Bundle {
    val bissc = master(BissCInterface())
    val position = out SInt (generics.resolutionBits bits)
    val errorFlag = out Bool()
    val warningFlag = out Bool()
    val crcErrorFlag = out Bool()
    val reset = in Bool()
    val startRequest = in Bool()
    val speedConfig = in UInt(2 bits) // BiSS-C Speed Selection
    val resolutionConfig = in UInt(2 bits) // BiSS-C Resolution Selection
    val newDataReady = out Bool()
  }

  // Registers for position and flags
  val positionReg    = Reg(SInt(generics.resolutionBits bits)) init(0)
  val errorReg       = Reg(Bool()) init(False)
  val warningReg     = Reg(Bool()) init(False)
  val crcErrorReg    = Reg(Bool()) init(False)
  val dataReadyReg   = Reg(Bool()) init(False)

  // CRC6 Instance using SpinalHDL's CRCGenerator
  val crc6 = new CRCGenerator(
    width = 6,
    polynomial = 0x03,
    reflectInput = false,
    reflectOutput = false,
    finalXOR = 0x3F // Inversion as per BiSS-C
  )

  // Clock Divider for BiSS-C Speed Selection
  val maClock = Reg(Bool()) init(False)
  val clockDivider = Reg(UInt(16 bits)) init(0)
  val currentDivider = Reg(UInt(16 bits)) init(50) // Default 1 MHz (50 MHz / 50)

  // Define possible speed settings
  // 0: 1 MHz (50 MHz / 50)
  // 1: 2 MHz (50 MHz / 25)
  // 2: 5 MHz (50 MHz / 10)
  // 3: 10 MHz (50 MHz / 5)
  val dividerValues = Vec(UInt(16 bits), 4)
  dividerValues(0) := 50   // 1 MHz
  dividerValues(1) := 25   // 2 MHz
  dividerValues(2) := 10   // 5 MHz
  dividerValues(3) := 5    // 10 MHz

  // Update the divider based on speedConfig
  when(io.speedConfig < dividerValues.length) {
    currentDivider := dividerValues(io.speedConfig)
  } otherwise {
    currentDivider := 50 // Default to 1 MHz if invalid
  }

  // Clock Divider Logic
  when(clockDivider === (currentDivider - 1)) {
    maClock := ~maClock
    clockDivider := 0
  } otherwise {
    clockDivider := clockDivider + 1
  }

  io.bissc.ma := maClock // Drive the MA clock

  // Define possible resolutions and corresponding bit widths
  val resolution = Reg(BisscResolution.type) init(BisscResolution.BIT_18)

  switch(io.resolutionConfig) {
    is(0) { resolution := BisscResolution.BIT_18 }
    is(1) { resolution := BisscResolution.BIT_26 }
    is(2) { resolution := BisscResolution.BIT_32 }
    is(3) { resolution := BisscResolution.BIT_36 }
  }

  // Compute resolution bits
  val resolutionBits = BisscResolution.getResolutionBits(resolution)

  // State Machine using SpinalHDL's FSM
  val fsm = new StateMachine {
    val idle = new State with EntryPoint {
      whenIsActive {
        dataReadyReg := False
        when(io.reset) {
          goto(idle)
          bitCounter := 0
          dataShiftReg := 0
          positionReg := 0
          errorReg := False
          warningReg := False
          crcErrorReg := False
          dataReadyReg := False
        } elsewhen(io.startRequest) {
          goto(receiving)
          bitCounter := 0
        }
      }
    }

    val receiving = new State {
      whenIsActive {
        when(maClock.rise()) {
          dataShiftReg := (dataShiftReg << 1) | io.bissc.slo.asUInt
          bitCounter := bitCounter + 1

          when(bitCounter === (resolutionBits + BisscConstants.frameOverheadBits - 1)) {
            goto(processing)
          }
        }
      }
    }

    val processing = new State {
      whenIsActive {
        // Extract position and status bits based on resolution
        val posStart = BisscConstants.frameOverheadBits + resolutionBits - 1
        val posEnd   = BisscConstants.frameOverheadBits

        val positionBits = dataShiftReg(posStart downto posEnd)
        val errorBit    = dataShiftReg(posEnd - 1)
        val warningBit  = dataShiftReg(posEnd - 2)
        val crcRx       = dataShiftReg(posEnd - 7 downto posEnd - 2)

        // Compute CRC on received data (position + error + warning)
        crc6.io.dataIn := dataShiftReg(posStart downto posEnd + 8) // Adjust bit range as needed

        // Update Registers
        positionReg := positionBits.asSInt
        errorReg    := errorBit.asBool
        warningReg  := warningBit.asBool
        crcErrorReg := crc6.io.crcOut =/= crcRx(5 downto 0) // Assuming CRC is 6 bits

        dataReadyReg := True

        goto(idle)
      }
    }
  }

  // Assign Outputs
  io.position      := positionReg
  io.errorFlag     := errorReg
  io.warningFlag   := warningReg
  io.crcErrorFlag  := crcErrorReg
  io.newDataReady  := dataReadyReg
}

// CRC6 Computation Using SpinalHDL's CRCGenerator
class CRCGenerator(width: Int, polynomial: BigInt, reflectInput: Boolean, reflectOutput: Boolean, finalXOR: BigInt) extends Component {
  val io = new Bundle {
    val dataIn = in UInt (width bits)
    val crcOut = out UInt (6 bits)
  }

  val crcGen = CRCGenerator(
    width = 6,
    polynomial = polynomial,
    reflectInput = reflectInput,
    reflectOutput = reflectOutput,
    finalXOR = finalXOR
  )

  io.crcOut := crcGen.compute(io.dataIn)
}

// Testbench Simulation
object Apb3BisscSlaveCtrlSim {
  def main(args: Array[String]): Unit = {
    SimConfig
      .withWave
      .compile(new Apb3BisscSlaveCtrl(BisscSlaveCtrlMemoryMappedConfig(new BisscGenerics(36, 50 MHz)))) // Set max resolution
      .doSim { dut =>
        // Initialize Signals
        dut.io.apb.PRESETn #= false
        dut.io.bissc.slo #= false
        dut.io.apb.PSEL #= false
        dut.io.apb.PENABLE #= false
        dut.io.apb.PWRITE #= false
        dut.io.apb.PADDR #= 0
        dut.io.apb.PWDATA #= 0

        // Release Reset
        sleep(20)
        dut.io.apb.PRESETn #= true
        sleep(20)

        // Define a list of test resolutions and corresponding sample positions
        val testResolutions = List(
          (0, 0x00003FFF),       // 18-bit
          (1, 0x03FFFFFF),       // 26-bit
          (2, 0xFFFFFFFF),       // 32-bit
          (3, 0x0FFFFFFFFF)      // 36-bit
        )

        for ((resCode, samplePosition) <- testResolutions) {
          // Configure BiSS-C Resolution
          apbWrite(dut, 0x1C, resCode) // Resolution Selection

          // Configure BiSS-C Speed (e.g., 1 MHz)
          apbWrite(dut, 0x18, 0) // Speed Selection: 0 -> 1 MHz

          // Compute CRC for position + error + warning
          val errorBit = false
          val warningBit = false
          val crcInput = (samplePosition << 10) | (if (errorBit) 1 << 9 else 0) | (if (warningBit) 1 << 8 else 0)
          val crcValue = computeCRC6(crcInput, BisscResolution.getResolutionBits(BisscResolution.getResolution(resCode)) + 2) // positionBits + error + warning
          val sampleFrame = (samplePosition << 10) | (if (errorBit) 1 << 9 else 0) | (if (warningBit) 1 << 8 else 0) | crcValue

          // Convert sampleFrame to bits (MSB-first)
          val resolution = BisscResolution.getResolution(UInt(resCode))
          val resolutionBits = BisscResolution.getResolutionBits(resolution)
          val frameBits = resolutionBits + BisscConstants.frameOverheadBits
          val dataBits = (0 until frameBits).map { i =>
            ((sampleFrame >> (frameBits - 1 - i)) & 1).toInt
          }.toArray

          // Fork a process to provide SLO data based on dataBits
          fork {
            // Wait until startRequest is asserted
            waitUntil(dut.io.bissc.ma.toBoolean && dut.io.startRequest.toBoolean) // Start of frame
            for (bit <- dataBits) {
              // Wait for rising edge of MA
              dut.clockDomain.waitRisingEdge()
              // Set SLO based on the current bit
              dut.io.bissc.slo #= (bit == 1)
            }
          }

          // Send Start Request
          apbWrite(dut, 0x14, 1) // Start request
          sleep(1)
          apbWrite(dut, 0x14, 0) // De-assert start request to make it a pulse

          // Wait for Data Ready Interrupt
          waitUntil(dut.io.interrupt.toBoolean)
          sleep(1)

          // Read Position
          val position = apbRead(dut, 0x00)
          println(s"Resolution: ${resolution.getName}, Position: 0x${position.toHexString}")

          // Read Error Flag
          val error = apbRead(dut, 0x04)
          println(s"Error Flag: $error")

          // Read Warning Flag
          val warning = apbRead(dut, 0x08)
          println(s"Warning Flag: $warning")

          // Read CRC Error Flag
          val crcError = apbRead(dut, 0x0C)
          println(s"CRC Error Flag: $crcError")

          // Validate CRC
          assert(!crcError, s"CRC Error detected for resolution code $resCode")

          // Validate Position
          assert(position.toBigInt == samplePosition, s"Position mismatch for resolution code $resCode")
        }

        // End Simulation
        simSuccess()
      }

    // APB3 Write Function
    def apbWrite(dut: Apb3BisscSlaveCtrl, address: Int, data: Int): Unit = {
      dut.io.apb.PSEL #= true
      dut.io.apb.PADDR #= address
      dut.io.apb.PWDATA #= data
      dut.io.apb.PWRITE #= true
      dut.io.apb.PENABLE #= false
      sleep(1) // Setup
      dut.io.apb.PENABLE #= true
      sleep(1) // Hold
      dut.io.apb.PSEL #= false
      dut.io.apb.PENABLE #= false
    }

    // APB3 Read Function
    def apbRead(dut: Apb3BisscSlaveCtrl, address: Int): Int = {
      dut.io.apb.PSEL #= true
      dut.io.apb.PADDR #= address
      dut.io.apb.PWRITE #= false
      dut.io.apb.PENABLE #= false
      sleep(1) // Setup
      dut.io.apb.PENABLE #= true
      sleep(1) // Hold
      val readValue = dut.io.apb.PRDATA.toInt
      dut.io.apb.PSEL #= false
      readValue
    }

    // Scala-based CRC6 computation for simulation purposes
    def computeCRC6(data: BigInt, width: Int): BigInt = {
      var crc = 0L
      for (i <- (width - 1) to 0 by -1) {
        val bit = ((data >> i) & 1).toInt ^ ((crc >> 5) & 1).toInt
        crc = ((crc << 1) & 0x3F) | bit
        if (bit == 1) {
          crc ^= 0x03 // Polynomial x^6 + x + 1
        }
      }
      ~crc & 0x3F // Invert CRC and mask to 6 bits
    }
}
