import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.core.sim._

// Constants and Configurations
object BisscConstants {
  val frameOverheadBits = 10 // Start bit, error, warning, CRC
}

// Enumeration for supported BiSS-C encoder resolutions
object BisscResolution extends SpinalEnum {
  val BIT_18 = newElement()
  val BIT_26 = newElement()
  val BIT_32 = newElement()
  val BIT_36 = newElement()
}

// Companion object for BisscResolution to get bit widths
object BisscResolution {
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
case class BisscGenerics(resolutionBits: Int, maxClockFreq: HertzNumber)

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

  // CRC6 Instance
  val crc6 = new CRC6()

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

  // State Machine States
  val idle :: receiving :: processing :: Nil = Enum(3)
  val state = RegInit(idle)

  // Data Reception Registers
  // Maximum frame length: 36 (resolution) + 10 (overhead) = 46 bits
  val bitCounter = Reg(UInt(log2Up(36 + BisscConstants.frameOverheadBits) bits)) init(0)
  val dataShiftReg = Reg(UInt(46 bits)) init(0) // 36 + 10 bits

  // State Machine Logic
  switch(state) {
    is(idle) {
      dataReadyReg := False
      when(io.reset) {
        state := idle
        bitCounter := 0
        dataShiftReg := 0
        positionReg := 0
        errorReg := False
        warningReg := False
        crcErrorReg := False
        dataReadyReg := False
      } elsewhen(io.startRequest) {
        state := receiving
        bitCounter := 0
      }
    }

    is(receiving) {
      when(maClock.rise()) { // On rising edge of MA
        dataShiftReg := (dataShiftReg << 1) | io.bissc.slo.asUInt
        bitCounter := bitCounter + 1

        when(bitCounter === (resolutionBits + BisscConstants.frameOverheadBits - 1)) {
          state := processing
        }
      }
    }

    is(processing) {
      // Extract position and status bits based on resolution
      val posStart = BisscConstants.frameOverheadBits + resolutionBits - 1
      val posEnd   = BisscConstants.frameOverheadBits

      val positionBits = dataShiftReg(posStart downto posEnd)
      val errorBit    = dataShiftReg(BisscConstants.frameOverheadBits - 1)
      val warningBit  = dataShiftReg(BisscConstants.frameOverheadBits - 2)
      val crcRx       = dataShiftReg(BisscConstants.frameOverheadBits - 7 downto 0)

      // Compute CRC on received data (position + error + warning)
      val crcComputed = crc6.compute(dataShiftReg(posStart downto posEnd + 8)) // Adjust bit range as needed

      // Update Registers
      positionReg := positionBits.asSInt
      errorReg    := errorBit.asBool
      warningReg  := warningBit.asBool
      crcErrorReg := crcComputed =/= crcRx(5 downto 0) // Assuming CRC is 6 bits

      dataReadyReg := True

      state := idle
    }
  }

  // Assign Outputs
  io.position      := positionReg
  io.errorFlag     := errorReg
  io.warningFlag   := warningReg
  io.crcErrorFlag  := crcErrorReg
  io.newDataReady  := dataReadyReg
}

// CRC6 Computation According to BiSS-C Specification
class CRC6 extends Component {
  // Define a method to compute CRC6 on a data word
  def compute(data: UInt): UInt = {
    val crc = Reg(UInt(6 bits)) init(0)

    // Reset CRC
    crc := 0

    // Iterate over each bit (MSB-first)
    for (i <- (data.getWidth - 1) downto 0) {
      val bit = data(i) ^ crc(5)
      crc(5 downto 1) := crc(4 downto 0)
      crc(0) := bit
      when(bit) {
        crc(5 downto 0) := crc(5 downto 0) ^ 0x03 // Polynomial x^6 + x + 1
      }
    }

    ~crc // Invert CRC as per BiSS-C specification
  }
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
          val resolution = BisscResolution.getResolution(dut.io.resolutionConfig)
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
