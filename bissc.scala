import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.core.sim._

// Constants and Configurations
object BisscConstants {
  val frameOverheadBits = 10 // Start bit, error, warning, CRC
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

  // Interrupt Logic
  val interruptSignal = bisscCtrl.io.dataReady || bisscCtrl.io.error || bisscCtrl.io.crcError
  io.interrupt := interruptSignal
  busCtrl.read(interruptSignal,              0x1C, "Interrupt status")
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
  bissReceiver.io.reset        := io.reset
  bissReceiver.io.startRequest := io.startRequest
  bissReceiver.io.speedConfig  := io.speedConfig
}

// BiSS-C Receiver Logic with Internal Clock Divider
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

  // State Machine States
  val idle :: receiving :: processing :: Nil = Enum(3)
  val state = RegInit(idle)

  // Data Reception Registers
  val bitCounter = Reg(UInt(log2Up(generics.resolutionBits + BisscConstants.frameOverheadBits) bits)) init(0)
  val dataShiftReg = Reg(UInt((generics.resolutionBits + BisscConstants.frameOverheadBits) bits)) init(0)

  // State Machine Logic
  switch(state) {
    is(idle) {
      dataReadyReg := False
      when(io.startRequest) {
        state := receiving
        bitCounter := 0
      }
    }

    is(receiving) {
      when(maClock.rise()) { // On rising edge of MA
        dataShiftReg := (dataShiftReg << 1) | io.bissc.slo.asUInt
        bitCounter := bitCounter + 1

        when(bitCounter === (generics.resolutionBits + BisscConstants.frameOverheadBits - 1)) {
          state := processing
        }
      }
    }

    is(processing) {
      // Extract position and status bits
      val positionBits = dataShiftReg(generics.resolutionBits + 9 downto 10)
      val errorBit    = dataShiftReg(9)
      val warningBit  = dataShiftReg(8)
      val crcRx       = dataShiftReg(5 downto 0)

      // Compute CRC on received data (position + error + warning)
      val crcComputed = crc6.compute(dataShiftReg(generics.resolutionBits + 9 downto 6))

      // Update Registers
      positionReg := positionBits.asSInt
      errorReg    := errorBit.asBool
      warningReg  := warningBit.asBool
      crcErrorReg := crcComputed =/= crcRx

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
      .compile(new Apb3BisscSlaveCtrl(BisscSlaveCtrlMemoryMappedConfig(new BisscGenerics(32, 50 MHz))))
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

        // Define a sample BiSS-C frame (position + error + warning + CRC)
        // For example, 32-bit position, 1 error bit, 1 warning bit, 6 CRC bits
        val samplePosition = 0x12345678 // Example position
        val errorBit = false
        val warningBit = false
        // Compute CRC for position + error + warning
        val crcInput = (samplePosition << 10) | (errorBit.toInt << 9) | (warningBit.toInt << 8)
        val crc6 = new CRC6()
        val crcValue = crc6.compute(crcInput.asUInt((32 + 10) bits)).toBigInt
        val sampleFrame = (samplePosition << 10) | (errorBit.toInt << 9) | (warningBit.toInt << 8) | crcValue

        // Convert sampleFrame to bits (MSB-first)
        val dataBits = (0 until (dut.gens.resolutionBits + BisscConstants.frameOverheadBits)).map { i =>
          ((sampleFrame >> (dut.gens.resolutionBits + BisscConstants.frameOverheadBits - 1 - i)) & 1).toInt
        }.toArray

        // Fork a process to provide SLO data based on dataBits
        fork {
          // Wait until startRequest is asserted
          waitUntil(dut.io.bissc.ma.toBoolean && dut.io.startRequest.toBoolean)
          for (bit <- dataBits) {
            // Wait for rising edge of MA
            dut.clockDomain.waitRisingEdge()
            // Set SLO based on the current bit
            dut.io.bissc.slo #= (bit == 1)
          }
        }

        // APB3 Write and Read Functions
        def apbWrite(address: Int, data: Int): Unit = {
          // APB3 Write Sequence
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

        def apbRead(address: Int): Int = {
          // APB3 Read Sequence
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

        // Configure BiSS-C Speed (e.g., 1 MHz)
        apbWrite(0x18, 0) // Speed Selection: 0 -> 1 MHz

        // Send Start Request
        apbWrite(0x14, 1) // Start request

        // Wait for Data Ready Interrupt
        waitUntil(dut.io.interrupt.toBoolean)

        // Read Position
        val position = apbRead(0x00)
        println(s"Position: 0x${position.toHexString}")

        // Read Error Flag
        val error = apbRead(0x04)
        println(s"Error Flag: $error")

        // Read Warning Flag
        val warning = apbRead(0x08)
        println(s"Warning Flag: $warning")

        // Read CRC Error Flag
        val crcError = apbRead(0x0C)
        println(s"CRC Error Flag: $crcError")

        // End Simulation
        simSuccess()
      }
  }
}
