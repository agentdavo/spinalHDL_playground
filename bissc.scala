import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.lib.bus.amba3.apb._

// Configuration for BiSS-C slave control
case class BisscSlaveCtrlMemoryMappedConfig(ctrlGenerics: BisscGenerics)

// Generic configuration for BiSS-C controller
class BisscGenerics(val resolutionBits: Int, val maxClockFreq: HertzNumber)

// Define frame overhead bits (start bit, error, warning, CRC)
val frameOverheadBits = 10

// BiSS-C Slave Controller with APB3 integration
case class Apb3BisscSlaveCtrl(generics: BisscSlaveCtrlMemoryMappedConfig) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3BisscSlaveCtrl.getApb3Config)) // APB3 interface
    val bissc = master(Bissc())                             // BiSS-C SPI interface
    val interrupt = out Bool()                              // Interrupt for host system
  }

  // Instantiate BiSS-C control logic
  val bisscCtrl = new BisscSlaveCtrl(generics.ctrlGenerics)
  io.bissc <> bisscCtrl.io.spi

  // APB3 slave factory for memory-mapped control and data registers
  val busCtrl = Apb3SlaveFactory(io.apb)

  // Connect BiSS-C control to APB3 registers
  val bridge = bisscCtrl.io.driveFrom(busCtrl, 0)(generics)
  io.interrupt := bridge.interruptCtrl.interrupt

  // Memory-mapped register addresses for position, error, and status flags
  busCtrl.read(bisscCtrl.io.position, address = 0x00, documentation = "Position data")
  busCtrl.read(bisscCtrl.io.error, address = 0x04, documentation = "Error status")
  busCtrl.read(bisscCtrl.io.warning, address = 0x08, documentation = "Warning status")
  busCtrl.read(bisscCtrl.io.crcError, address = 0x0C, documentation = "CRC Error status")

  // Control registers for reset and start request
  busCtrl.drive(bisscCtrl.io.reset, address = 0x10, documentation = "Reset control")
  busCtrl.drive(bisscCtrl.io.startRequest, address = 0x14, documentation = "Start request")

  // Interrupt when data is ready or error occurs
  busCtrl.createInterrupt(
    interruptSource = bisscCtrl.io.dataReady || bisscCtrl.io.error || bisscCtrl.io.crcError,
    address = 0x18,
    documentation = "Interrupt status"
  )
}

// BiSS-C Slave Controller logic
class BisscSlaveCtrl(generics: BisscGenerics) extends Component {
  val io = new Bundle {
    val spi = master(Bissc())               // BiSS-C SPI interface
    val position = out SInt (generics.resolutionBits bits) // Position data output
    val error = out Bool()                  // Error flag
    val warning = out Bool()                // Warning flag
    val crcError = out Bool()               // CRC error flag
    val reset = in Bool()                   // Reset control
    val startRequest = in Bool()            // Start request control
    val dataReady = out Bool()              // Data ready flag
  }

  // Instantiate the BiSS-C receiver for protocol management
  val bissCtrl = new BissCReceiver(generics.maxClockFreq, generics.maxClockFreq, generics.resolutionBits)

  // Connect SPI and status signals
  io.spi <> bissCtrl.io.bissc
  io.position := bissCtrl.io.position
  io.error := bissCtrl.io.errorFlag
  io.warning := bissCtrl.io.warningFlag
  io.crcError := bissCtrl.io.crcErrorFlag
  io.dataReady := bissCtrl.io.newDataReady

  // Handle reset and start request
  bissCtrl.io.reset := io.reset
  bissCtrl.io.startRequest := io.startRequest
}

// Define BiSS-C interface (SPI-like)
case class Bissc() extends Bundle with IMasterSlave {
  val sck = Bool()  // Clock signal
  val miso = Bool() // Master in slave out
  val mosi = Bool() // Master out slave in

  override def asMaster(): Unit = {
    out(sck, mosi)
    in(miso)
  }
}

// Configuration for APB3 interface
object Apb3BisscSlaveCtrl {
  def getApb3Config = Apb3Config(
    addressWidth = 16,
    dataWidth = 32
  )
}

// BiSS-C Receiver logic with ClockingArea
class BissCReceiver(clkFreq: HertzNumber, maxClockFreq: HertzNumber, defaultResolution: Int = 32) extends Component {
  val io = new Bundle {
    val bissc = master(Bissc())
    val position = out SInt (defaultResolution bits)
    val errorFlag = out Bool ()
    val warningFlag = out Bool ()
    val crcErrorFlag = out Bool ()
    val reset = in Bool ()
    val startRequest = in Bool ()
    val newDataReady = out Bool()
  }

  // Define clocking area to manage clock and state transitions
  val clockDomain = ClockDomain.current
  val area = new ClockingArea(clockDomain) {
    // State machine definition
    val idle :: request :: ack :: readData :: timeout :: reset :: stopClock :: Nil = Enum(7)
    val state = Reg(idle)

    val bitCounter = Reg(UInt(log2Up(64) bits)) init(0)  // Bit counter for data reception
    val timeoutCounter = Reg(UInt(16 bits)) init(0)      // Timeout counter for 31.25 µs
    val timeoutLimit = U((clkFreq.toDouble * 31.25e-6).toInt, 16 bits) // 31.25 µs period

    // Registers for position, flags, and CRC
    val positionReg = Reg(SInt(36 bits)) init(0)
    val dataShiftReg = Reg(UInt(64 bits)) init(0)
    val errorFlag = Reg(Bool()) init(False)
    val warningFlag = Reg(Bool()) init(False)
    val crcErrorFlag = Reg(Bool()) init(False)
    val newDataFlag = Reg(Bool()) init(False)

    // Clock generation logic
    val bissClock = Reg(Bool()) init(False)
    val counter = Reg(UInt(6 bits)) init(0)
    val clockDivRatio = Reg(UInt(3 bits)) init(0)

    val clockEnable = (counter === clockDivRatio)
    when(clockEnable) {
      bissClock := !bissClock
      counter := 0
    } otherwise {
      counter := counter + 1
    }

    io.bissc.sck := bissClock

    // Main state machine
    switch(state) {
      is(idle) {
        when(io.bissc.miso) {
          state := request
        }
        when(io.reset) {
          state := reset
        }
      }

      is(request) {
        bissClock := True
        state := ack
      }

      is(ack) {
        when(bissClock.rise() && bitCounter === 1) {
          when(!io.bissc.miso) {
            state := readData
            bitCounter := 0
          }
        }
      }

      is(readData) {
        when(bissClock.fall()) {
          dataShiftReg := dataShiftReg(62 downto 0) ## io.bissc.miso
          bitCounter := bitCounter + 1

          // Read full frame (position + error + warning + CRC)
          when(bitCounter === (defaultResolution + frameOverheadBits)) {
            bitCounter := 0
            processFrame(dataShiftReg)
            state := timeout
            timeoutCounter := 0
          }
        }
      }

      is(timeout) {
        when(io.bissc.miso) {
          bissClock := False
          state := stopClock
        } otherwise {
          bissClock := True
          timeoutCounter := timeoutCounter + 1

          when(timeoutCounter === timeoutLimit) {
            state := stopClock
          }
        }
      }

      is(reset) {
        bissClock := False
        timeoutCounter := 0

        when(timeoutCounter < timeoutLimit) {
          timeoutCounter := timeoutCounter + 1
        } otherwise {
          bissClock := True
          state := idle
        }
      }

      is(stopClock) {
        bissClock := False
        state := idle
      }
    }

    // Process received data frame (position, error, warning, CRC)
    def processFrame(frameData: UInt): Unit = {
      val alignedData = frameData |<< frameData.leadingZeros
      val crcRx = alignedData(5 downto 0)
      val dataRx = alignedData(62 downto 6)
      val errorWarning = dataRx(1 downto 0)
      val newPos = defaultResolution match {
        case 18 => dataRx(19 downto 2).asSInt
        case 26 => dataRx(27 downto 2).asSInt
        case 32 => dataRx(33 downto 2).asSInt
        case 36 => dataRx(37 downto 2).asSInt
      }

      // Extract error, warning, and CRC check
      errorFlag := errorWarning(1) === False
      warningFlag := errorWarning(0) === False
      crcErrorFlag := computeCRC(dataRx) =/= crcRx

      // Update position if CRC is valid
      if (!crcErrorFlag) {
        positionReg := newPos
        newDataFlag := True
      } else {
        newDataFlag := False
      }
    }

    // CRC logic (moved to separate component for reusability)
    val crc6 = new CRC6()
    def computeCRC(data: UInt): UInt = crc6.compute(data)
  }

  io.position := area.positionReg
  io.errorFlag := area.errorFlag
  io.warningFlag := area.warningFlag
  io.crcErrorFlag := area.crcErrorFlag
  io.newDataReady := area.newDataFlag
}

// Separate CRC6 component for modularity and reuse
class CRC6 extends Component {
  val crc6Table = Vec(UInt(6 bits), 64)
  crc6Table := Vec(
    U"6'h00", U"6'h03", U"6'h06", U"6'h05", U"6'h0C", U"6'h0F", U"6'h0A", U"6'h09",
    U"6'h18", U"6'h1B", U"6'h1E", U"6'h1D", U"6'h14", U"6'h17", U"6'h12", U"6'h11",
    U"6'h30", U"6'h33", U"6'h36", U"6'h35", U"6'h3C", U"6'h3F", U"6'h3A", U"6'h39",
    U"6'h28", U"6'h2B", U"6'h2E", U"6'h2D", U"6'h24", U"6'h27", U"6'h22", U"6'h21",
    U"6'h23", U"6'h20", U"6'h25", U"6'h26", U"6'h2F", U"6'h2C", U"6'h29", U"6'h2A",
    U"6'h3B", U"6'h38", U"6'h3D", U"6'h3E", U"6'h37", U"6'h34", U"6'h31", U"6'h32",
    U"6'h13", U"6'h10", U"6'h15", U"6'h16", U"6'h1F", U"6'h1C", U"6'h19", U"6'h1A",
    U"6'h0B", U"6'h08", U"6'h0D", U"6'h0E", U"6'h07", U"6'h04", U"6'h01", U"6'h02"
  )

  def compute(data: UInt): UInt = {
    var crc = U(0, 6 bits)
    for (i <- 0 until (data.getWidth / 6)) {
      val dataSegment = data((6 * i + 5) downto (6 * i))
      crc = crc6Table((dataSegment ^ crc)(5 downto 0))
    }
    ~crc
  }
}

// Define the clock and reset parameters
case class TestClockConfig(clkPeriod: TimeNumber = 10 ns)

// Test bench for Apb3BisscSlaveCtrl
object Apb3BisscSlaveCtrlSim {
  def main(args: Array[String]): Unit = {
    SimConfig
      .withWave
      .compile(new Apb3BisscSlaveCtrl(BisscSlaveCtrlMemoryMappedConfig(new BisscGenerics(32, 50 MHz))))
      .doSim { dut =>
        // Generate clock and reset
        val clkConfig = TestClockConfig()
        dut.clockDomain.forkStimulus(period = clkConfig.clkPeriod.toLong)

        // Reset signal
        dut.io.apb.PRESETn #= false
        sleep(10) // Hold reset for a while
        dut.io.apb.PRESETn #= true

        // SPI signals (BiSS-C encoder simulation)
        dut.io.bissc.miso #= false
        dut.io.bissc.mosi #= false
        dut.io.bissc.sck #= false

        // APB3 signals initialization
        dut.io.apb.PSEL #= false
        dut.io.apb.PENABLE #= false
        dut.io.apb.PWRITE #= false
        dut.io.apb.PADDR #= 0
        dut.io.apb.PWDATA #= 0

        // Simulation step
        def apbWrite(address: Int, data: Int): Unit = {
          dut.io.apb.PSEL #= true
          dut.io.apb.PADDR #= address
          dut.io.apb.PWDATA #= data
          dut.io.apb.PWRITE #= true
          dut.io.apb.PENABLE #= false
          sleep(1) // Cycle delay
          dut.io.apb.PENABLE #= true
          sleep(1) // Acknowledge
          dut.io.apb.PSEL #= false
        }

        def apbRead(address: Int): Int = {
          dut.io.apb.PSEL #= true
          dut.io.apb.PADDR #= address
          dut.io.apb.PWRITE #= false
          dut.io.apb.PENABLE #= false
          sleep(1) // Cycle delay
          dut.io.apb.PENABLE #= true
          sleep(1) // Acknowledge
          val readValue = dut.io.apb.PRDATA.toInt
          dut.io.apb.PSEL #= false
          readValue
        }

        // Simulate BiSS-C encoder data transmission (simulate an encoder sending valid data)
        def sendBisscData(data: BigInt): Unit = {
          val dataBits = data.toString(2).reverse.padTo(64, '0').reverse // 64-bit data (LSB-first)
          for (i <- 0 until dataBits.length) {
            dut.io.bissc.miso #= dataBits(i) == '1'
            dut.io.bissc.sck #= true
            sleep(1) // Half clock cycle
            dut.io.bissc.sck #= false
            sleep(1) // Half clock cycle
          }
        }

        // Send a valid BiSS-C frame (simulate a 32-bit position frame with error, warning, and CRC)
        val validPosition = BigInt("11000000001111000000011100001111000011111111111111111100000000", 2) // Sample data
        sendBisscData(validPosition)

        // Read APB3 register for position
        val position = apbRead(0x00)
        println(s"Position: $position")

        // Assert that the position matches the expected value
        assert(position == 0x123456) // Replace with actual expected position value

        // Test for error and warning flags
        val error = apbRead(0x04)
        println(s"Error Flag: $error")

        val warning = apbRead(0x08)
        println(s"Warning Flag: $warning")

        // End simulation
        simSuccess()
      }
  }
}

