import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fifo._
import spinal.lib.bus.wishbone._

//    Support for stereo audio transmission (and more channels with TDM)
//    Generation of the Word Select (LRCK) signal to indicate the start of a new audio word
//    Generation of the Bit Clock (SCLK) signal to clock the audio data
//    Generation of the Serial Data (SD) signal to transmit the audio data
//    Support for multiple sample rates (up to 384 kHz) and master clock frequencies (12.288 MHz, 24.576 MHz, 45.1580 MHz, and 49.1520 MHz)
//    Support for external master clock input
//    Support for switching between I2S and TDM modes
//    Ability to select the number of TDM channels per line
//    Support for 24-bit audio data width
//    Use of appropriate synchronization and buffering techniques to ensure reliable data transmission



case class I2sTransmitter(
  bitsPerSample: Int = 24,
  samplingRate: HertzNumber = 96 kHz,
  tdmChannelsPerLine: Int = 0,
  mclkRatio: Int = 256,
  masterClockFrequency: HertzNumber = 49.152 MHz
) extends Component {

  val io = new Bundle {
    val lrck = in Bool
    val mclk = in Bool
    val data = master(Flow(Vec(Bits(bitsPerSample bits), tdmChannelsPerLine + 2)))
    val modeRegister = in UInt(32 bits)
  }

  val sclkCounter = Counter(masterClockFrequency.toInt / (2 * samplingRate.toInt) - 1)
  val dataCounter = Counter(tdmChannelsPerLine + 2)
  val mclkCounter = Counter(masterClockFrequency.toInt / (samplingRate.toInt * mclkRatio) - 1)

  val dataReg = RegNextWhen(io.data.payload, io.lrck)

  io.data.ready := io.lrck
  io.lrck := sclkCounter.willOverflow
  io.mclk := mclkCounter.willOverflow

  val sclkToggle = sclkCounter.willOverflow
  val wsToggle = sclkToggle && (dataCounter.value === 0 || dataCounter.value === 1)

  val channelData = Reg(Bits(bitsPerSample bits))
  channelData := dataReg(dataCounter)
  val dataOut = Reg(Bits(2 * bitsPerSample bits))
  dataOut(2 * bitsPerSample - 1 downto bitsPerSample) := channelData
  dataOut(bitsPerSample - 1 downto 0) := wsToggle.asBits.resized(bitsPerSample)

  when(sclkToggle) {
    dataCounter.increment()
  }

  io.data.payload := dataOut

  // Extract mode register values
  val i2sMode = io.modeRegister(1 downto 0)
  val tdmChannelsPerLineMode = io.modeRegister(7 downto 2)
  val sampleRateMode = io.modeRegister(15 downto 8)
  val masterClockFrequencyMode = io.modeRegister(19 downto 16)
  val mclkToLrckRatio = io.modeRegister(28 downto 24)

  // Update parameters based on mode register values
  val channelsPerLine = if (tdmChannelsPerLineMode > 0) tdmChannelsPerLineMode * 2 else tdmChannelsPerLine
  val bitsPerChannel = bitsPerSample
  val masterClock = masterClockFrequencyMode.mux(
    U"4'b0000" -> 12.288 MHz,
    U"4'b0001" -> 24.576 MHz,
    U"4'b0010" -> 45.158 MHz,
    U"4'b0011" -> 49.152 MHz
  )
  val samplingRate = sampleRateMode.mux(
    U"8'b000_01000" -> 32 kHz,
    U"8'b000_11101" -> 44.1 kHz,
    U"8'b001_00000" -> 48 kHz,
    U"8'b001_10000" -> 64 kHz,
    U"8'b010_00100" -> 88.2 kHz,
    U"8'b010_00000" -> 96 kHz,
    U"8'b100_00000" -> 128 kHz,
    U"8'b110_00100" -> 176.4 kHz,
    U"8'b110_00000" -> 192 kHz,
    U"8'b111_00000" -> 384 kHz
    )
  
  // Update counters based on mode register values
  sclkCounter.init(masterClock.toInt / (2 * samplingRate.toInt) - 1)
  mclkCounter.init(masterClock.toInt / (samplingRate.toInt * mclkToLrckRatio.toInt) - 1)
  dataCounter.init(channelsPerLine + 2)

  // Override parameters with TDM mode values if TDM is enabled
  if (tdmChannelsPerLine > 0) {
    channelsPerLine := tdmChannelsPerLine
    bitsPerChannel := bitsPerSample - log2Up(tdmChannelsPerLine)
  }

  // Update dataOut based on number of channels and bits per channel
  val channelBits = Vec(Bits(bitsPerChannel bits), channelsPerLine)
  val channelOuts = channelBits.map { channel =>
  val channelData = Reg(Bits(bitsPerSample bits))
  channelData := dataReg(dataCounter)
  dataCounter.increment()
  channelData.resized(bitsPerChannel)
  }
  
val dataOut = Reg(Bits(channelsPerLine * bitsPerChannel * 2 bits))
  
  (0 until channelsPerLine).foreach { i =>
  dataOut((i + 1) * bitsPerChannel * 2 - 1 downto i * bitsPerChannel * 2) := channelOuts(i)
  }
  dataOut(2 * bitsPerSample - 1 downto bitsPerSample) := wsToggle.asBits.resized(bitsPerSample)

  io.data.payload := dataOut
}





import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone._

case class I2sTransmitterWishbone(
  config: WishboneConfig,
  channels: Int = 2,
  bitsPerSample: Int = 24,
  samplingRate: Int = 48000,
  tdmChannelsPerLine: Int = 0,
  mclkRatio: Int = 64,
  masterClockFrequency: HertzNumber = 12.288 MHz
) extends Component {
  val io = new Bundle {
    val i2sTransmitter = master(I2sTransmitter(channels, bitsPerSample, tdmChannelsPerLine))
    val bus = slave(Wishbone(config))
  }

  val modeRegisterWidth = 32
  val sampleCounterWidth = log2Up(samplingRate / mclkRatio)
  val mclkCounterWidth = log2Up(masterClockFrequency.toBigInt / (mclkRatio * samplingRate).toBigInt)

  val modeRegister = Reg(UInt(modeRegisterWidth bits)) init(0)
  val sampleCounter = Counter(samplingRate / mclkRatio)
  val mclkCounter = Counter(masterClockFrequency / (mclkRatio * samplingRate))

  io.i2sTransmitter.data.valid := sampleCounter.willOverflow
  io.i2sTransmitter.data.channel := 0
  io.i2sTransmitter.data.data := 0

  when(sampleCounter.willOverflow) {
    sampleCounter.clear()
    when(mclkCounter.willOverflow) {
      mclkCounter.clear()
    }
    io.i2sTransmitter.lrck #= ~io.i2sTransmitter.lrck
  }

  when(mclkCounter.willOverflow) {
    mclkCounter.clear()
    io.i2sTransmitter.sclk #= ~io.i2sTransmitter.sclk
  }

  val readWriteHandler = new Area {
    val address = io.bus.ADR(config.addressWidth - 1 downto 2)
    val sel = io.bus.SEL
    val we = io.bus.WE
    val datMosi = io.bus.DAT_MOSI.asBits
    val datMiso = io.bus.DAT_MISO.asReg(Bits(32 bits)) init (0)

    val busReq = io.bus.CYC && io.bus.STB
    val busAck = Reg(Bool) init (False)
    val byteEnableReg = Reg(Bits(4 bits))

    val addrMode = address(config.addressWidth - 1 downto 2)
    val addrData = address(config.addressWidth - 1 downto 0)

    when(busReq && !busAck) {
      byteEnableReg := sel
      when(we) {
        switch(addrMode) {
          for (i <- 0 until modeRegisterWidth by 8) {
            is(U(i / 8)) {
              modeRegister(i + 7 downto i) := datMosi(i + 7 downto i).asUInt
            }
          }
        }
      }.otherwise {
        datMiso := B"32'd0"
        switch(addrMode) {
          is(U"00") { // Mode register
            datMiso := modeRegister(addrData * 8, 8 bits).asUInt
          }
          is(U"01") { // Sample rate
            datMiso := samplingRate.asBits.resized(addrData * 8 + 8) >> addrData * 8
          }
          is(U"02") { // TDM channels per line
            datMiso := tdmChannelsPerLine.asBits.resized(addrData * 8 + 8) >> addrData * 8
          }
          is(U"03") { // MCLK ratio
            datMiso := mclkRatio.asBits.resized(addrData * 8 + 8) >> addrData * 8
          }
          is(U"04") { // Master clock frequency
            datMiso := masterClockFrequency.toBigInt.asBits.resized(addrData * 8 + 8) >> addrData * 8
          }
        }
      }
      busAck := True
    }

    when(io.bus.ACK) {
      busAck := False
    }
  }

  val writeHandler = new Area {
    val busData = io.bus.DAT_MOSI.asBits
    val busWrite = io.bus.WE
    val busAddress = io.bus.ADR(config.addressWidth - 1 downto 2)

    when(io.bus.STB && io.bus.CYC && !io.bus.SEL(3)) {
      switch(busAddress) {
        is(U"00") { // Data register
          when(busWrite) {
            io.i2sTransmitter.data.channel := busData(bitsPerSample - 1 downto 0).asUInt
            io.i2sTransmitter.data.data := busData(23 downto bitsPerSample).asUInt
          }
        }
      }
    }
  }
}






object I2sTransmitterGenerator extends App {
  val config = I2sConfig()
  SpinalConfig(targetDirectory = "output").generateVerilog(I2sTransmitter(config))
}


object I2sTransmitterSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new I2sTransmitterSimTopLevel).doSimUntilVoid()
  }
}

class I2sTransmitterSimTopLevel extends Component {
  val io = new Bundle {
    val i2sTransmitter = master(I2sTransmitter(I2sConfig()))
  }

  val clockDomain = ClockDomain.external("master", frequency = 100 MHz)
  val clockArea = new ClockingArea(clockDomain) {
    val modeRegister = Reg(UInt(32 bits)) init 0
    io.i2sTransmitter.modeRegister := modeRegister

    val audioInput = Stream(AudioSample(I2sConfig()))
    audioInput << Stream(AudioSample(I2sConfig())).m2sPipe()
    io.i2sTransmitter.audioInput << audioInput

    io.i2sTransmitter.masterClock.foreach(_ := False)
  }

  clockDomain.forkStimulus(10)

  val audioSamples = Seq.fill(100)(AudioSample(I2sConfig()))
  audioSamples.zipWithIndex.foreach { case (audioSample, index) =>
    audioSample.channels.zipWithIndex.foreach { case (channel, channelIndex) =>
      channel.randomize()
    }

    val wordIndex = index % 24
    val channelIndex = index / 24

    val sclk = index % 2 == 0
    val lrck = wordIndex == 0

    val sd = audioSample.channels(channelIndex).asBits(wordIndex * 8, 24 bits)

    clockDomain.waitSampling(1)
    clockDomain.assert(io.i2sTransmitter.i2s.sd(channelIndex) === sd)
    clockDomain.assert(io.i2sTransmitter.i2s.lrck === lrck.toBoolean)
    clockDomain.assert(io.i2sTransmitter.i2s.sclk === sclk.toBoolean)
  }
}




object I2sTransmitterWishboneSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile {
      val dut = new I2sTransmitterWishbone(
        WishboneConfig(16, 32)
      )
      dut
    }.doSim { dut =>
      val bus = dut.io.bus
      val i2sTransmitter = dut.io.i2sTransmitter

      // Reset the I2S transmitter
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(5)
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.io.bus.WE #= false
      dut.io.bus.ADR #= 0
      dut.io.bus.DAT_MOSI #= 0
      dut.clockDomain.waitSampling(5)

      // Write to the mode register to configure the I2S transmitter
      bus.CYC #= true
      bus.STB #= true
      bus.WE #= true
      bus.ADR #= 0
      bus.DAT_MOSI #= 0x12345678
      dut.clockDomain.waitSampling(5)
      bus.CYC #= false
      bus.STB #= false
      bus.WE #= false
      dut.clockDomain.waitSampling(5)

      // Read the mode register to confirm that it was set correctly
      bus.CYC #= true
      bus.STB #= true
      bus.WE #= false
      bus.ADR #= 0
      dut.clockDomain.waitSampling(5)
      assert(bus.DAT_MISO.toLong == 0x12345678)

      // Transmit some test data on the I2S interface
      i2sTransmitter.modeRegister #= 0x80000000
      dut.clockDomain.waitSampling(5)
      for (i <- 0 until 96) {
        i2sTransmitter.data #= i.toLong << 24 | i.toLong << 16 | i.toLong << 8 | i.toLong
        dut.clockDomain.waitSampling(1)
      }
    }
  }
}

