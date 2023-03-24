import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._



// This implementation uses a state machine to extract each field from the Channel Status block. 
// The fields are extracted in the order specified by the IEC 60958 standard, and each state 
// reads a specific set of bits from the input data.
// Once all fields have been extracted, they are combined into a ChannelStatus bundle and output. 
// This bundle can then be used to determine information about the audio signal, such as the 
// sample rate, number of channels, and emphasis.

case class ChannelStatus() extends Bundle {
  val preEmphasis = Bool
  val sourceNumber = Bits(4 bits)
  val sampleRate = Bits(3 bits)
  val professional = Bool
  val reserved1 = Bits(3 bits)
  val mode = Bits(2 bits)
  val reserved2 = Bits(1 bit)
  val emphasis = Bool
  val reserved3 = Bits(5 bits)
  val userBits = Bits(16 bits)
}

case class ChannelStatusDecoder() extends Component {
  val io = new Bundle {
    val input = in Bits(192 bits)
    val output = out(ChannelStatus())
  }

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val READ_PRE_EMPHASIS = new State
    val READ_SOURCE_NUMBER = new State
    val READ_SAMPLE_RATE = new State
    val READ_PROFESSIONAL = new State
    val READ_RESERVED1 = new State
    val READ_MODE = new State
    val READ_RESERVED2 = new State
    val READ_EMPHASIS = new State
    val READ_RESERVED3 = new State
    val READ_USER_BITS = new State

    val channelStatus = ChannelStatus()

    IDLE.whenIsActive {
      when(io.input(0) === False && io.input(8 downto 1) === B"000111110") {
        goto(READ_PRE_EMPHASIS)
      }
    }

    READ_PRE_EMPHASIS.whenIsActive {
      channelStatus.preEmphasis := io.input(10)
      goto(READ_SOURCE_NUMBER)
    }

    READ_SOURCE_NUMBER.whenIsActive {
      channelStatus.sourceNumber := io.input(15 downto 12)
      goto(READ_SAMPLE_RATE)
    }

    READ_SAMPLE_RATE.whenIsActive {
      channelStatus.sampleRate := io.input(18 downto 16)
      goto(READ_PROFESSIONAL)
    }

    READ_PROFESSIONAL.whenIsActive {
      channelStatus.professional := io.input(19)
      goto(READ_RESERVED1)
    }

    READ_RESERVED1.whenIsActive {
      channelStatus.reserved1 := io.input(22 downto 20)
      goto(READ_MODE)
    }

    READ_MODE.whenIsActive {
      channelStatus.mode := io.input(24 downto 23)
      goto(READ_RESERVED2)
    }

    READ_RESERVED2.whenIsActive {
      channelStatus.reserved2 := io.input(25)
      goto(READ_EMPHASIS)
    }

    READ_EMPHASIS.whenIsActive {
      channelStatus.emphasis := io.input(26)
      goto(READ_RESERVED3)
    }

    READ_RESERVED3.whenIsActive {
      channelStatus.reserved3 := io.input(31 downto 27)
      goto(READ_USER_BITS)
    }

    READ_USER_BITS.whenIsActive {
      channelStatus.userBits := io.input(47 downto 32)
      io.output := channelStatus
      goto(IDLE)
    }
  }
}





case class ChannelStatus(word0: Bits, word1: Bits, word2: Bits, word3: Bits) extends Bundle {
  val audioWordLength = word0(0, 4)
  val sampleRate      = word0(4, 4)
  val originality     = word0(8, 2)
  val channelMode     = word0(10, 2)
  val sourceFormat    = word0(12, 2)
  val audioCategory   = word0(14, 2)
  val sourceNumber    = word1(0, 4)
  val channelNumber   = word1(4, 4)
  val validity        = word1(8)
  val userBit         = word1(9)
  val channelStatus   = word1(10, 2)
  val emphasis        = word1(12)
  val samplingFreqExt = word1(13)
  val samplingFreq    = (word1(14) ## word1(15) ## word2(0, 7)).asUInt
  val reserved        = word2(7, 5)
  val channelLayout   = word2(12, 4)
  val multichannel    = word3(0)
  val lfe             = word3(1)
  val level           = word3(2, 6)
  val dynRangeType    = word3(8, 2)
  val dynRangeCode    = word3(10, 6)
  val roomType        = word3(16, 3)
}





case class ClockRecoveryPLL() extends Component {
  val io = new Bundle {
    val spdifClk = in(Bool)
    val pllClk = out(Bool)
  }

  val pllLocked = RegInit(False)

  val pllCtrl = new PLL()

  pllCtrl.io.refClk := io.spdifClk
  pllCtrl.io.resetn := True
  pllCtrl.io.locked := pllLocked

  io.pllClk := pllCtrl.io.outClk
  pllLocked := pllCtrl.io.locked
}

case class PLL() extends Component {
  val io = new Bundle {
    val refClk = in(Bool)
    val resetn = in(Bool)
    val locked = out(Bool)
    val outClk = out(Bool)
  }

  val pll = new PLLCore()

  pll.io.REFCLK := io.refClk
  pll.io.RSTB := io.resetn
  pll.io.CLKOUT0 := io.outClk

  io.locked := pll.io.LOCKED
}

case class PLLCore() extends BlackBox {
  val io = new Bundle {
    val REFCLK = in(Bool)
    val RSTB = in(Bool)
    val CLKOUT0 = out(Bool)
    val LOCKED = out(Bool)
  }
  setDefinitionName("pll_core")
}





case class SpdifClockRecoveryConfig(
  sampleRate: HertzNumber,
  channelStatusBits: Bits,
  pllConfig: PLLConfig
)

class SpdifClockRecovery(config: SpdifClockRecoveryConfig) extends Component {
  val io = new Bundle {
    val input = in(Bits(1))
    val output = out(Bool)
  }

  val pll = new PLL(config.pllConfig)
  pll.io.clk_in := io.input

  val targetPeriod = config.sampleRate.toPeriod
  val threshold = targetPeriod / 2

  val counter = Reg(UInt(log2Up(threshold.toInt).bitCount) init(0))
  val direction = Reg(Bool)

  when(counter < threshold) {
    counter := counter + 1
  } otherwise {
    counter := 0
    direction := !direction
  }

  io.output := direction

  pll.io.reset := False
  pll.io.locked := counter === 0
}





case class SpdifReceiver(
    dataIn: Bool,
    sclk: Bool,
    lrclk: Bool,
    reset: Bool
) extends Component {

  val clockRecovery = SpdifClockRecovery(dataIn, sclk)
  val channelStatusDecoder = ChannelStatusDecoder(clockRecovery.channelStatusBits)
  
  val sampleWidth = 24
  val channelCount = 8
  
  val audioBitsPerChannel = Reg(UInt(24 bits)) init(0)
  val audioBitCount = Reg(UInt(log2Up(sampleWidth) bits)) init(0)
  val audioChannelCount = Reg(UInt(log2Up(channelCount) bits)) init(0)
  val audioChannelIndex = Reg(UInt(log2Up(channelCount) bits)) init(0)
  
  val audioData = Vec(Reg(SInt(sampleWidth bits)) init(0), channelCount)
  val audioValid = RegInit(False)
  
  when(reset) {
    audioBitsPerChannel := 0
    audioBitCount := 0
    audioChannelCount := 0
    audioChannelIndex := 0
    audioData.foreach(_.clearAll())
    audioValid := False
  }
  .elsewhen(clockRecovery.frameValid && channelStatusDecoder.valid) {
    when(channelStatusDecoder.bits.sampleWidth > 0) {
      audioBitsPerChannel := channelStatusDecoder.bits.sampleWidth
    }
    when(channelStatusDecoder.bits.channelCount > 0) {
      audioChannelCount := channelStatusDecoder.bits.channelCount
    }
    when(clockRecovery.channelStatusChanged) {
      audioChannelIndex := 0
    }
    when(audioChannelIndex < audioChannelCount) {
      audioData(audioChannelIndex) := audioData(audioChannelIndex) |
        (SInt(clockRecovery.dataBits(audioBitCount, audioBitCount - 1)).resized |
         S(0, (sampleWidth - clockRecovery.channelStatusBits) bits)) << 
          (audioBitCount << 3)
    }
    audioBitCount := audioBitCount + 1
    when(audioBitCount === audioBitsPerChannel) {
      audioBitCount := 0
      audioChannelIndex := audioChannelIndex + 1
      when(audioChannelIndex === audioChannelCount) {
        audioValid := True
      }
    }
  }
  
  val io = new Bundle {
    val audio = out Vec(audioValid ## audioData, channelCount)
    val lrclk = out Bool
  }
  
  io.audio.foreach(_.forceWidth(sampleWidth))
  io.lrclk := lrclk
}







import spinal.core._
import spinal.lib._

case class SpdifReceiver(bitWidth: Int, channelCount: Int) extends Component {
  val io = new Bundle {
    val spdifIn = in Bool
    val pcmOut = out Vec(SInt(bitWidth bits), channelCount)
  }

  val divider = Reg(UInt(23 bits)) init (0)
  val preamble = Reg(Bits(3 bits)) init (0)
  val frameBit = Reg(Bool) init (False)
  val channelStatus = Reg(Bits(192 bits)) init (0)
  val audioData = Reg(Bits(32 bits)) init (0)

  val bitCounter = CounterFreeRun(2)
  val preambleCounter = CounterFreeRun(3)
  val dividerCounter = CounterFreeRun(24)

  when(preambleCounter.willOverflow) {
    preamble := (preamble ## io.spdifIn).resized
  }

  when(preamble === "110" && io.spdifIn === False) {
    preamble := 0
    frameBit := True
  }

  when(frameBit && dividerCounter.willOverflow) {
    divider := divider + 1
    dividerCounter.value := 0
  }

  when(dividerCounter.willOverflow) {
    bitCounter.increment()
  }

  when(bitCounter.willOverflow) {
    bitCounter.value := 0

    when(divider === 191) {
      channelStatus := (channelStatus ## io.spdifIn).resized
    }

    when(divider === 192) {
      audioData := (audioData ## io.spdifIn).resized

      when(audioData.msb) {
        io.pcmOut(0) := ((~audioData).asSInt - S(1 << bitWidth - 1)).resized
      } otherwise {
        io.pcmOut(0) := audioData.asSInt
      }

      for (i <- 1 until channelCount) {
        val channelData = (io.spdifIn ## channelStatus((i - 1) * 24 + 15 downto (i - 1) * 24) ## audioData).resized
        when(channelData.msb) {
          io.pcmOut(i) := ((~channelData).asSInt - S(1 << bitWidth - 1)).resized
        } otherwise {
          io.pcmOut(i) := channelData.asSInt
        }
      }

      channelStatus := 0
      audioData := 0
    }
  }
}







import spinal.core._
import spinal.lib._

case class SpdifReceiver(
  bitClock: Bool,
  data: Bool,
  reset: Bool,
  channelCount: Int,
  wordWidth: Int,
  sampleRate: HertzNumber,
  consumerMode: Boolean,
  debug: Boolean
) extends Component {

  val io = new Bundle {
    val output = out Vec(Analog(Bool), channelCount)
  }

  val frameSync = RegInit(False)
  val bitCounter = Counter(wordWidth)
  val channelStatusCounter = Counter(192)
  val channelStatusData = Reg(UInt(192 bits))
  val sampleCounter = Counter((sampleRate.toBigInt * 1000L).toInt)
  val sample = Reg(Bits(wordWidth bits))
  val frame = Reg(Vec(Bits(wordWidth bits), channelCount))
  val channelStatus = ChannelStatusDecoder(channelStatusData)

  val dataReg = RegNext(data)
  val bitClockReg = RegNext(bitClock)

  val pllLocked = SpdifClockRecovery(bitClockReg, dataReg, reset)

  val shiftReg = ShiftRegister(dataReg, 4)

  when(reset) {
    frameSync := False
    bitCounter.clear()
    channelStatusCounter.clear()
    channelStatusData := 0
    sampleCounter.clear()
    sample := 0
    frame.foreach(_ := 0)
  }.elsewhen(!pllLocked) {
    frameSync := False
    bitCounter.clear()
    channelStatusCounter.clear()
    channelStatusData := 0
    sampleCounter.clear()
    sample := 0
    frame.foreach(_ := 0)
  }.otherwise {
    when(!frameSync) {
      when(shiftReg === 0x06 && bitCounter.value === 0) {
        frameSync := True
        bitCounter.increment()
      }.otherwise {
        bitCounter.increment()
      }
    }.otherwise {
      when(bitCounter.willOverflow) {
        when(channelStatusCounter.value === 191) {
          channelStatusCounter.clear()
          channelStatusData := shiftReg.resized
          sampleCounter.increment()
          sample := shiftReg.resized
          for(i <- 0 until channelCount) {
            frame(i) := frame(i)(1, 0) ## sample
          }
        }.otherwise {
          channelStatusCounter.increment()
          channelStatusData := channelStatusData ## shiftReg.resized
        }
        bitCounter.clear()
      }.otherwise {
        bitCounter.increment()
      }
    }
  }

  io.output := frame
}
