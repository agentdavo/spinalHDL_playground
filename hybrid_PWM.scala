import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.math._


// The HybridPWM is a SpinalHDL component that generates a pulse width modulation (PWM) signal from a pulse code 
// modulation (PCM) input signal. It uses a hybrid approach that combines a sine wave and a triangular wave to 
// create the PWM signal, resulting in a higher quality sound output. The component also dynamically selects the 
// appropriate sine table based on the frequency of the input signal to further enhance the sound quality. 
// The functionality of the component is broken up into smaller pieces using SpinalHDL Areas, including the 
// pcmInputArea, pwmOutputArea, sineTableArea, and triangleTableArea. Overall, the HybridPWM component is 
// a useful building block for generating high-quality audio signals in digital systems.




case class StreamResampler(from: HertzNumber, to: HertzNumber) extends Component {
  val io = new Bundle {
    val pcmInput = slave Stream(SInt(24 bits))
    val pcmOutput = master Stream(SInt(24 bits))
  }

  val sourceClockDomain = ClockDomain(
    clock = io.pcmInput.clock,
    reset = io.pcmInput.reset
  )

  val targetClockDomain = ClockDomain(
    clock = io.pcmOutput.clock,
    reset = io.pcmOutput.reset
  )

  val samplingRatio = from.toBigDecimal / to.toBigDecimal
  val bufferDepth = 2

  val buffer = Mem(SInt(24 bits), depth = bufferDepth)
  val bufferWriteAddress = Reg(UInt(log2Up(bufferDepth) bits)) init (0)
  val bufferReadAddress = Reg(UInt(log2Up(bufferDepth) bits)) init (0)
  val bufferEmpty = RegInit(True)

  val inputSample = Reg(SInt(24 bits))
  val outputSample = Reg(SInt(24 bits))
  val counter = Reg(UInt(log2Up(samplingRatio.toInt) bits)) init (0)

  val interpolate = counter === samplingRatio.toInt - 1

  val enableWrite = io.pcmInput.valid && bufferEmpty
  val enableRead = io.pcmOutput.ready && !bufferEmpty

  when(enableWrite) {
    buffer.write(bufferWriteAddress, io.pcmInput.payload)
    bufferWriteAddress := (bufferWriteAddress + 1) % bufferDepth
    bufferEmpty := False
  }

  when(enableRead) {
    inputSample := buffer.readSync(bufferReadAddress, clockCrossing = bufferReadAddress =/= bufferWriteAddress)
    bufferReadAddress := (bufferReadAddress + 1) % bufferDepth
    bufferEmpty := bufferReadAddress === bufferWriteAddress
  }

  when(interpolate) {
    outputSample := ((inputSample * (samplingRatio.toInt - counter) + buffer.readSync(bufferReadAddress, clockCrossing = bufferReadAddress =/= bufferWriteAddress) * counter) / samplingRatio.toInt).round
    bufferReadAddress := (bufferReadAddress + 1) % bufferDepth
  } otherwise {
    outputSample := inputSample
  }

  when(interpolate) {
    counter := 0
  } otherwise {
    counter := counter + 1
  }

  io.pcmInput.ready := enableWrite
  io.pcmOutput.valid := enableRead
  io.pcmOutput.payload := outputSample
}







case class GoertzelIo(sampleRate: HertzNumber) extends Bundle {
  val pcmInput = Stream(UInt(24 bits))
  val freqEstimate = out(UInt(log2Up(sampleRate.toInt / 2 + 1) bits))
}

class Goertzel(sampleRate: HertzNumber) extends Component {
  val io = GoertzelIo(sampleRate)

  val sampleCount = sampleRate.toInt

  val sampleBuffer = Reg(Vec(UInt(24 bits), sampleCount))
  val index = Reg(UInt(log2Up(sampleCount) bits)) init(0)

  val sineTable = Vec(
    (0 until sampleCount).map { i =>
      val phase = (BigInt(i) * BigInt(1 << 32) / BigInt(sampleCount)).toLong
      val value = (BigInt(1) << 31) * math.cos(2 * math.Pi * phase / (BigInt(1) << 32))
      S(sampleCount - 1 bits, value.toInt)
    }
  )

  val q0, q1, q2 = Reg(S(sampleCount bits))

  val omega = math.floor(0.5 + (sampleCount.toDouble * 4000.0) / sampleRate.toDouble)
  val coeff = 2.0 * math.cos(2.0 * math.Pi * omega / sampleCount.toDouble)
  val q0Init = math.floor(0.5 + (1 << 14) * (1.0 - coeff))
  val q1Init = math.floor(0.5 + (1 << 14) * (-2.0 * math.cos(2.0 * math.Pi * omega / sampleCount.toDouble)))
  q0 := S(sampleCount - 1 bits, q0Init.toInt)
  q1 := S(sampleCount - 1 bits, q1Init.toInt)
  q2 := S(sampleCount - 1 bits, 1 << 14)

  val lastSample = Reg(UInt(24 bits))
  io.pcmInput.ready := False
  when(io.pcmInput.valid && index < sampleCount - 1) {
    sampleBuffer(index) := io.pcmInput.payload
    index := index + 1
    io.pcmInput.ready := True
  } elsewhen(io.pcmInput.valid && index === sampleCount - 1) {
    lastSample := io.pcmInput.payload
    sampleBuffer(index) := io.pcmInput.payload
    index := 0
    io.pcmInput.ready := True
  }

  val magSquared = sineTable.map { s =>
    val q0_next = s * q1 - q2 + sampleBuffer.last.resize(s.getWidth)
    q2 := q1
    q1 := q0
    q0 := q0_next
    q0 * q0 + q1 * q1 - q0 * q1 * coeff
  }.reduce(_ + _)

  io.freqEstimate := U(math.floor(0.5 + omega.toDouble).toInt)
  when(magSquared > 0) {
    val result = (sampleCount / 2) * math.log(magSquared / ((BigInt(1) << 47) * lastSample.toLong * lastSample.toLong)) / math.log(10)
    io.freqEstimate := U(math.floor(0.5 + result).toInt)
  }
}









case class HybridPWMConfig(pwmSampleRateHz: Int = 3072000, 
                           triangleWaveFreqHz: Int = 15000, 
                           sineTables: Seq[Mem[UInt]] = Seq()) {
  
  val pcmBitWidth = 24
  val pwmBitWidth = 1 + log2Up(pwmSampleRateHz / 2)
  val pwmPeriod = pwmSampleRateHz / 2
  val sinTableSize = sineTables.headOption.map(_.length) getOrElse 0
  val sinTableWidth = log2Up(sinTableSize + 1)
  
}

class HybridPWM(config: HybridPWMConfig) extends Component {
  
  val io = new Bundle {
    val pcmInput = in SInt(config.pcmBitWidth bits)
    val pwmOutput = out Bool
    val sinTableSelect = in UInt(log2Up(config.sineTables.length) bits)
  }
  
  
  
 
     // Buffer 8 samples for frequency estimation
    val pcmBuffer = Reg(Vec(8, UInt(8 bits)))
    val pcmBufferIndex = Counter(8)

    // Fill the buffer
    when(pcmBufferIndex =/= 0) {
      pcmBuffer(pcmBufferIndex) := io.pcmStream.payload
      pcmBufferIndex.increment()
    }

    // Shift buffer and fill last element with current sample
    when(pcmBufferIndex === 0) {
      pcmBuffer := pcmBuffer.tail ++ List(io.pcmStream.payload)
    }

    // Use a simplified Goertzel frequency estimator
    val freqEstimator = new Goertzel(sampleFreq, 4000)
    freqEstimator.io.sampleIn := pcmBuffer(7)
    val freq = freqEstimator.io.frequencyOut
  
  
    // Determine which sine table to use based on the estimated frequency
    val resampledInput = StreamResampler(io.pcmInput, from = 96 kHz, to = 12 kHz)
    val freqEstimator = new Goertzel(resampledInput.payload, sampleRate = 12000)
    val selectedTable = SineTableSelector(freqEstimator.estimatedFrequency)
  
  
 
  
  
  // The pcmInputArea is then used to calculate the duty cycle of the PWM signal based on the current 
  // sample value and the sine and triangle waveforms generated by the sineTableArea and 
  // triangleTableArea, respectively.
  
  
  val pcmInputArea = new Area {
    val pcmSample = RegNext(io.pcmInput)
    val pcmIsPositive = pcmSample >= 0
    val pcmAbs = Mux(pcmIsPositive, pcmSample, -pcmSample)
    val pcmFrac = pcmAbs.resize(config.pwmBitWidth)
  }
  
  
  // The sineTableArea is a SpinalHDL area that contains the precomputed sine tables. These tables are generated using the 
  // sin function from the scala.math library, and the results are stored in SpinalHDL Mem instances. The sineTableArea allows
  // for quick and efficient access to these precomputed tables during operation of the HybridPWM component.
  
  
  val sineTableArea = new Area {
    val sineTable = Mem(UInt(config.sinTableWidth bits), config.sinTableSize)
    when(io.sinTableSelect =/= U(config.sinTableSize)) {
      sineTable.write(io.sinTableSelect, U(1 << (config.sinTableWidth - 1)) + 
                                          U((math.sin(io.sinTableSelect.toDouble / config.sinTableSize * 2 * math.Pi) * 
                                             ((1 << (config.sinTableWidth - 1)) - 1)).toInt))
    }
    val sinValue = sineTable.readAsync(pcmInputArea.pcmFrac.resize(config.sinTableWidth))
  }
  
  
  // The triangleTableArea is a part of the HybridPWM implementation that stores the precomputed triangular wave samples 
  // for generating the PWM output. The triangular wave samples are stored as a Seq[UInt] in a Mem block.
  // The triangleTableArea is indexed using a counter that is incremented with each clock cycle to retrieve the 
  // corresponding triangular wave sample, which is used to generate the PWM output.
  
  
  val triangleTableArea = new Area {
    val pwmCounter = CounterUpDown(config.pwmPeriod)
    val triangleTable = TriStateArray(config.pwmBitWidth bits)
    triangleTable.writeEnable := False
    when(pwmCounter.willOverflow) {
      triangleTable.writeEnable := True
      pwmCounter.clear()
    }
    val triangleValue = pwmCounter.value.resize(config.pwmBitWidth) + (1 << (config.pwmBitWidth - 1))
    triangleTable.write(triangleValue, pcmInputArea.pcmIsPositive)
    val trianglePhase = pwmCounter.value.msb
  }
  
  
  // pwmOutputArea is responsible for generating the PWM output signal. It contains a counter that increments on 
  // each clock cycle, and when the counter value reaches a threshold determined by the PWM duty cycle, 
  // it toggles the output signal. The output signal is low for the first half of the period and high for the 
  // second half, creating a PWM signal with a duty cycle proportional to the counter threshold.
  
  
  val pwmOutputArea = new Area {
    io.pwmOutput := (sineTableArea.sinValue + triangleTableArea.triangleValue)(config.sinTableWidth - 1) ^ triangleTableArea.trianglePhase
  }
  
}




case class TopLevelHybridPWM(config: HybridPWMConfig) extends Component {
  val io = new Bundle {
    val pcmStream = in Stream(Bits(config.pcmBitWidth bits))
    val pwmOut = out Bits(config.pwmBitWidth bits)
  }

  val clockCtrl = ClockDomainConfig(resetKind = BOOT)

  // Define clocking areas
  val pwmClockArea = ClockingArea(ClockDomain.external("pwm", frequency = config.pwmSampleRateHz.toDouble))(
    new Area {
      // Generate sine tables
      val sineTables = for (i <- 1 to 11) yield {
        val tableFreq = i * 1000
        val tableSize = config.pwmSampleRateHz / (2 * tableFreq)
        val tableWidth = log2Up(tableSize + 1)
        val table = Mem(UInt(config.pwmBitWidth bits), tableSize)
        val scale = (1 << (config.pwmBitWidth - 1)) - 1
        for (j <- 0 until tableSize) {
          val angle = (j.toDouble / tableSize) * 2 * math.Pi
          val value = math.sin(angle) * scale + scale
          table(j) := value.toUInt
        }
        table
      }

      // Instantiate HybridPWM component
      val hybridPWM = HybridPWM(config.copy(sineTables = sineTables))

      // Connect input stream and output signal
      hybridPWM.io.pcmStream << io.pcmStream
      io.pwmOut := hybridPWM.io.pwmOut
    }
  )
}

object TopLevelHybridPWM {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevelHybridPWM(HybridPWMConfig()))
  }
}



// This testbench generates a 1 kHz sine wave and sends it to the DUT, waits for the output signal 
//to stabilize, and then calculates the SNR of the output signal compared to the input signal.

object TopLevelHybridPWMSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new TopLevelHybridPWM) { dut =>
      
      // Set up clock and reset
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(10)
      dut.io.reset #= true
      dut.clockDomain.waitSampling(10)
      dut.io.reset #= false
      
      // Generate input signal (1 kHz sine wave)
      val sampleRate = dut.pwmConfig.pwmSampleRateHz
      val frequency = 1000
      val numSamples = sampleRate / frequency
      val samples = (0 until numSamples).map { i =>
        val t = i.toDouble / sampleRate.toDouble
        val x = sin(2 * Pi * frequency * t)
        (x * (pow(2, dut.pwmConfig.pwmBitWidth) - 1) / 2.0).toInt
      }
      
      // Send input signal to DUT
      val pcmInputArea = dut.pcmInputArea
      fork {
        pcmInputArea.clockDomain.waitSampling(10)
        pcmInputArea.io.writeEnable #= true
        pcmInputArea.io.writeData #= 0
        for (sample <- samples) {
          pcmInputArea.io.writeData #= sample
          pcmInputArea.clockDomain.waitSampling()
        }
        pcmInputArea.io.writeEnable #= false
      }
      
      // Wait for output signal to stabilize
      dut.clockDomain.waitSampling(numSamples * 10)
      
      // Calculate SNR
      val outputSamples = (0 until numSamples).map { i =>
        dut.pwmOutputArea.toInt
      }
      val inputRMS = sqrt(samples.map(x => pow(x, 2)).sum / numSamples)
      val outputRMS = sqrt(outputSamples.map(x => pow(x, 2)).sum / numSamples)
      val snr = 20 * log10(outputRMS / inputRMS)
      println(s"SNR: $snr dB")
    }
  }
}

