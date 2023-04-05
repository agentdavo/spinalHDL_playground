import spinal.core._
import spinal.lib._

import spinal.core.sim._
import spinal.lib.sim._



// MultiFeedbackDeltaSigmaModulator
// a multi-feedback topology delta-sigma modulator with multiple feedback loops to shape the noise in the quantization error.

class MultiFeedbackDeltaSigmaModulator(inputWidth: Int, outputWidth: Int, osr: Int) extends Component {
  val io = new Bundle {
    val input = in SInt(inputWidth bits)
    val output = out SInt(outputWidth bits)
  }

  // Integrator with a configurable feedback gain
  def integratorWithFeedbackGain(input: SInt, feedbackGain: SInt, outputWidth: Int): (SInt, SInt) = {
    val integrator = Reg(SInt(outputWidth bits)) init(0)
    val feedback = Reg(SInt(outputWidth bits)) init(0)
    val error = input - feedback
    integrator := (integrator + error) >> 1
    feedback := (feedback + (integrator * feedbackGain)) >> 1
    (integrator, feedback)
  }

  // Multi-feedback topology delta-sigma modulator
  val (integrator1, feedback1) = integratorWithFeedbackGain(io.input, SInt(osr, outputWidth bits), outputWidth)
  val (integrator2, feedback2) = integratorWithFeedbackGain(io.input, SInt(osr / 2, outputWidth bits), outputWidth)
  val (integrator3, feedback3) = integratorWithFeedbackGain(io.input, SInt(osr / 3, outputWidth bits), outputWidth)

  io.output := integrator1 + integrator2 + integrator3
}





// SingleBitQuantizer
// a simple algorithm that takes a real-valued input signal and outputs a single-bit signal, based on whether the input is 
// greater than or less than zero. Specifically, if the input is greater than zero, the output is 1, and if the input is 
// less than or equal to zero, the output is 0. This is a form of "quantization", where the continuous input signal 
// is "quantized" to a digital output signal with a single bit of resolution.

object SingleBitQuantizer {
  def apply(input: UInt, threshold: UInt): Bool = {
    input > threshold
  }
}




// The DigitalNoiseShaperFilter 
// a noise shaping algorithm to shape the quantization noise in a way that reduces its perceptual impact on the 
// audio signal. Specifically, it uses a high-order IIR filter to shape the noise spectrum, which is then added 
// to the audio signal before quantization. This results in a reduced noise floor at lower frequencies and an 
// increased noise floor at higher frequencies, which is less perceptually significant to human ears.

class DigitalNoiseShaperFilter(inputWidth: Int, order: Int) extends Component {
	
  val io = new Bundle {
    val input = in(SInt(inputWidth bits))
    val output = out(SInt(inputWidth bits))
  }

  val delayLine = List.fill(order)(Reg(SInt(inputWidth bits)) init(0))
  val taps = List.fill(order)(Reg(SInt(inputWidth bits)) init(0))
  val sum = taps.reduce(_ + _)
  val quantizationError = io.input - sum

  io.output := delayLine.head.resized + quantizationError
  for (i <- 0 until order - 1) {
    taps(i) := delayLine(i) - delayLine(i + 1)
  }
  taps.last := quantizationError
  delayLine.head := io.output
  for (i <- 1 until order) {
    delayLine(i) := delayLine(i - 1)
  }
}




// PulseDensityScaler
// scales the pulse density of a single-bit PDM signal. In other words, it takes a high-frequency PDM signal with a 
// variable pulse density and converts it to a lower-frequency PDM signal with a fixed pulse density. The fixed pulse 
// density is determined by the oversampling ratio of the PDM signal and the desired output clock frequency.
// The pulse density scaling is performed by counting the number of 1s in a sliding window of the PDM signal and 
// adjusting the output pulse density accordingly.

class PulseDensityScaler(osr: Int, pdmFrequency: HertzNumber) extends Component {
  val io = new Bundle {
    val input = in Bool
    val output = out Bool
  }

  val pulseCounter = Reg(UInt(log2Up(osr) bits)) init(0)
  val increment = Reg(UInt(log2Up(osr) bits)) init(0)

  increment := 0
  when (pulseCounter === 0) {
    increment := osr - 1
  } .elsewhen (pulseCounter === increment) {
    increment := 0
  }

  pulseCounter := pulseCounter + increment
  io.output := pulseCounter < increment

  val clockDivider = new ClockDivider {
    val io = new Bundle {
      val input = in(Bool)
      val output = out(Bool)
    }
    io.output := io.input
  }

  clockDivider.io.input := io.output
  clockDivider.setConfig(ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW, clockEdge = RISING))

  // By subtracting the jitter from the pulse width, the pulse is made slightly narrower to compensate for the 
  // expected jitter, ensuring that the output signal has a more consistent pulse width.
  
  val period = (1 second) / pdmFrequency
  val pulseWidth = period * increment.toDouble / osr
  val jitter = 0.1 * pulseWidth
  clockDivider.addPulse(pulseWidth - jitter)
}





class PdmModulator(pcmFrequency: HertzNumber,
		   dsmFrequency: HertzNumber,
	           pdmOuptutFrequency: HertzNumber,
                   inputWidth: Int) extends Component {
	
  val io = new Bundle {
    val pcmInput = in Bits(inputWidth bits)
    val pdmOutput = out Bool
  }

  val dsmOsr = dsmFrequency / pcmFrequency
  val pdmOsr = pdmOuptutFrequency / dsmFrequency

  // upsample input to dsmFrequency
  val modulator = new MultiFeedbackDeltaSigmaModulator(inputWidth, inputWidth, dsmOsr)
  modulator.io.input := io.input

  // quantize to single bits
  val quantizedOutput = new SingleBitQuantizer(modulator.io.output, 1)

  // quantize to single bits
  val noiseShaper = new DigitalNoiseShaperFilter(inputWidth, 1)
  noiseShaper.io.input := modulator.io.quantizationError

  // quantize to single bits
  val shapedOutput = noiseShaper.output ^ quantizedOutput

  // downsample shapedOutput to pdmOuptutFrequency
  val pulseDensityScaler = new PulseDensityScaler(pdmOsr, pdmOuptutFrequency)
  pulseDensityScaler.io.input :=
	
  io.pdmOutput := pulseDensityScaler.output
	
}




// This test uses an external clock domain that is synchronized to the PDM output of the DUT.
// It generates a multi-tone signal with three frequencies and applies it to the PCM input of the PDM modulator.
// The output PDM signal is buffered using a BufferedOutPort and compared to the reference implementation.
// The test then calculates and prints the SNR and THD of the PDM output.


object PdmModulatorTestbench extends App {
  // Define test parameters
  val pcmFreq = 48000 Hz
  val pdmFreq = 3072000 Hz
  val convFreq = 98304000 Hz
  val osr = convFreq / pcmFreq
  val numSamples = 48000
  val amplitude = 0.5

  // Create test signals
  val inputSignal = Seq.tabulate(numSamples) { i =>
    val t = i.toDouble / pcmFreq.toDouble
    val f1 = 440 Hz
    val f2 = 880 Hz
    val f3 = 1320 Hz
    amplitude * (Math.sin(2 * Math.PI * f1 * t) + Math.sin(2 * Math.PI * f2 * t) + Math.sin(2 * Math.PI * f3 * t)).toFloat
  }

  // Convert input signal to PDM using reference implementation
  val pdmRef = Seq.tabulate(numSamples) { i =>
    val pdmSample = Seq.tabulate(osr.toInt) { j =>
      if (inputSignal(i) > 0) 1 else 0
    }
    pdmSample.reduce(_ + _).toDouble / osr.toDouble
  }

  // Create test components
  val pdmModulator = new PdmModulator(pdmFreq, convFreq, pcmFreq)
  val clockDomain = ClockDomain.external(pdmModulator.io.pdmOutput)
  val pcmBuffer = BufferedOutPort(Bits(24 bits), clockDomain)

  // Simulate the test components
  SimConfig.withWave.doSim(pdmModulator) { dut =>
    clockDomain.forkStimulus(period = 20)
    clockDomain.waitSampling()

    // Generate and apply input signal
    var i = 0
    while (i < numSamples) {
      dut.io.pcmInput #= (inputSignal(i) * ((1 << 23) - 1)).toInt
      pcmBuffer.enqueue(dut.io.pdmOutput.toBits)
      i += 1
      clockDomain.waitSampling()
    }

    // Compare output against reference implementation
    val pdmSim = pcmBuffer.dequeue(numSamples).map(_.toBoolean).toSeq
    val pdmSNR = SNR(pdmRef, pdmSim, pdmFreq / pcmFreq)
    val pdmTHD = THD(pdmRef, pdmSim, pdmFreq / pcmFreq)

    // Print results
    println(f"PDM SNR: ${pdmSNR}%.2fdB")
    println(f"PDM THD: ${pdmTHD * 100}%.4f%%")
    
    //  These functions take two sequences of Booleans (representing the reference and simulated PDM signals), as well as 
    // a decimation factor, and calculate the SNR and THD metrics as described in the testbench. The firFilter function 
    // is a simple implementation of a finite impulse response (FIR) filter, used for low-pass filtering the signals 
    // before calculating the SNR and THD.
    
		// Calculate the signal-to-noise ratio (SNR) in dB
		def SNR(ref: Seq[Bool], sig: Seq[Bool], decimation: Int): Double = {
		  val refDouble = ref.map(if (_) 1.0 else -1.0).toArray
		  val sigDouble = sig.map(if (_) 1.0 else -1.0).toArray
		  val refFiltered = firFilter(refDouble, Seq(1.0 / decimation))
		  val sigFiltered = firFilter(sigDouble, Seq(1.0 / decimation))
		  val noise = refFiltered.zip(sigFiltered).map(p => p._1 - p._2)
		  val signalPower = sigFiltered.map(x => x * x).sum
		  val noisePower = noise.map(x => x * x).sum
		  10 * math.log10(signalPower / noisePower)
		}

		// Calculate the total harmonic distortion (THD) in percent
		def THD(ref: Seq[Bool], sig: Seq[Bool], decimation: Int): Double = {
		  val refDouble = ref.map(if (_) 1.0 else -1.0).toArray
		  val sigDouble = sig.map(if (_) 1.0 else -1.0).toArray
		  val refFiltered = firFilter(refDouble, Seq(1.0 / decimation))
		  val sigFiltered = firFilter(sigDouble, Seq(1.0 / decimation))
		  val error = refFiltered.zip(sigFiltered).map(p => p._1 - p._2)
		  val totalPower = sigFiltered.map(x => x * x).sum
		  val harmonicPower = (2 to 20).map { n =>
			val freq = n * pcmFreq / (sig.size * decimation)
			val coeffs = Seq.fill(n)(1.0 / n)
			val filter = firFilter(refDouble, coeffs)
			filter.zip(sigDouble).map(p => p._1 * p._2).map(x => x * x).sum
		  }.sum
		  100 * math.sqrt(harmonicPower) / math.sqrt(totalPower)
		}

		// FIR filter implementation
		def firFilter(data: Seq[Double], coeffs: Seq[Double]): Seq[Double] = {
		  val n = coeffs.size
		  val buffer = Seq.fill(n)(0.0)
		  def filterElement(buffer: Seq[Double], coeffs: Seq[Double], data: Double): (Double, Seq[Double]) = {
			val result = (0 until n).map(i => buffer(i) * coeffs(i)).sum + data * coeffs(n)
			val newBuffer = buffer.tail ++ Seq(data)
			(result, newBuffer)
		  }
		  data.foldLeft((Seq[Double](), buffer)) { case ((result, buffer), data) =>
			val (elem, newBuffer) = filterElement(buffer, coeffs, data)
			(result :+ elem, newBuffer)
		  }._1
		}

    
  }
}
