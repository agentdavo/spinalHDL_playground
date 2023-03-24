import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.complex._

//
// Step 1 - UpSampler4x
//
// 24 bits PCM at 96kHz is passed through a optimal z-form 4x 6pt 5th-order method described 
// in "Polynomial Interpolators for High-Quality Resampling of Oversampled Audio" by Olli Niemitalo.
// to a sampling rate of fs = 384 kHz at 8 bits.

//
// Step 2 - PredistortionFilter
//
// the signal is passed through the PredistortionFilter module to reduce distortion

//
// Step 3 - QNSFilter
//
// the signal is passed through the QNSFilter module to perform frequency-domain quantization 

//
// Step 4 - DoubleSidedPWM
//
// module to generate the double sided PWM output signal
//




object LagrangeInterpolationOptimalZForm4x6pt5thOrder {

  val coefficients = Seq[Double](
    0.07571827673995030, -0.87079480370960549, 0.186883718356452901, 1.09174419992174300, 0.03401038103941584,
    0.39809419102537769, 0.41706012247048818, -0.40535151498252686, -0.62917625718809478, -0.05090907029392906,
    0.02618753167558019, 0.12392296259397995, 0.21846781431808182, 0.15915674384870970, 0.01689861603514873
  )

  def interpolate(x: Double, y: Seq[Double]): Double = {
    val even1 = y(1) + y(0)
    val odd1 = y(1) - y(0)
    val even2 = y(2) + y(1)
    val odd2 = y(2) - y(1)
    val even3 = y(3) + y(2)
    val odd3 = y(3) - y(2)
    val even4 = y(4) + y(3)
    val odd4 = y(4) - y(3)
    val even5 = y(5) + y(4)
    val odd5 = y(5) - y(4)

    val c0 = even1 * coefficients(0) + even2 * coefficients(1) + even3 * coefficients(2)
    val c1 = odd1 * coefficients(3) + odd2 * coefficients(4) + odd3 * coefficients(5)
    val c2 = even1 * coefficients(6) + even2 * coefficients(7) + even3 * coefficients(8)
    val c3 = odd1 * coefficients(9) + odd2 * coefficients(10) + odd3 * coefficients(11)
    val c4 = even1 * coefficients(12) + even2 * coefficients(13) + even3 * coefficients(14)
    val c5 = odd1 * coefficients(15) + odd2 * coefficients(16) + odd3 * coefficients(17)

    (((((c5 * (x - 2.5) + c4) * (x - 1.5) + c3) * (x - 0.5) + c2) * (x + 0.5) + c1) * (x + 1.5) + c0) * (x + 2.5)
  }
}




case class UpSampler4xConfig(inputWidth: Int, outputWidth: Int) extends Bundle

class UpSampler4x(config: UpSampler4xConfig) extends Component {
  val io = new Bundle {
    val input = in SInt (config.inputWidth bits)
    val output = out SInt (config.outputWidth bits)
  }

  val clockDomain = ClockDomain.current
  val ratio = 4

  val maxDelta = 2 * ratio - 1
  val latency = 2 * ratio + 1

  val delayLine = Mem(SInt(config.inputWidth bits), ratio)
  val deltaAccumulator = Reg(SInt(log2Up(maxDelta) + 1 bits)) init(0)

  when(deltaAccumulator < (maxDelta / 2).asSInt) {
    deltaAccumulator := deltaAccumulator + ratio.asSInt
  } otherwise {
    deltaAccumulator := deltaAccumulator - (ratio - 1).asSInt
  }

  val coef = LagrangeInterpolationOptimalZForm4x6pt5thOrder.coefficients
  val indices = (0 until 6).map(_ * ratio)
  val samples = indices.map(i => delayLine.read(deltaAccumulator + i.asSInt))

  io.output := LagrangeInterpolationOptimalZForm4x6pt5thOrder.interpolate(deltaAccumulator.asDouble / ratio, samples.map(_.toDouble)).toInt

  when(clockDomain.isResetActive) {
    deltaAccumulator := 0
  } otherwise {
    delayLine.write(deltaAccumulator + ((ratio - 1) / 2).asSInt, io.input)
  }
}




case class FFTConfig(
  fftSize: Int = 512,
  dataWidth: Int = 24,
  phaseWidth: Int = 16
) extends Bundle {
  val twiddles = Vec(ComplexNumber(dataWidth bits, phaseWidth bits), fftSize/2)
  val butterflyTables = Vec(Vec(SInt(dataWidth bits), fftSize/2), log2Up(fftSize))
  val reversedIndexTable = Vec(UInt(log2Up(fftSize) bits), fftSize)
  def getTwiddle(n: Int): ComplexNumber = twiddles(n)
}




class FFT(config: FFTConfig) extends Component {
  val io = new Bundle {
    val in = Vec(ComplexNumber(config.dataWidth bits, config.phaseWidth bits), config.fftSize)
    val out = Vec(ComplexNumber(config.dataWidth bits, config.phaseWidth bits), config.fftSize)
  }

  val stages = log2Up(config.fftSize)

  val butterflyData = Reg(Vec(Vec(ComplexNumber(config.dataWidth bits, config.phaseWidth bits), config.fftSize / 2), stages))
  val butterflyTwiddles = Vec(Vec(ComplexNumber(config.dataWidth bits, config.phaseWidth bits), config.fftSize / 2), stages)
  val reversedIndex = Vec(UInt(stages bits), config.fftSize)

  reversedIndex(0) := 0
  for (i <- 1 until stages) {
    reversedIndex(i) := (reversedIndex(i - 1) | (1 << (i - 1))).resize(stages bits)
  }

  for (i <- 0 until config.fftSize / 2) {
    val twiddle = config.getTwiddle(i)
    butterflyTwiddles(0)(i) := twiddle
    butterflyTwiddles(1)(i) := twiddle
    butterflyData(0)(i) := io.in(i) + io.in(i + config.fftSize / 2)
    butterflyData(0)(i + config.fftSize / 2) := (io.in(i) - io.in(i + config.fftSize / 2)) * twiddle
  }

  for (stage <- 1 until stages) {
    val halfSize = 1 << stage
    for (i <- 0 until config.fftSize by halfSize) {
      for (j <- 0 until halfSize / 2) {
        val index = reversedIndex(stage - 1) | (i + j)
        val twiddle = butterflyTwiddles(stage)(j)

        butterflyData(stage)(i + j) := butterflyData(stage - 1)(index) + butterflyData(stage - 1)(index + halfSize / 2)
        butterflyData(stage)(i + j + halfSize / 2) := (butterflyData(stage - 1)(index) - butterflyData(stage - 1)(index + halfSize / 2)) * twiddle
      }
    }

    butterflyTwiddles(stage + 1) := butterflyTwiddles(stage)
  }

  io.out := butterflyData.last
}




case class IFFT(fftConfig: FFTConfig) extends Component {

  val io = new Bundle {
    val in = in Vec(ComplexNumber(fftConfig.dataWidth bits, fftConfig.phaseWidth bits), fftConfig.fftSize)
    val out = out Vec(ComplexNumber(fftConfig.dataWidth bits, fftConfig.phaseWidth bits), fftConfig.fftSize)
    val enable = in Bool()
  }

  // Constants
  val stages = log2Up(fftConfig.fftSize)
  val stageWidths = List.tabulate(stages)(n => fftConfig.fftSize / (1 << n + 1))
  val butterflyWidths = List.tabulate(stages)(n => 1 << n)

  // Internal signals
  val x = Seq.fill(stages + 1)(Reg(Vec(ComplexNumber(fftConfig.dataWidth bits, fftConfig.phaseWidth bits), fftConfig.fftSize / 2)))
  val s = Seq.fill(stages + 1)(Reg(Vec(ComplexNumber(fftConfig.dataWidth bits, fftConfig.phaseWidth bits), fftConfig.fftSize / 2)))
  val butterflyData = Seq.fill(stages + 1)(Reg(Vec(ComplexNumber(fftConfig.dataWidth bits, fftConfig.phaseWidth bits), fftConfig.fftSize / 2)))
  val butterflyOutput = Seq.fill(stages)(Reg(Vec(ComplexNumber(fftConfig.dataWidth bits, fftConfig.phaseWidth bits), fftConfig.fftSize)))

  // Index reverse logic
  val reversedIndexTable = Mem(UInt(log2Up(fftConfig.fftSize) bits), init = Vec((0 until fftConfig.fftSize).map(i => i.toBinaryString.reverse.padTo(log2Up(fftConfig.fftSize), '0').reverse).map(B(_, radix = 2))))
  val reversedIndex = reversedIndexTable.readSync(io.enable, io.in.map(c => reversedIndexTable.readAsync(c.toUInt)))

  // Stage logic
  for (stage <- 0 until stages) {
    for (i <- 0 until stageWidths(stage)) {
      x(stage)(i) := io.in(reversedIndex(i + stageWidths(stage))).conjugate
      s(stage + 1)(i) := x(stage)(i) + s(stage)(i)
    }

    for (i <- 0 until butterflyWidths(stage)) {
      val butterflyIndex = i << stage + 1
      butterflyData(stage)(i) := s(stage + 1)(i) + io.in(reversedIndex(butterflyIndex + stageWidths(stage))).conjugate

      val twiddle = fftConfig.getTwiddle(i << stages - stage - 1)
      butterflyOutput(stage)(butterflyIndex) := s(stage)(i) + butterflyData(stage)(i) * twiddle
      butterflyOutput(stage)(butterflyIndex + (1 << stage)) := s(stage)(i) - butterflyData(stage)(i) * twiddle
    }
  }

  // Output logic
  io.out := Vec(butterflyOutput.last) |+| Vec(butterflyOutput.init.map(butterflyOutput => butterflyOutput.rotate(fftConfig.fftSize / 2)))
}




case class PredistortionFilterConfig(
    coeffs: Seq[Double],
    dataWidth: Int,
    coeffsWidth: Int,
    inputSamplingRate: Double,
    outputSamplingRate: Double
)

class PredistortionFilter(config: PredistortionFilterConfig) extends Component {
  val io = new Bundle {
    val input = in(SInt(config.dataWidth bits))
    val output = out(SInt(config.dataWidth bits))
  }

  val coeffs = Vec(config.coeffs.map(c => SFix(c, -config.coeffsWidth exp)))
  val integrator = Reg(SFix(0, -config.coeffsWidth exp))

  val integratorOutput = integrator + io.input.asSFix / SFix(config.inputSamplingRate / config.outputSamplingRate, -config.coeffsWidth exp)
  integrator := integratorOutput - coeffs(0) * io.output.asSFix

  io.output := coeffs.drop(1).zipWithIndex.foldLeft(integratorOutput)((acc, ci) => {
    val (c, i) = ci
    val delayedInput = Delay(io.input, i + 1)
    acc - c * delayedInput.asSFix
  }).asSInt
}




case class QNSFilterConfig(
  fftSize: Int = 512,
  signalWidth: Int = 24,
  outputWidth: Int = 24,
  fftConfig: FFTConfig = FFTConfig(fftSize, false)
)

class QNSFilter(config: QNSFilterConfig) extends Component {
  val io = new Bundle {
    val in = in Vec(SInt(config.inputWidth bits), config.fftSize)
    val out = out Vec(SInt(config.outputWidth bits), config.fftSize)
  }

  // FFT
  val fft = new FFT(config.fftConfig)
  fft.io.in := io.in

  // Frequency-domain quantization noise shaping
  val fftReal = fft.io.out.map(_.real.resize(config.outputWidth bits))
  val fftImag = fft.io.out.map(_.imag.resize(config.outputWidth bits))
  val fftMag = fftReal.zip(fftImag).map { case (re, im) => re * re + im * im }
  val coeffs = Vec(QFixed(2 exp, -14 exp).toFloat, Seq(1, -2, 1))
  val quantErr = (fftMag, coeffs).zipped.foldLeft(B(0, 1 bits)) { case (sum, (mag, coeff)) =>
    val quantized = (mag >> 1) + (mag & 1)
    val error = quantized - coeff * sum
    error.resize(sum.getWidth) + sum
  }

  // IFFT
  val ifft = new IFFT(config.fftConfig)
  ifft.io.in.real := (fftReal, quantErr).zipped.map(_ - _)
  ifft.io.in.imag := fftImag.map(_ => 0)
  io.out := ifft.io.out.real
}






case class DoubleSidedPWMConfig(
  signalWidth: Int,
  quantWidth: Int,
  pwmWidth: Int
)

case class DoubleSidedPWM(config: DoubleSidedPWMConfig) extends Component {
  val io = new Bundle {
    val in = in(SInt(config.signalWidth bits))
    val out = out(SInt(config.signalWidth bits))
  }

  val quantized = io.in >> (config.signalWidth - config.quantWidth)
  val pwmIn = SInt(config.signalWidth bits)
  val pwmOut = UInt(config.pwmWidth bits)

  // Double-sided PWM calculation
  when (quantized > 0) {
    pwmOut := quantized.asUInt
  } otherwise {
    pwmOut := (-quantized).asUInt
  }

  pwmIn := pwmOut.asSInt.resize(config.signalWidth) << (config.signalWidth - config.pwmWidth)
  io.out := pwmIn
}




case class TopLevelConfig(
  upSampler4xConfig: UpSampler4xConfig = UpSamplerConfig(
    inputWidth = 24,
    outputWidth = 8,),
  predistortionConfig: PredistortionFilterConfig = PredistortionFilterConfig(
    inputWidth = 24,
    outputWidth = 24,
    predistortionCoeffs = Seq(
      -0.0001, 0.0002, -0.0002, 0.0004, -0.0004, 0.0008, -0.0009, 0.0014,
      -0.0016, 0.0025, -0.003, 0.0044, -0.0053, 0.0074, -0.0091, 0.0124
    )
  )
  fftConfig: FFTConfig = FFTConfig(),
  qnsConfig: QNSFilterConfig = QNSFilterConfig(),
  pwmConfig: DoubleSidedPwmConfig = DoubleSidedPwmConfig(),
)




class TopLevel(config: TopLevelConfig) extends Component {
  val io = new Bundle {
    val pcmIn = in Bits(config.dataWidth bits)
    val pwmOut = out Bits(config.pwmWidth bits)
  }
  
  // Instantiate UpSampler4x
  val upSampler = new UpSampler4x(config.upSampler4xConfig)

  // Instantiate PredistortionFilter
  val predistortionFilter = new PredistortionFilter(config.predistortionCoeffs)

  // Instantiate QNSFilter
  val qnsFilter = new QNSFilter(config.qnsConfig)

  // Instantiate DoubleSidedPwmM
  val modulator = new DoubleSidedPwm(config.modulatorConfig)

  // Connect input to UpSampler4x
  upSampler.io.input := io.pcmIn
  
  // Connect UpSampler to PredistortionFilter
  predistortionFilter.io.signalIn := upSampler.io.output

  // Connect output of PredistortionFilter to QNSFilter
  qnsFilter.io.signalIn := predistortionFilter.io.signalOut

  // Connect output of QNSFilter to DoubleSidedPwm
  modulator.io.pwmIn := qnsFilter.io.signalOut

  // Connect output of DoubleSidedPwm to output
  io.pwmOut := modulator.io.pwmOut
}

object TopLevel {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateVerilog(TopLevel())
  }
}
