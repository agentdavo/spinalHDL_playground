import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import javax.sound.sampled._

import spinal.core._
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer
import scala.math._


  // Define the coefficients for the Lagrange interpolator
  // 6-point, 5th-order optimal 4x z-form implementation
  // 149dB SNR 
  // See "Polynomial Interpolators for High-Quality Resampling of Oversampled Audio" by Olli Niemitalo

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




case class OversamplingFIRConfig(
  inputWidth: Int = 24,
  outputWidth: Int = 24,
  shiftRegisterSize: Int = 6
)




class OversamplingFIR(config: OversamplingFIRConfig) extends Component {
  val io = new Bundle {
    val pcmInput = in Bits(config.inputWidth bits)
    val pcmOutput = out Bits(config.outputWidth bits)
  }

  val outputStream = Stream(Fragment(Bits(config.inputWidth bits)))
  val outputOversampledRatio = 4

  outputStream.valid := Delay(io.pcmInput, config.shiftRegisterSize - 1).willBeRemoved()
  outputStream.payload := Delay(io.pcmInput, config.shiftRegisterSize - 1)

  val shiftRegister = RegInit(Bits(config.inputWidth bits), 0)
  shiftRegister := io.pcmInput ## shiftRegister(config.inputWidth - 1 downto 1)

  val input = io.inputStream.payload

  val interpolatedValue = LagrangeInterpolationOptimalZForm4x6pt5thOrder.interpolate(2.0, Seq(
    shiftRegister((config.shiftRegisterSize - 1)),
    shiftRegister((config.shiftRegisterSize - 1) - 1),
    shiftRegister((config.shiftRegisterSize - 1) - 2),
    shiftRegister((config.shiftRegisterSize - 1) - 3),
    shiftRegister((config.shiftRegisterSize - 1) - 4),
    shiftRegister((config.shiftRegisterSize - 1) - 5)
  ))

  when(outputStream.fire) {
    io.pcmOutput := interpolatedValue.resized
  }
}




object OversamplingFIRSim {
  def main(args: Array[String]): Unit = {
    val simConfig = SimConfig.withWave
    simConfig.compile(new OversamplingFIR(OversamplingFIRConfig())).doSim { dut =>
      val sampleRate = 96000
      val numSamples = 1000
      val numTones = 32
      val minFreq = 16.0
      val maxFreq = 20000.0

      // Generate the 32-tone input signal
      def generateToneSignal(freq: Double, sampleRate: Double, numSamples: Int, scalingFactor: Double): Seq[Long] = {
        val omega = 2 * Pi * freq / sampleRate
        (0 until numSamples).map { n => (sin(omega * n) * ((1 << (dut.config.inputWidth - 1)) - 1) * scalingFactor).toLong }
      }

      val freqs = Array.tabulate(numTones) { i =>
        minFreq * pow(maxFreq / minFreq, i.toDouble / (numTones - 1))
      }

      // Calculate the scaling factor for 1 dB less than the max amplitude
      val scalingFactor = pow(10, -1 / 20.0)

      val inputSignal = freqs.map(freq => generateToneSignal(freq, sampleRate, numSamples, scalingFactor)).reduce(_ zip _ map { case (x, y) => x + y })

      // Save the input signal to a WAV file
      val inputBytes = inputSignal.flatMap(_.toBinaryString.reverse.padTo(dut.config.inputWidth, '0')).grouped(8).map(x => Integer.parseInt(x.reverse, 2).toByte).toArray
      val inputOut = new ByteArrayOutputStream()
      val inputFormat = new AudioFormat(sampleRate.toFloat, dut.config.inputWidth, 1, true, false)
      val inputInputStream = new AudioInputStream(new ByteArrayInputStream(inputBytes), inputFormat, inputBytes.length / (dut.config.inputWidth / 8))
      AudioSystem.write(inputInputStream, AudioFileFormat.Type.WAVE, new File("input_signal.wav"))

      // Process the input signal through the OversamplingFIR
      val outputSignal = ArrayBuffer[Long]()
      for (n <- 0 until numSamples) {
        dut.io.pcmInput #= inputSignal(n)
        dut.clockDomain.waitSampling(dut.config.outputOversampledRatio)

        if (n % dut.config.outputOversampledRatio == 0) {
          outputSignal += dut.io.pcmOutput.toLong
        }
      }

      // Save the output signal to a WAV file
      val outputBytes = outputSignal.flatMap(_.toBinaryString.reverse.padTo(dut.config.outputWidth, '0')).grouped(8).map(x => Integer.parseInt(x.reverse, 2).toByte).toArray
      val outputOut = new ByteArrayOutputStream()
      val outputFormat = new AudioFormat(sampleRate.toFloat * dut.config.outputOversampledRatio, dut.config.outputWidth, 1, true, false)
      val outputInputStream = new AudioInputStream(new ByteArrayInputStream(outputBytes), outputFormat, outputBytes.length / (dut.config.outputWidth / 8))
      AudioSystem.write(outputInputStream, AudioFileFormat.Type.WAVE, new File("output_signal.wav"))

      println("Input and output signals saved to input_signal.wav and output_signal.wav")
    }
  }
}
