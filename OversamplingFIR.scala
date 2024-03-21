import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import javax.sound.sampled._

import spinal.core._
import spinal.core.sim._
import spinal.lib.pipeline.{Pipeline, Stage, Connection}
import scala.math._

import scala.collection.mutable.ArrayBuffer

// Define the coefficients for the Lagrange interpolator
// 6-point, 5th-order optimal 4x z-form implementation
// 149dB SNR
// See "Polynomial Interpolators for High-Quality Resampling of Oversampled Audio" by Olli Niemitalo

object LagIntOpt {
  val coeffs = Seq[SFix](
    SFix( 0.07571827673995030,  4 exp),
    SFix(-0.87079480370960549,  4 exp),
    SFix( 0.18688371835645290,  4 exp),
    SFix( 1.09174419992174300,  4 exp),
    SFix( 0.03401038103941584,  4 exp),
    SFix( 0.39809419102537769,  4 exp),
    SFix( 0.41706012247048818,  4 exp),
    SFix(-0.40535151498252686,  4 exp),
    SFix(-0.62917625718809478,  4 exp),
    SFix(-0.05090907029392906,  4 exp),
    SFix( 0.02618753167558019,  4 exp),
    SFix( 0.12392296259397995,  4 exp),
    SFix( 0.21846781431808182,  4 exp),
    SFix( 0.15915674384870970,  4 exp),
    SFix( 0.01689861603514873,  4 exp)
  )
}

class Interpolator extends Component {
  
  val io = new Bundle {
    val x = in SFix (4 exp, 32 bits)
    val y = in Vec (SFix(4 exp, 32 bits), 6)
    val result = out SFix (4 exp, 32 bits)
  }

  val pipeline = Pipeline(
    List(
      new StageEO,
      new StageCoeffMultAdd,
      new StageFinalAdd
    )
  )

  pipeline.input(0) := io.x
  pipeline.input(1) := io.y
  io.result := pipeline.output(0)

  class StageEO extends Stage {
    val x = input(SFix(4 exp, 32 bits))
    val y = input(Vec(SFix(4 exp, 32 bits), 6))

    val e, o = Vec(SFix(4 exp, 32 bits), 5)

    for (i <- 0 until 5) {
      e(i) := y(i + 1) + y(i)
      o(i) := y(i + 1) - y(i)
    }

    output(e ++ o)
    output(x)
  }

  class StageCoeffMultAdd extends Stage {
    val eo = input(Vec(SFix(4 exp, 32 bits), 10))

    val c = Vec(SFix(4 exp, 32 bits), 6)

    for (i <- 0 until 6) {
      c(i) := eo(i * 2) * LagIntOpt.coeffs(i * 3) +
        eo(i * 2 + 1) * LagIntOpt.coeffs(i * 3 + 1) +
        eo(i * 2 + 2) * LagIntOpt.coeffs(i * 3 + 2)
    }

    val outVec = Vec(SFix(4 exp, 32 bits), 6)

    // Perform multiplications
    outVec(0) := eo(0)
    outVec(1) := eo(1) * (eo(10) + SFix(1.5, 4 exp))
    outVec(2) := eo(2) * (eo(10) + SFix(0.5, 4 exp))
    outVec(3) := eo(3) * (eo(10) - SFix(0.5, 4 exp))
    outVec(4) := eo(4) * (eo(10) - SFix(1.5, 4 exp))
    outVec(5) := eo(5) * (eo(10) - SFix(2.5, 4 exp))

    output(c)
    output(outVec)
  }

  class StageFinalAdd extends Stage {
    // Input vectors
    val c = input(Vec(SFix(4 exp, 32 bits), 6))
    val outVec = input(Vec(SFix(4 exp, 32 bits), 6))

    // Calculate the final result
    val result = c.zip(outVec).map { case (coeff, out) => coeff + out }

    output(result)
  }
}


case class OversamplingFIRConfig(
    inputWidth: Int = 24,
    outputWidth: Int = 24,
    shiftRegisterSize: Int = 6
)

class OversamplingFIR(config: OversamplingFIRConfig) extends Component {
  
  val io = new Bundle {
    val inputStream = slave Stream (Bits(config.inputWidth bits))
    val outputStream = master Stream (Bits(config.outputWidth bits))
  }

  // Optimize buffer
  val buffer = Mem(SFix(4 exp, config.inputWidth bits), config.shiftRegisterSize)
  val bufferWritePtr = Counter(config.shiftRegisterSize)
  
  // Optimized read pointers
  val bufferReadPtrs = Vec(SFix(4 exp, config.inputWidth bits), 6)
  
  // Streamline buffer write operation with efficient data conversion and conditional check
  when(io.inputStream.fire) {
    buffer(bufferWritePtr.value) := SFix.fromBits(io.inputStream.payload, 4 exp, config.inputWidth bits)
    bufferWritePtr.increment()
  }

  // Simplify interpolator connection by directly mapping buffer contents
  val interpolator = new Interpolator
  interpolator.io.x := SFix(2.0, 4 exp)
  
  for (i <- 0 until 6) {
    bufferReadPtrs(i) := buffer((bufferWritePtr.value - i) % config.shiftRegisterSize)
  }
  interpolator.io.y := bufferReadPtrs

  // Streamline output stream connection
  io.inputStream.ready := True
  io.outputStream.valid := io.inputStream.valid
  io.outputStream.payload := interpolator.io.result.toBits.resized(config.outputWidth bits)
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
      def generateToneSignal(
          freq: Double,
          sampleRate: Double,
          numSamples: Int,
          scalingFactor: Double
      ): Seq[Long] = {
        val omega = 2 * Pi * freq / sampleRate
        (0 until numSamples).map { n =>
          (sin(omega * n) * ((1 << (dut.config.inputWidth - 1)) - 1) * scalingFactor).toLong
        }
      }

      val freqs = Array.tabulate(numTones) { i =>
        minFreq * pow(maxFreq / minFreq, i.toDouble / (numTones - 1))
      }

      // Calculate the scaling factor for 1 dB less than the max amplitude
      val scalingFactor = pow(10, -1 / 20.0)

      val inputSignal = freqs
        .map(freq => generateToneSignal(freq, sampleRate, numSamples, scalingFactor))
        .reduce(_ zip _ map { case (x, y) => x + y })

      // Process the input signal through the OversamplingFIR
      val outputSignal = ArrayBuffer[Long]()
      for (n <- 0 until numSamples) {
        dut.io.inputStream.payload #= inputSignal(n)
        dut.io.inputStream.valid #= true
        dut.clockDomain.waitSampling()

        dut.io.inputStream.valid #= false
        dut.clockDomain.waitSampling(dut.config.outputOversampledRatio - 1)

        if (n % dut.config.outputOversampledRatio == 0) {
          outputSignal += dut.io.outputStream.payload.toLong
        }
      }

        // Save the output signal to a WAV file
        val outputBytes = outputSignal
          .flatMap(_.toBinaryString.reverse.padTo(dut.config.outputWidth, '0'))
          .grouped(8)
          .map(x => Integer.parseInt(x.reverse, 2).toByte)
          .toArray
        val outputOut = new ByteArrayOutputStream()
        val outputFormat = new AudioFormat(
          sampleRate.toFloat * dut.config.outputOversampledRatio,
          dut.config.outputWidth,
          1,
          true,
          false
        )
        val outputInputStream = new AudioInputStream(
          new ByteArrayInputStream(outputBytes),
          outputFormat,
          outputBytes.length / (dut.config.outputWidth / 8)
        )
        AudioSystem.write(
          outputInputStream,
          AudioFileFormat.Type.WAVE,
          new File("output_signal.wav")
        )

        println(
          "Input and output signals saved to input_signal.wav and output_signal.wav"
        )
    }
  }
}
