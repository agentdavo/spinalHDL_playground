import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinal.sim._
import scala.util.Random


// This testbench generates a few different test signals with different frequencies, resamples them to 8 kHz, 
// and feeds them into the Goertzel component. The estimated frequency is printed out at the end of the simulation.


object GoertzelSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new GoertzelTestbench()) { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      // Test signals
      val frequencies = Seq(100, 1000, 3000)
      val sampleRate = 8000
      val sampleCount = 128
      val maxAmplitude = (1 << 23) - 1

      val samples = Seq.tabulate(sampleCount) { i =>
        val t = i.toDouble / sampleRate
        frequencies.map { f =>
          (maxAmplitude * math.sin(2.0 * math.Pi * f * t)).toInt
        }.sum
      }

      // Generate input stream
      val input = StreamFragment(BigInt(0), mask = BigInt((1 << 24) - 1))
      SimThread(input.ready #= true)
      SimThread(input.valid #= false)
      dut.clockDomain.waitSampling(10)

      for (sample <- samples) {
        val bits = sample & ((1 << 24) - 1)
        input.fragment #= bits
        input.valid #= true
        SimThread.sleep(1)
        input.valid #= false
        dut.clockDomain.waitSampling(1)
      }

      // Wait for the final result
      while (!dut.io.result.valid.toBoolean) {
        dut.clockDomain.waitSampling(1)
      }

      // Print results
      val frequency = dut.io.result.payload.toInt
      println(s"Detected frequency: ${frequency} Hz")
    }
  }
}

class GoertzelTestbench extends Component {
  val io = new Bundle {
    val pcmInput = slave StreamFragment(Bits(24 bits))
    val result = master Stream(UInt(32 bits))
  }

  val goertzel = new Goertzel(io.pcmInput)

  io.result.valid := goertzel.io.result.valid
  io.result.payload := goertzel.io.result.payload
}
