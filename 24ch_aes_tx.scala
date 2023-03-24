import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

import scala.util.Random




case class AES_audio_tx(numChannels: Int = 24) extends Component {
  val io = new Bundle {
    val audioIn = in Stream(Vec(Bits(24 bits), numChannels))
    val aesOut = out Stream Bool
  }
  
  val sampleRate = 48000 Hz
  val bmcTable = List(
    "10".b, "11".b, "001".b, "0001".b, "00001".b, "000001".b, "0000001".b, "00000001".b
  )

  // Generate the preamble and syncword
  val preamble = "0" * 196
  val syncword = "11111111111110".b

  // Generate the channel status word
  val channelStatus = Bits(10 bits)
  channelStatus(0, 3) := "0011".b // Professional use
  channelStatus(3, 5) := (numChannels - 1).asBits.resize(2)
  channelStatus(5, 7) := "11".b // Sample rate 48kHz
  channelStatus(7) := True // Audio word length = 24 bits
  channelStatus(8, 10) := "10".b // No emphasis

  // BMC encoder
  val bmc = new Area {
    val state = RegInit(False)

    io.aesOut.valid := False
    io.aesOut.payload := False

    when(io.audioIn.valid) {
      val channelBits = io.audioIn.payload.flatMap(_.resized)
      val subframes = channelBits.subdivideIn(192 bits)
      val words = subframes.map { subframeBits =>
        val encoded = bmcTable((subframeBits << 1) | state.asUInt).asBools
        state := encoded.last
        B(encoded.dropRight(1).asBits, 2 bits)
      }
      io.aesOut.valid := True
      io.aesOut.payload := preamble ## channelStatus ## words.toBits ## syncword
    }
  }
}




object AES_audio_tx_tb extends App {
  val audioInChannelCount = 24
  val sampleRate = 48000

  val compiled = SimConfig.withWave.compile(new AES_audio_tx(audioInChannelCount, sampleRate))

  def randSignal(): Boolean = {
    Random.nextBoolean()
  }

  // Helper function to generate a test stream of audio data
  def generateTestData(n: Int): Stream[Bits] = {
    Stream.iterate(B(0, 24 bits))(prev => {
      val next = B((0 until 24).map(_ => randSignal()): _*)
      sleep((1.0 / sampleRate * 1e12 / 2).toLong) // sleep for half a sample period
      next
    }).take(n)
  }

  compiled.doSimUntilVoid { dut =>
    val clockThread = fork {
      dut.clockDomain.forkStimulus(2)
    }

    val audioInDriver = fork {
      // Generate a test stream of audio data
      val testData = generateTestData(10000)

      testData.foreach { sample =>
        dut.io.audioIn.valid #= true
        dut.io.audioIn.payload #= sample
        sleep((1.0 / sampleRate * 1e12 * 16).toLong) // sleep for 16 sample periods
        dut.io.audioIn.valid #= false
        sleep((1.0 / sampleRate * 1e12 * 16).toLong) // sleep for 16 sample periods
      }
    }

    val aesOutMonitor = fork {
      var channelStatusValid = false
      var audioValidIn = false
      var preambleValid = false
      var syncWordValid = false
      var audioDataValid = false

      dut.io.aesOut.toStream.take(1000000).foreach { bit =>
        if (!channelStatusValid && bit) {
          channelStatusValid = true
        } else if (!audioValidIn && channelStatusValid && bit) {
          audioValidIn = true
        } else if (!preambleValid && audioValidIn && bit) {
          preambleValid = true
        } else if (!syncWordValid && preambleValid && bit) {
          syncWordValid = true
        } else if (!audioDataValid && syncWordValid && bit) {
          audioDataValid = true
        }

        if (channelStatusValid && audioValidIn && preambleValid && syncWordValid && audioDataValid) {
          println(s"Found valid AES3 audio frame at ${simTime()}")
          channelStatusValid = false
          audioValidIn = false
          preambleValid = false
          syncWordValid = false
          audioDataValid = false
        }
      }
    }

    clockThread.join()
    audioInDriver.join()
    aesOutMonitor.join()
  }
}
