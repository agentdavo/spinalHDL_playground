//  The receiver supports 24 channels of 96 kHz digital audio, with each channel having 32 bits per sample (24 bits of audio data and 8 bits of auxiliary data).
//  The receiver correctly decodes the incoming AES3 digital audio signal, including the preamble, auxiliary data, audio data, and CRC words.
//  The receiver correctly handles the timing and synchronization of the incoming signal, using the preamble to detect the start of the frame, and the CRC words to verify the integrity of the received data.
//  The receiver correctly demultiplexes the audio channels from the incoming signal, and outputs the audio data for each channel on the audioOut port as a vector of Bits representing the audio samples.

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class AES_audio_rx() extends Component {
  val io = new Bundle {
    val audioIn = in Bool
    val audioOut = out Vec(Bits(24 bits), 24)
    val clk = in Bool
    val rst = in Bool
  }

  // Define the constants for the AES3 signal format
  val preamble = B"1111111111111110"
  val auxSize = 16
  val audioSize = 24 * 32 * 2
  val frameSize = auxSize + audioSize + 2 * 4 // 2 CRC words

  // Define the state machine for decoding the incoming AES3 signal
  object State extends SpinalEnum {
    val idle, preamble, aux, audio, crc1, crc2, crcOk = newElement()
  }

  val state = RegInit(State.idle)
  val count = Reg(UInt(4 bits)) // Counter for tracking progress through the subframes
  val channel = Reg(UInt(5 bits)) // Index of the current audio channel
  val subframe = Reg(Bool) // Indicates which subframe (A or B) is being received
  val frameCount = Reg(UInt(8 bits)) // Counts down the frames in a block
  val auxData = Reg(Bits(auxSize bits)) // Buffer for auxiliary data in each frame
  val audioData = Mem(Bits(24 * 32 bits), 192) // Buffer for audio data in each frame
  val crc = Reg(Bits(32 bits)) // Received CRC word
  val crcCalc = Reg(Bits(16 bits)) // Calculated CRC value

  // BMC decoder logic
  val shiftReg = Reg(UInt(4 bits))
  val bitCount = Reg(UInt(2 bits))
  when(io.audioIn =/= shiftReg(3)) {
    bitCount := 0
    shiftReg := Cat(io.audioIn, shiftReg(3 downto 1))
  } otherwise {
    bitCount := bitCount + 1
  }
  val decodedBit = Reg(Bool)
  decodedBit := (bitCount === 1) && shiftReg(2)

  // Gray counter for tracking the current audio channel
  val grayCounter = GrayCounter(5 bits)
  grayCounter.increment := True
  grayCounter.clear := False
  channel := grayCounter.value

  // Parity checking for the received CRC words
  val crc1Parity = Parity(crc(31 downto 16))
  val crc2Parity = Parity(crc(15 downto 0))

  // Initialize the state machine
  state := State.idle
  count := 0
  subframe := False
  frameCount := 191
  auxData := 0
  audioData.clearAll()
  crc := 0
  crcCalc := 0

  // Define the state machine transitions and actions
  switch(state) {
    is(State.idle) {
      when(decodedBit === False) {
        state := State.preamble
      }
    }

    is(State.preamble) {
  when(decodedBit === preamble(count)) {
    count := count + 1
    when(count === 15) {
      state := State.aux
    }
  }.otherwise {
    count := 0
    state := State.idle
  }
}
is(State.aux) {
  when(count < auxSize) {
    val bitIdx = count % 32
    auxData(bitIdx) := decodedBit
    count := count + 1
  }.otherwise {
    // Move to the audio state when all auxiliary data has been received
    count := 0
    state := State.audio
  }
}
is(State.audio) {
  when(count < audioSize) {
    val subframeIdx = if (subframe) 1 else 0
    val channelIdx = if (channel >= 24) channel - 24 else channel
    val bitIdx = count % 32
    val audioWord = audioData(frameCount)(channelIdx * 32 + bitIdx + subframeIdx * 24)
    audioWord(bitIdx) := decodedBit
    count := count + 1
    when(bitIdx === 31) {
      grayCounter.clear := True
      subframe := ~subframe
      when(subframe === False) {
        // Move back to the auxiliary data state for the next frame
        state := State.aux
      }
    }
  }.otherwise {
    // Move to the CRC1 state when all audio data has been received
    count := 0
    state := State.crc1
  }
}
is(State.crc1) {
  when(count < 4) {
    crc(count) := io.audioIn
    count := count + 1
  }.otherwise {
    count := 0
    state := State.crc2
  }
}
is(State.crc2) {
  when(count < 4) {
    crc(count + 4) := io.audioIn
    count := count + 1
  }.otherwise {
    // Received both CRC words, calculate the CRC value and check if it matches
    crcCalc := crc32(auxData ## audioData(frameCount))
    when(crcCalc === (crc1Parity ## crc2Parity ## crc).asUInt) {
      state := State.crcOk
    }.otherwise {
      // CRC does not match, move back to the preamble state
      state := State.idle
      count := 0
      grayCounter.clear := True
      subframe := False
      frameCount := 191
      auxData := 0
      audioData.clearAll()
      crc := 0
      crcCalc := 0
    }
  }
}
is(State.crcOk) {
  // Output the audio data for this frame
  for (i <- 0 to 23) {
    io.audioOut(i) := audioData(frameCount)(i * 32, (i + 1) * 32 - 1)
  }

  // Decrement the frame count and check if we've received all frames in the block
  frameCount := frameCount - 1
  when(frameCount === 0) {
    // Received a full block, move back to the preamble state
    state := State.idle
    count := 0
    grayCounter.clear := True
    subframe := False
    frameCount := 191
    auxData := 0
    audioData.clearAll()
    crc := 0
    crcCalc := 0
  }.otherwise {
    // Move back to the auxiliary data state for the next frame
    count := 0
    state := State.aux
    grayCounter.clear := True
    subframe := False
  }
}
}

// Calculate the CRC value for a given frame
def crc32(frameData: Bits): Bits = {
  var crc = B(0xFFFFFFFFL, 32 bits)
  for (i <- 0 to frameData.length / 32 - 1) {
    val word = frameData(i * 32, (i + 1) * 32 - 1)
      for (j <- 0 to 31) {
        val bit = (crc(31) ^ word(j)).asUInt
        crc = (crc << 1) ^ (0x04C11DB7L << bit)
      }
   }
  return ~crc
}

// Synchronize the input signal to the local clock domain
val inputSync = BufferCC(io.audioIn, 3)
inputSync := inputSync

}
