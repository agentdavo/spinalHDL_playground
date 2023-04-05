import spinal.core._
import spinal.lib._

// The MultilevelOutput module is a SpinalHDL component that implements a multilevel output stage for an audio amplifier. 
// The output stage uses a combination of Pulse Width Modulation (PWM) and Pulse Frequency Modulation (PFM) techniques 
// to generate a modulated output signal that can drive the amplifier.

// The input to the MultilevelOutput module is a signed integer value, which represents the digital input signal 
// to the output stage. The module converts this input signal to a modulated output signal by using multiple instances 
// of PWMOutput and PFMOutput components with different configurations.

// Each PWMOutput and PFMOutput instance generates a modulated output signal with a different level of resolution, 
// and the module combines these signals to generate a multilevel modulated output signal.

// The PWMOutput component generates a PWM signal with a specified duty cycle and period. The component generates a 
// PFM signal with a specified threshold and step size. The MultilevelOutput module uses multiple instances of 
// PWMOutput and PFMOutput components to generate multiple modulated signals with different levels of resolution.

// The MultilevelOutput module then combines these signals to generate a multilevel modulated output signal that 
// can drive the amplifier. Specifically, it uses a list of PWMOutput and PFMOutput instances to generate multiple 
// modulated signals, and then applies a logical OR operation to the output of each instance to generate the final 
// output signal. This technique provides higher resolution and lower distortion than either PWM or PFM technique alone.

// The output of the MultilevelOutput module is a Boolean value that represents the modulated output signal. 
// This output signal can be used to drive the amplifier, which can then amplify the modulated signal to the 
// desired level. The use of a multilevel output stage with a combination of PWM and PFM techniques is well-suited 
// for high-performance audio applications that require high resolution, low distortion, and high efficiency.

// MultilevelOutput module supports double-sided PWM. Double-sided PWM is a technique that uses two PWM signals with 
// opposite polarities to generate a modulated signal. This technique can provide better linearity and reduce even-order 
// harmonics in the output signal. We instantiate two instances of PWMOutput for each level of resolution, one with 
// positive polarity and one with negative polarity. We then use a difference amplifier to combine the outputs of 
// the positive and negative PWM signals and generate the double-sided PWM signal. We then combine the double-sided 
// PWM signal with the PFM signal to generate the final modulated output signal.


// In this example configuration, the input is signed 24 bits wide, the PWM resolution is 8 bits, 
// there are 5 levels of resolution, the PFM threshold is 8 bits wide, the PFM step size is 1, 
// and the input signal is right-shifted by 3 bits before being applied to the output stage.
// These parameters can be adjusted as needed depending on the specific requirements and constraints of the application.




case class MultilevelOutputConfig(
  inputWidth: BitCount = 24 bits,
  resolution: BitCount = 8 bits,
  levels: Int = 5,
  thresholdWidth: BitCount = 8 bits,
  step: UInt = 1,
  shift: Int = 3
)




case class PWMOutput(resolution: BitCount, polarity: Bool, deadtime: UInt) extends Component {
  val io = new Bundle {
    val dutyCycle = in UInt(resolution)
    val output = out Bool
  }

  // Counter that increments with each clock cycle
  val counter = Counter(resolution)
  
  // Threshold value to determine the duty cycle of the PWM output
  val threshold = Reg(UInt(resolution)) init(0)

  // Output PWM signal based on the polarity setting
  when(polarity) {
    io.output := counter < threshold
  } otherwise {
    io.output := counter >= threshold
  }

  // Increment the counter
  counter.increment()

  // If the counter overflows, clear it and add the deadtime to the threshold value
  when(counter.willOverflow) {
    counter.clearAll()
    threshold := threshold + deadtime
  }

  // Update the threshold value with the duty cycle input
  threshold := io.dutyCycle
}




case class PFMOutput(inputWidth: BitCount, thresholdWidth: BitCount, step: UInt) extends Component {
  val io = new Bundle {
    val input = in UInt inputWidth  // input signal
    val output = out Bool  // modulated output signal
  }

  // comparator register to compare the input signal with a threshold
  val comparator = Reg(UInt(thresholdWidth)) init(0)

  // set the output signal based on the comparison result
  io.output := io.input > comparator
  // increment the comparator threshold
  comparator := comparator + step
}




case class MultilevelOutput(inputWidth: BitCount, resolution: BitCount, levels: Int, thresholdWidth: BitCount, step: UInt, shift: Int) extends Component {
  val io = new Bundle {
    val input = in SInt inputWidth  // input signal
    val output = out Bool  // modulated output signal
  }

  // create a list of PWMOutput instances with positive polarity and 1 cycle deadtime
  val pwmOutputsPos = List.tabulate(levels) { i =>
    val pwm = PWMOutput(resolution, true, 1)
    // set the duty cycle of each PWM instance
    pwm.io.dutyCycle := (io.input.abs() * (i + 1)) >> shift
    pwm
  }

  // create a list of PWMOutput instances with negative polarity and 1 cycle deadtime
  val pwmOutputsNeg = List.tabulate(levels) { i =>
    val pwm = PWMOutput(resolution, false, 1)
    // set the duty cycle of each PWM instance
    pwm.io.dutyCycle := (io.input.abs() * (i + 1)) >> shift
    pwm
  }

  // create a list of PFMOutput instances
  val pfmOutputs = List.tabulate(levels) { i =>
    val pfm = PFMOutput(inputWidth, thresholdWidth, step)
    // set the input to each PFM instance
    pfm.io.input := io.input.abs()
    pfm
  }

  // create a list of outputs for each level
  val levelOutputs = pwmOutputsPos.zip(pwmOutputsNeg).zip(pfmOutputs).map { case ((pwmPos, pwmNeg), pfm) =>
    
    // calculate the difference between the positive and negative PWM outputs
    val pwmDiff = pwmPos.io.output.asSInt - pwmNeg.io.output.asSInt
    
    // determine the output for each level based on the PWM difference and the PFM output
    Mux(pwmDiff < 0, pfm.io.output || pwmPos.io.output, pfm.io.output || pwmNeg.io.output)
    
  }

  // set the final output of the MultilevelOutput component
  io.output := levelOutputs.reduceLeft(_ || _)
}




object MultilevelOutput {
  def apply(config: MultilevelOutputConfig)(input: SInt): Bool = {
    val module = MultilevelOutput(config)
    module.io.input := input
    module.io.output
  }
}
