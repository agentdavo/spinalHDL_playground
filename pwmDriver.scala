import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

import spinal.sim._


case class PWNDriver(width: Int) extends Component {
  val io = new Bundle {
    val pwmIn = in(SInt(width bits))
    val pfmIn = in(SInt(width bits))
    val pwmOutHighSide = out(Bool)
    val pwmOutLowSide = out(Bool)
    val pfmOut = out(Bool)
    val levels = in(Vec(UInt(width bits), 2 to 32))
    val deadtime = in(TimeNumber())
    val pfmThreshold = in(FixedPoint(16 bits, 14 bits))
    val turnOnDelayHigh = in(Vec(TimeNumber(), 2 to 32))
    val turnOffDelayHigh = in(Vec(TimeNumber(), 2 to 32))
    val turnOnDelayLow = in(Vec(TimeNumber(), 2 to 32))
    val turnOffDelayLow = in(Vec(TimeNumber(), 2 to 32))
  }

  // Quantize the input signals to the available levels
  val levels = io.levels
  val levelBits = log2Up(levels.max)
  val pwmInQuantized = levels.map(level => (io.pwmIn.asUInt * level / (BigInt(1) << width) + level / 2) / level)
  val pfmInQuantized = levels.map(level => (io.pfmIn.asUInt * level / (BigInt(1) << width) + level / 2) / level)

  // Generate multi-level PWM signal
  val pwmOut = (io.pwmOutHighSide, io.pwmOutLowSide).zipped.map { (pwmHigh, pwmLow) =>
    val pwmDelayed = RegNext(pwmHigh || pwmLow) init(False)
    val pwmTurnOnDelay = (pwmHigh, pwmLow) match {
      case (true, false) => io.turnOnDelayHigh
      case (false, true) => io.turnOnDelayLow
      case (true, true) => Seq(io.turnOnDelayHigh.min(io.turnOnDelayLow): _*)
      case (false, false) => Seq.fill(io.turnOnDelayHigh.length)(0.ns)
    }
    val pwmTurnOffDelay = (pwmHigh, pwmLow) match {
      case (true, false) => io.turnOffDelayHigh
      case (false, true) => io.turnOffDelayLow
      case (true, true) => Seq(io.turnOffDelayHigh.max(io.turnOffDelayLow): _*)
      case (false, false) => Seq.fill(io.turnOffDelayHigh.length)(0.ns)
    }
    val pwmDelay = (io.deadtime.toTicks - (io.deadtime.toTicks * level / levels.last)).toInt
    pwmDelayed.clearWhen(!(pwmHigh || pwmLow))
    !pwmDelayed.delay(pwmDelay + pwmTurnOffDelay(level)) && (pwmHigh.delay(pwmTurnOnDelay(level)) || pwmLow.delay(pwmTurnOnDelay(level)))
  }.reduce(_ || _)

  // Generate PFM signal
  val pfmThresholdQuantized = levels.min + (levels.max - levels.min) * io.pfmThreshold.toDouble
  val pfmOut = pfmInQuantized.map(_ >= pfmThresholdQuantized).reduce(_ && _)

  // Assign output signals
  io.pwmOutHighSide := pwmOut
  io.pwmOutLowSide := pwmOut
  io.pfmOut := pfmOut
}






case class PWNDriverApb3(width: BitCount) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(addressWidth = log2Up(width / 8), dataWidth = 32))
    val pwmOutHighSide = out(Bool)
    val pwmOutLowSide = out(Bool)
  }

  val pwnDriver = PWNDriver(width)

  pwnDriver.io.pwmIn := io.apb.PADDR
  pwnDriver.io.levels := Vec(io.apb.PRDATA.grouped(4).map(_.asUInt))
  pwnDriver.io.deadtime := io.apb.PWDATA(7 downto 0).asUInt
  pwnDriver.io.turnOnDelayHigh := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
  pwnDriver.io.turnOffDelayHigh := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
  pwnDriver.io.turnOnDelayLow := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
  pwnDriver.io.turnOffDelayLow := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
  io.pwmOutHighSide := pwnDriver.io.pwmOutHighSide
  io.pwmOutLowSide := pwnDriver.io.pwmOutLowSide

  // APB3 register mapping
  io.apb.PRDATA := 0
  io.apb.PREADY := True

  val pwmOutReg = RegNext(io.pwmOutHighSide ## io.pwmOutLowSide)
  when (io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE) {
    switch (io.apb.PADDR) {
      case 0 =>
        pwnDriver.io.pwmIn := io.apb.PWDATA
      case 4 =>
        pwnDriver.io.levels := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
      case 8 =>
        pwnDriver.io.deadtime := io.apb.PWDATA(7 downto 0).asUInt
      case 12 =>
        pwnDriver.io.turnOnDelayHigh := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
      case 16 =>
        pwnDriver.io.turnOffDelayHigh := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
      case 20 =>
        pwnDriver.io.turnOnDelayLow := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
      case 24 =>
        pwnDriver.io.turnOffDelayLow := Vec(io.apb.PWDATA.grouped(4).map(_.asUInt))
    }
  }.otherwise {
    io.apb.PRDATA := pwmOutReg.asUInt
    io.apb.PREADY := True
  }
}





object PWNDriverTestBench extends App {
  SimConfig.withWave.compile {
    val dut = PWNDriver(width = 8)

    dut.io.levels(0) := 0
    dut.io.levels(1) := 64
    dut.io.levels(2) := 128
    dut.io.levels(3) := 192

    dut.io.turnOnDelayHigh(0) := 1.us
    dut.io.turnOnDelayHigh(1) := 2.us
    dut.io.turnOnDelayHigh(2) := 3.us

    dut.io.turnOffDelayHigh(0) := 4.us
    dut.io.turnOffDelayHigh(1) := 5.us
    dut.io.turnOffDelayHigh(2) := 6.us

    dut.io.turnOnDelayLow(0) := 7.us
    dut.io.turnOnDelayLow(1) := 8.us
    dut.io.turnOnDelayLow(2) := 9.us

    dut.io.turnOffDelayLow(0) := 10.us
    dut.io.turnOffDelayLow(1) := 11.us
    dut.io.turnOffDelayLow(2) := 12.us

    dut
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)

    // Test multi-level PWM
    for (pwm <- -127 to 127) {
      dut.io.pwmIn #= pwm
      sleep(10)
      assert(dut.io.pwmOutHighSide.toBoolean == (pwm >= 0))
      assert(dut.io.pwmOutLowSide.toBoolean == (pwm < 0))
    }

    // Test PFM
    for (pfm <- -127 to 127) {
      dut.io.pfmIn #= pfm
      sleep(10)
      assert(dut.io.pfmOut.toBoolean == (pfm >= 0))
    }
  }
}
