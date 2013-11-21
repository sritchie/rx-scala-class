package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(false)
    run

    assert(out.getSignal === true, "and 1")
  }

  test("demux") {
    def wires(n: Int) =
      Stream.continually(new Wire).take(n).toList
    val in = new Wire
    val cs @ List(c1, c2) = wires(2)
    val os @ List(o1, o2, o3, o4) = wires(4)
    demux(in, cs, os)
    in.setSignal(true)
    // Set them to index
    c1.setSignal(true)
    c2.setSignal(false)
    run

    assert(o1.getSignal == false &&
      o2.getSignal == true &&
      o3.getSignal == false &&
      o4.getSignal == false, "demux 1")
  }
}
