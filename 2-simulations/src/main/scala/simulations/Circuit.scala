package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  /**
    * I started trying to implement this as a binary function above,
    * but the delay mixin is so gnarly that I just accepted the pain
    * and copy-pasted like a champion.
    */
  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig || a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  /**
    * without even looking at the lecture notes!!!
    */
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a, b, c = new Wire
    inverter(a1, a)
    inverter(a2, b)
    andGate(a, b, c)
    inverter(c, output)
  }

  /**
    * Control wires and output wires are in here in decreasing
    * order.
    */
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    assert(out.length == Math.pow(2, c.length))
    def setAll(items: List[Wire], bool: Boolean) {
      items.foreach(_.setSignal(bool))
    }
    def halve[T](items: List[T]): (List[T], List[T]) = {
      assert(items.length % 2 == 0)
      val idx = items.length / 2
      (items.take(idx), items.drop(idx))
    }
    def demuxAction() {
      (c, out) match {
        case (Nil, w :: Nil) =>
          w.setSignal(in.getSignal)
        case (c :: cs, ws) =>
          val (hi, lo) = halve(ws)
          val outs = if (c.getSignal) {
            setAll(lo, false)
            hi
          } else {
            setAll(hi, false)
            lo
          }
          demux(in, cs, outs)
      }
    }
    in addAction demuxAction
    c.foreach(_.addAction(demuxAction))
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
