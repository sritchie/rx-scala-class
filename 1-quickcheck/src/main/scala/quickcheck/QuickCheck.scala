package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("Min works inside the heap") =
    forAll { (a: Int, b: Int) =>
      (a min b) == findMin(insert(b, insert(a, empty)))
    }

  property("Melded's min should be the min of left or right") =
    forAll { (l: H, r: H) =>
      val melded = meld(l, r)
      if (isEmpty(l) && isEmpty(r))
        isEmpty(melded)
      else if (isEmpty(l))
        findMin(melded) == findMin(r)
      else if (isEmpty(r))
        findMin(melded) == findMin(l)
      else {
        val meldedMin = findMin(melded)
        meldedMin == (findMin(l) min findMin(r))
      }
    }

  property("insert and delete should equal empty.") =
    forAll { i: Int => isEmpty(deleteMin(insert(i, empty))) }

  protected def popUntilEmpty(h: H): List[A] =
    if (isEmpty(h))
      List.empty
    else
      findMin(h) :: popUntilEmpty(deleteMin(h))

  property("Popping should yield a sorted list.") =
    forAll { h: H =>
      val items = popUntilEmpty(h)
      items.sorted == items
    }

  property("Melding two heaps should contain all elements from each of the two component heaps.") =
    forAll { (l: H, r: H) =>
      popUntilEmpty(meld(l, r)).sorted ==
        (popUntilEmpty(l) ++ popUntilEmpty(r)).sorted
    }

  /**
    * I think that the way this trait's defined is totally insane,
    * BUT, what the hell. This is a generator of arbitrary heaps.
    */
  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(i, h)

  lazy val emptyHeap: Gen[H] = value(empty)

  implicit lazy val arbHeap: Arbitrary[H] =
    Arbitrary(oneOf(genHeap, emptyHeap))
}
