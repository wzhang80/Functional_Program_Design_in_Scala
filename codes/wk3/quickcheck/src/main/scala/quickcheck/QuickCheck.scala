package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      y <- oneOf(const(empty), genHeap)
    } yield insert(x, y)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int, c: Int) =>
    val vec = Vector(a, b, c).sorted
    val h1 = insert(a, empty)
    val h2 = insert(b,h1)
    val h3 = insert(c,h2)
    findMin(h3) == vec(0) && findMin(deleteMin(h3)) == vec(1) && findMin(deleteMin(deleteMin(h3))) == vec(2)
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sort1") = forAll { h: H =>
    def helper(hh: H): Boolean = {
      if (hh == empty) {
        true
      } else {
        val min = findMin(hh)
        if (deleteMin(hh) == empty) true
        else min <= findMin(deleteMin(hh)) && helper(deleteMin(hh))
      }
    }
    helper(h)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    if (h1 == empty && h2 == empty) {
      meld(h1,h2) == empty
    }
    else if (h1 == empty) {
      findMin(meld(h1,h2)) == findMin(h2)
    } else if (h2 == empty) {
      findMin(meld(h1,h2)) == findMin(h1)
    } else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      if (min1 <= min2) findMin(meld(h1,h2)) == min1
      else findMin(meld(h1,h2)) == min2
    }
  }

}
