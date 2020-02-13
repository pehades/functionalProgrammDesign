package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def oneOf(gen1: Gen[H], gen2: Gen[H]) = {
    val x = (new java.util.Random).nextInt()
    if (x>0) gen1
    else gen2
  }

  lazy val genHeap: Gen[H] = for {
    x<- arbitrary[Int]
    y<- oneOf(empty, genHeap)
  } yield insert(x, y)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    val e = empty
    insert(1, e)
    insert(2, e)
    insert(5, e)
    findMin(h) == 1
  }

  property("gen3") = forAll { (h: H) =>
    val e = empty
    insert(1, e)
    insert(2, e)
    insert(5, e)
    val e2 = empty
    insert(2, e2)
    insert(5, e2)

    deleteMin(h) == e2
  }
  property("gen4") = forAll { (h: H) =>
    val e: H = empty
    isEmpty(e) == true
  }

  property("gen5") = forAll { (h: H) =>
    val e1 = empty
    insert(0, e1)
    insert(1, e1)
    insert(3, e1)

    val e2 = empty
    insert(0, e2)
    insert(5, e2)


    val e3 = empty
    insert(0, e3)
    insert(0, e3)
    insert(1, e3)
    insert(3, e3)
    insert(5, e3)
    meld(e1, e2) == e3
  }

}
