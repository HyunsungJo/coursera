package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    if (isEmpty(h)) {
      val r = scala.util.Random
      val a = r.nextInt
      val b = r.nextInt
      findMin(insert(b, insert(a, h))) == Math.min(a, b)
    } else true
  }

  property("gen3") = forAll { (h: H) =>
    if (isEmpty(h)) {
      val r = scala.util.Random
      val i = r.nextInt
      isEmpty(deleteMin(insert(i, h)))
    } else true
  }

  property("gen4") = forAll { (h: H) =>
    def findAndDelMin(h: H, prevMin: Int): Boolean = {
//      println(s"prevMin: $prevMin, h: $h")
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        if (min >= prevMin) findAndDelMin(deleteMin(h), min)
        else false
      }
    }
//    println("=========================")
    if (!isEmpty(h)) {
      val min = findMin(h)
      findAndDelMin(deleteMin(h), min)
    } else true
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val meldMin = findMin(meld(h1, h2))
    meldMin == findMin(h1) || meldMin == findMin(h2)
  }

  property("gen6") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      val orgMin = findMin(h)
      findMin(deleteMin(insert(orgMin - 1, h))) == orgMin
    } else true
  }
}
