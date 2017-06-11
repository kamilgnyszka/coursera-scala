package quickcheck

import common._

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int)  =>
    val m = findMin(insert(b, insert(a, empty)))
    val n = findMin(insert(a, insert(b, empty)))
    if (a < b) m == a && n == a
    else m == b && n == b
  }

  property("gen3") = forAll { (a: Int)  =>
    isEmpty(deleteMin(insert(a,empty)))
  }

  property("gen4") = forAll { (h: H)  =>
    def createList(heap: H): List[A] =  {
      if (isEmpty(heap)) List()
      else findMin(heap)::createList(deleteMin(heap))
    }

    val l = createList(h)
    val ls = l.sorted

    ls == l
  }

  property("gen5") = forAll { (h1: H,h2: H)  =>
    findMin(h1).min(findMin(h2)) == findMin(meld(h1,h2))
  }

  property("gen6") = forAll { (a: Int, b: Int, c: Int, d: Int)  =>
    def createList(heap: H): List[A] =  {
      if (isEmpty(heap)) List()
      else findMin(heap)::createList(deleteMin(heap))
    }
    val l1 = List(a, b, c, d).sorted
    val l2 = createList(insert(a,insert(b,insert(c,insert(d,empty)))))

    l1 == l2
  }
}
