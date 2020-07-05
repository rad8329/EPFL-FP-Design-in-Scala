package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- frequency((1, Gen.const(empty)), (5, genHeap))
  } yield insert(value, heap)

  property(
    "The smallest of 2 elements should be the smallest in a previously empty Heap"
  ) = forAll { (from: A, to: A) =>
    val heap = insert(from, insert(to, empty))
    findMin(heap) == min(from, to)
  }

  property(
    "Deleting the minimal value in a Heap should result in an empty Heap"
  ) = forAll { (value: A) =>
    isEmpty(deleteMin(insert(value, empty)))
  }

  property(
    "The minimal value of a merged Heap should be the min of the min of both heaps"
  ) = forAll { (leftHeap: H, rightHeap: H) =>
    findMin(meld(leftHeap, rightHeap)) == min(
      findMin(leftHeap),
      findMin(rightHeap)
    )
  }

  property(
    "Finding and deleting elements recursively in a Heap should return same elements"
  ) = forAll { (heap: H) =>
    @scala.annotation.tailrec
    def isSorted(aHeap: H): Boolean =
      if (isEmpty(aHeap)) true
      else {
        val minimalValue = findMin(aHeap)
        val theHeapWithOutTheMinimumValue = deleteMin(aHeap)

        isEmpty(theHeapWithOutTheMinimumValue) || (minimalValue <= findMin(
          theHeapWithOutTheMinimumValue
        ) && isSorted(theHeapWithOutTheMinimumValue))
      }

    isSorted(heap)
  }

  property(
    "Two heaps should be equal after removing min elements, yielding the same elements until empty"
  ) = forAll { (leftHeap: H, rightHeap: H) =>
    @scala.annotation.tailrec
    def heapEqual(theLeftHeap: H, theRightHeap: H): Boolean =
      if (isEmpty(theLeftHeap) && isEmpty(theRightHeap)) true
      else {
        val leftMinimalValue = findMin(theLeftHeap)
        val rightMinimalValue = findMin(theRightHeap)
        leftMinimalValue == rightMinimalValue && heapEqual(
          deleteMin(theLeftHeap),
          deleteMin(theRightHeap)
        )
      }
    heapEqual(
      meld(leftHeap, rightHeap),
      meld(deleteMin(leftHeap), insert(findMin(leftHeap), rightHeap))
    )
  }

  property(
    "The minimal value of 2 heaps should be the minimal after dis pacing them and melding both"
  ) = forAll { (leftHeap: H, rightHeap: H) =>
    val minimal = min(findMin(leftHeap), findMin(rightHeap))

    findMin(meld(deleteMin(leftHeap), insert(minimal, rightHeap))) == minimal
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
