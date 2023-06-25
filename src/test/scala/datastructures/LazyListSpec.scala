package fpinscala
package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.List

class LazyListSpec extends AnyFlatSpec with should.Matchers:

  "toList" should "convert the lazy list to a List retaining all the same values" in:
    LazyList(1,2,3,4,5).toList should be (List(1,2,3,4,5))

  it should "return Nil when the LazyList is empty" in:
    LazyList.empty.toList should be (Nil)

  "take" should "return a new LazyList containing the first n elements of the LazyList" in:
    LazyList(1,2,3,4,5,6).take(3).toList should be(List(1,2,3))

  it should "return an empty LazyList the original LazyList is empty" in:
    LazyList.empty.take(5) should be(LazyList.empty)

  "drop" should "return a new LazyList without the first n elements of the LazyList" in:
    LazyList(1,2,3,4,5,6).drop(3).toList should be(List(4,5,6))

  it should "return an empty LazyList if the original LazyList is empty" in:
    LazyList.empty.drop(5) should be(LazyList.empty)

  "takeWhile" should "return a new LazyList with all the elements until the predicate does not hold" in:
    LazyList(1,2,3,4,5,6).takeWhile(_ < 5).toList should be(List(1,2,3,4))

  "forAll" should "return true if the predicate matches for all values in the LazyList" in:
    LazyList(1,2,3,4,5,6).forAll(_ < 7) should be(true)

  it should "return false if the predicate fails for any value in the LazyList" in:
    LazyList(1,2,3,4,5,6).forAll(_ < 5) should be(false)

  "exists" should "return true if the predicate matches for all values in the LazyList" in :
    LazyList(1, 2, 3, 4, 5, 6).exists(_ < 7) should be(true)

  it should "return false if the predicate matches for onlu some value in the LazyList" in :
    LazyList(1, 2, 3, 4, 5, 6).exists(_ < 5) should be(true)

  it should "return false if the predicate fails for all values in the LazyList" in :
    LazyList(1, 2, 3, 4, 5, 6).exists(_ > 10) should be(false)

  "headOption" should "return a value wrapped in an Option when the LazyList is not empty" in:
    LazyList(1,2,3,4).headOption should be(Some(1))

  it should "return None when the LazyList is empty" in:
    LazyList.empty.headOption should be(None)

  "map" should "apply the provided function to the values in the LazyList" in:
    LazyList(1,2,3,4,5,6).map(_ * 2).toList should be(List(2,4,6,8,10,12))

  "filter" should "keep only the elements that match the predicate" in:
    LazyList(1,2,3,4,5,6).filter(_ < 4).toList should be(List(1,2,3))

  "append" should "combine the two lists together in the correct order" in:
    LazyList(1,2,3,4,5,6).append(LazyList(7,8,9,10)).toList should be(List(1,2,3,4,5,6,7,8,9,10))

  "flatMap" should "perform the function on a LazyList and flatten the result" in:
    LazyList(1,2,3,4,5,6).flatMap(i => LazyList(i, i + 1)).toList should be(List(1,2,2,3,3,4,4,5,5,6,6,7))

  "continually" should "keep generating the same value forever" in:
    LazyList.continually[Int](1).take(5).toList should be(List(1,1,1,1,1))

  "from" should "generate an infinite lazy list starting from n" in:
    LazyList.from(5).take(5).toList should be(List(5,6,7,8,9))

  "fibs" should "return an infinite list of the fibonacci sequence" in:
    LazyList.fibs.take(10).toList should be(List(0,1,1,2,3,5,8,13,21,34))

  "zipAll" should "zip together should zip together with Some values where the value exists and None when it doesn't" in:
    LazyList(1,2,3,4,5).zipAll(LazyList(1,2,3,4)).toList should be (List((Some(1),Some(1)),(Some(2),Some(2)),(Some(3),Some(3)),(Some(4),Some(4)),(Some(5),None)))

    LazyList(1, 2, 3, 4).zipAll(LazyList(1, 2, 3, 4, 5)).toList should be (List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)), (Some(4), Some(4)), (None, Some(5))))

    LazyList(1, 2, 3, 4, 5).zipAll(LazyList(1, 2, 3, 4, 5)).toList should be
    List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)), (Some(4), Some(4)), (Some(5), Some(5)))

  "zipWith" should "correctly add pairwise when using the zipWith function with addition and two lists of the same size" in:
    LazyList(1,2,3,4).zipWith(LazyList(1,2,3,4))(_ + _).toList should be(List(2, 4, 6, 8))

  "startsWith" should "return true if the prefix matches the start of the LazyList" in:
    LazyList(1,2,3,4,5).startsWith(LazyList(1,2,3)) should be(true)


  "tails" should "return all suffixes of a non-empty list" in:
    val list = LazyList(1, 2, 3)
    val result = list.tails.toList.map(_.toList)
    result should be(List(List(1, 2, 3), List(2, 3), List(3), List()))

  it should "return a list with an empty list for an empty list" in:
    val list = LazyList.empty[Int]
    val result = list.tails.toList.map(_.toList)
    result should be(List(List()))

  "scanRight" should "apply the binary operation from right to left, keeping the intermediate results" in:
    val list = LazyList(1, 2, 3)
    val result = list.scanRight(0)(_ + _).toList
    result should be(List(6, 5, 3, 0))

  it should "include the initial value for an empty list" in:
    val list = LazyList.empty[Int]
    val result = list.scanRight(0)(_ + _).toList
    result should be(List(0))
