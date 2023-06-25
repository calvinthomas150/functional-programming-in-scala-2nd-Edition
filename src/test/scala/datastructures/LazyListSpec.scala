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
    LazyList(1,2,3,4,5,6).take(3).toList should be(LazyList(1,2,3).toList)

  it should "return an empty LazyList the original LazyList is empty" in:
    LazyList.empty.take(5) should be(LazyList.empty)

  "drop" should "return a new LazyList without the first n elements of the LazyList" in:
    LazyList(1,2,3,4,5,6).drop(3).toList should be(LazyList(4,5,6).toList)

  it should "return an empty LazyList if the original LazyList is empty" in:
    LazyList.empty.drop(5) should be(LazyList.empty)

  "takeWhile" should "return a new LazyList with all the elements until the predicate does not hold" in:
    LazyList(1,2,3,4,5,6).takeWhile(_ < 5).toList should be(LazyList(1,2,3,4).toList)

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
    LazyList(1,2,3,4,5,6).map(_ * 2).toList should be(LazyList(2,4,6,8,10,12).toList)

  "filter" should "keep only the elements that match the predicate" in:
    LazyList(1,2,3,4,5,6).filter(_ < 4).toList should be(LazyList(1,2,3).toList)

  "append" should "combine the two lists together in the correct order" in:
    LazyList(1,2,3,4,5,6).append(LazyList(7,8,9,10)).toList should be(LazyList(1,2,3,4,5,6,7,8,9,10).toList)

  "flatMap" should "perform the function on a LazyList and flatten the result" in:
    LazyList(1,2,3,4,5,6).flatMap(i => LazyList(i, i + 1)).toList should be(LazyList(1,2,2,3,3,4,4,5,5,6,6,7).toList)




