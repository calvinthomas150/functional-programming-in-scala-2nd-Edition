package fpinscala
package datastructures

import org.scalatest._
import flatspec._
import matchers._

class ListSpec extends AnyFlatSpec with should.Matchers {

  val nonEmptyList: List[Int] = List(1,2,3,4)
  val emptyList: List[Nothing] = List.Nil

  "A list" should "all elements aside from the first when tail is called" in {
    List.tail(nonEmptyList) should be (List(2,3,4))
  }

  it should "throw NoSuchElementException if the List is empty and tail is called" in {

    a [NoSuchElementException] should be thrownBy {
      List.tail(emptyList)
    }
  }

  it should "return a list containing just the new head element when setHead is called on an empty list" in {
    List.setHead(1, emptyList) should be (List(1))
  }

  it should "re the same list but with the first item replaced when setHead is called on a non-empty list" in {
    List.setHead(5, nonEmptyList) should be (List(5,2,3,4))
  }

  it should "drop the number of elements suppled to the drop function from the front" in {
    List.drop(nonEmptyList, 2) should be (List(3,4))
  }

  it should "return the empty list if the number of elements requested to be dropped is greater than the number of" +
    "element in the list" in {
    List.drop(emptyList, 1) should be (emptyList)
    List.drop(nonEmptyList, 5) should be (emptyList)
  }

  it should "drop all elements until the predicate is false in the dropWhile function" in {
    List.dropWhile(nonEmptyList, _ < 2) should be (List(2,3,4))
  }

  it should "return the list without the final element when init is called and the list is not empty" in {
    List.init(nonEmptyList) should be (List(1,2,3))
  }

  it should "return the empty list when init is called on the empty list" in {
    List.init(emptyList) should be(List())
  }

  it should "return the empty list when init is called on a list with one element" in {
    List.init(List(1)) should be(List())
  }

  it should "calculate the length of the list in the expected way" in {
    List.length(nonEmptyList) should be (4)
  }

  it should "sum values in the expected way" in {
    List.sum(nonEmptyList) should be (10)
  }

  it should "multiply values in the expected way" in {
    List.product(List(1.0,2.0,3.0,4.0)) should be (24)
  }

  it should "reverse the list in the expected way" in {
    List.reverse(nonEmptyList) should be (List(4,3,2,1))
  }

  it should "give the same result for length_v2 when using length and lengthFoldLeft" in {
    List.length(nonEmptyList) should be (List.length_v2(nonEmptyList))
  }

  it should "give the correct result when doing subtraction with foldLeft and foldRight" in {
    List.foldLeft(nonEmptyList, 0, _ - _) should be (-10)
    List.foldRight(nonEmptyList, 0, _ - _) should be (-2)
  }

  it should "give the same result for foldRight and foldRight_v2 when using subtraction as the function" in {
    List.foldRight(nonEmptyList, 0, _ - _) should be (List.foldRight_v2(nonEmptyList, 0, _ - _))
  }

  it should "give the same result for append and append_v2" in {
    List.append(nonEmptyList, nonEmptyList) should be(List.append_v2(nonEmptyList, nonEmptyList))
  }

  it should "concatenate a List of Lists in to a single List when the concat function is called" in {
    List.concat(List(List(1,2,3,4), List(1,2,3,4))) should be (List(1,2,3,4,1,2,3,4))
  }

  it should "be able to convert from integers to strings using the map function" in {
    List.map(nonEmptyList, _.toString) should be (List("1", "2", "3", "4"))
  }

  it should "be able to increase every value by 1 in the list using the map function" in {
    List.map(nonEmptyList, _ + 1) should be(List(2, 3, 4, 5))
  }

  it should "remove all odd elements from a list when given an appropriate predicate" in {
    List.filter(nonEmptyList, _ % 2 == 0) should be(List(2,4))
  }

  it should "apply the map function and then flatten when using flatMap in the usual way" in {
    List.flatMap(List(1,2,3), i => List(i,i)) should be(List(1,1,2,2,3,3))
  }

  it should "give the same result for filter and filter_v2" in {
    List.filter(nonEmptyList, _ % 2 == 0) should be (List.filter_v2(nonEmptyList, _ % 2 == 0))
  }

  it should "correctly add pairwise when using the zipWith function with addition and two lists of the same size" in {
    List.zipWith(nonEmptyList, nonEmptyList, _ + _) should be(List(2,4,6,8))
  }

  it should "correctly add pairwise up to the end of the shortest list when using the zipWith function with addition" in {
    List.zipWith(List.drop(nonEmptyList, 2), nonEmptyList, _ + _) should be(List(4, 6))
    List.zipWith(nonEmptyList, List.drop(nonEmptyList, 2), _ + _) should be(List(4, 6))
  }

}
