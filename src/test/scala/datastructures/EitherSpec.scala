package fpinscala
package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Either.*

class EitherSpec extends AnyFlatSpec with Matchers:

  "map" should "transform the value inside a Right" in:
    Right(2).map(_ * 2) should be (Right(4))

  it should "leave a Left unchanged" in:
    Left("Error").map((_: Int) * 2) should be (Left("Error"))

  "flatMap" should "transform the value inside a Right using a function that returns an either" in:
    Right(2).flatMap(a => Right(a * 2)) should be (Right(4))

  it should "leave a Left unchanged" in:
    Left("Error").flatMap((_: Int) => Right(2)) should be (Left("Error"))

  "orElse" should "return the original Right" in:
    Right(2).orElse(Right(3)) should be (Right(2))

  it should "return the alternative for a Left" in:
    Left("Error").orElse(Right(3)) should be (Right(3))

  "map2" should "combine two Rights" in:
    Right(2).map2(Right(3))(_ + _) should be (Right(5))

  it should "return a Left if the first Either is a Left" in:
    Left("Error").map2(Right(2))((e,a) =>  e) should be (Left("Error"))

  it should "return a Left if the second Either is a Left" in:
    Right(2).map2(Left("Error"))((a,e) => a) should be (Left("Error"))

  it should "return the first Left if both Eithers are Lefts" in:
    Left("Error1").map2(Left("Error2"))((e,ee) => e) should be (Left("Error1"))

  "traverse" should "transform a list and wrap it in a Right" in:
    Either.traverse(scala.List(1, 2, 3))(a => Right(a * 2)) should be(Right(scala.List(2, 4, 6)))


  it should "return the first error in a Left for a list with errors" in:
    Either.traverse(scala.List(1, 2, 3))(a => if (a == 2) Left("Error") else Right(a)) should be(Left("Error"))


  "sequence" should "turn a list of Rights into a Right of list" in:
    Either.sequence(scala.List(Right(1), Right(2), Right(3))) should be(Right(scala.List(1, 2, 3)))

  it should "return the first Left in a list of Eithers" in:
    Either.sequence(scala.List(Right(1), Left("Error"), Right(3))) should be(Left("Error"))
