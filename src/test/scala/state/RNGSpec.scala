package fpinscala
package state

import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import state.RNG
import state.SimpleRNG


class RNGSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks:

  val simpleRNGs: Gen[RNG] = for seed <- Arbitrary.arbitrary[Long] yield SimpleRNG(seed)

  "nonNegativeInt" should "always return a value >= 0" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val (i, _) = RNG.nonNegativeInt(rng)
        i should be >= 0

  "randomPair" should "return two random numbers" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val((i, j), _) = RNG.randomPair(rng)
        i should not be j

  "double" should "return a random double between 0 and 1 inclusive" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val(d, r) = RNG.double(rng)
        d should (be >= 0d and be <= 1d)

  "intDouble" should "return a random int and a random double between 0 and 1 inclusive that are not the same" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ((i,d), r) = RNG.intDouble(rng)
        d should (be >= 0d and be <= 1d)
        d should not be i.toDouble

  "doubleInt" should "return a random double between 0 and 1 inclusive and a random int that are not the same" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ((d, i), r) = RNG.doubleInt(rng)
        d should (be >= 0d and be <= 1d)
        d should not be i.toDouble

  "double3" should "return three random doubles, none of which should be the same" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ((d1, d2, d3), r) = RNG.double3(rng)
        d1 should (be >= 0d and be <= 1d)
        d2 should (be >= 0d and be <= 1d)
        d3 should (be >= 0d and be <= 1d)
        d1 should not be d2
        d1 should not be d3
        d2 should not be d3

  "ints" should "return n random ints, when called with a count of n" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ints = RNG.ints(1000)(rng)
        ints._1.size should be (1000)
        ints._1.distinct should be (ints._1)

  "nonNegativeEven" should "return a rqndom even integer" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val (i, _) = RNG.nonNegativeEven(rng)
        i should be >= 0
        i % 2 should be (0)

  "doubleViaMap" should "return a random double between 0 and 1 inclusive" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val (d, r) = RNG.doubleViaMap(rng)
        d should (be >= 0d and be <= 1d)

  "unit" should "keep the value and the state the same" in:
    forAll { (a: Int, seed: Long) =>
      val rng = SimpleRNG(seed)
      val (result, rng2) = RNG.unit(a)(rng)
      result should be(a)
      rng should be(rng2)
    }

  "randIntDouble" should "return a random int and a random double between 0 and 1 inclusive that are not the same" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ((i, d), r) = RNG.intDouble(rng)
        d should (be >= 0d and be <= 1d)
        d should not be i.toDouble

  "randDoubleInt" should "return a random double between 0 and 1 inclusive and a random int that are not the same" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ((d, i), r) = RNG.doubleInt(rng)
        d should (be >= 0d and be <= 1d)
        d should not be i.toDouble

  "intsWithSequence" should "return n random ints, when called with a count of n" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val ints = RNG.intsViaSequence(1000)(rng)
        ints._1.size should be(1000)
        ints._1.distinct should be(ints._1)

  "nonNegativeLessThan" should "return  random integer that is less than the supplied value" in:
    forAll(simpleRNGs):
      (rng: RNG) =>
        val int = RNG.nonNegativeLessThan(10)(rng)
        int._1 should be < 10