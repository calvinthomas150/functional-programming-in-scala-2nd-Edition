package fpinscala
package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Option.*
class OptionSpec extends AnyFlatSpec with should.Matchers:

  val someValue = Option(7.0)
  val none = Option(null)

  val sqrt: Double => Option[Double] =
    (a: Double) => if (a < 0) None else Some(Math.sqrt(a))

  "Option" should "be None when null is passed to its apply method" in:
    None should be(None)

  it should "be a Some when a value other than null is passed to its apply method" in:
    someValue should be(Some(7))

  it should "return the value from within it when the it is a Some and getOrElse is called" in:
    someValue.getOrElse(0) should be(7.0)

  it should "return the default value passed in when the it is a None and getOrElse is called" in :
    None.getOrElse(0) should be(0)

  it should "return the original MyOption when the it is a Some and orElse is called" in :
    someValue.orElse(Some(10)) should be(Some(7.0))

  it should "return the default MyOption passed in when the it is a None and orElse is called" in :
    None.orElse(Some(0)) should be(Some(0))

  it should "transform the value within a Some by the provided function when the map method is called" in:
    Option(7).map(_ + 3) should be(Some(10))

  it should "be none when the original MyOption is None when map is called with a function" in:
    None.map(_ => someValue) should be(None)

  it should "apply the function from the flatMap returning a new MyOptio" in:
    someValue.flatMap(sqrt) should be(Some(Math.sqrt(someValue.getOrElse(0))))

  it should "apply the function from the flatMap returning a new None when the original value is None" in:
    None.flatMap(_ => someValue) should be(None)

  it should "return the original Some when it is called with a predicate function that the value in the Some passes for" in:
    someValue.filter(_ > 5) should be(someValue)

  it should "return None when it is called with a predicate function that doesn't pass for the MyOption" in:
    someValue.filter(_ < 5) should be(None)

  it should "return the correct mean and variance for a sequence of doubles" in:
    val doubles = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    mean(doubles) should be(Some(3.0))
    variance(doubles) should be (Some(2.0))

  it should "combine two option values using a binary function" in:
    val sum = (a:Double, b:Double) => a + b
    map2(someValue, someValue)(sum) should be(Some(14.0))
    map2(None, someValue)((a:Null, b:Double) => b) should be(None)
    map2(someValue, None)((a:Double, b:Null) => a) should be(None)
    map2(None, None)((a:Null, b:Null) => a) should be(None)



