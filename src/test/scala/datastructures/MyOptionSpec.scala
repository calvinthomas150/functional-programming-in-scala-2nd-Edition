package fpinscala
package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import MyOption.*
class MyOptionSpec extends AnyFlatSpec with should.Matchers:

  val mySomeValue = MyOption(7.0)
  val myNone = MyOption(null)

  val sqrt: Double => MyOption[Double] =
    (a: Double) => if (a < 0) MyNone else MySome(Math.sqrt(a))

  "MyOption" should "be MyNone when null is passed to its apply method" in:
    myNone should be(MyNone)

  it should "be a MySome when a value other than null is passed to its apply method" in:
    mySomeValue should be(MySome(7))

  it should "return the value from within it when the it is a MySome and getOrElse is called" in:
    mySomeValue.getOrElse(0) should be(7.0)

  it should "return the default value passed in when the it is a MyNone and getOrElse is called" in :
    myNone.getOrElse(0) should be(0)

  it should "return the original MyOption when the it is a MySome and orElse is called" in :
    mySomeValue.orElse(MySome(10)) should be(MySome(7.0))

  it should "return the default MyOption passed in when the it is a MyNone and orElse is called" in :
    myNone.orElse(MySome(0)) should be(MySome(0))

  it should "transform the value within a MySome by the provided function when the map method is called" in:
    MyOption(7).map(_ + 3) should be(MySome(10))

  it should "be none when the original MyOption is MyNone when map is called with a function" in:
    myNone.map(_ => mySomeValue) should be(myNone)

  it should "apply the function from the flatMap returning a new MyOptio" in:
    mySomeValue.flatMap(sqrt) should be(MySome(Math.sqrt(mySomeValue.getOrElse(0))))

  it should "apply the function from the flatMap returning a new MyNone when the original value is MyNone" in:
    myNone.flatMap(_ => mySomeValue) should be(MyNone)

  it should "return the original MySome when it is called with a predicate function that the value in the MySome passes for" in:
    mySomeValue.filter(_ > 5) should be(mySomeValue)

  it should "return None when it is called with a predicate function that doesn't pass for the MyOption" in :
    mySomeValue.filter(_ < 5) should be(myNone)

  it should "return the correct mean and variance for a sequence of doubles" in:
    val doubles = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    mean(doubles) should be(MySome(3.0))
    variance(doubles) should be (MySome(2.0))



