package fpinscala
package datastructures

enum MyOption[+A]:
  case MySome(get: A)
  case MyNone

  def map[B](f: A => B): MyOption[B] = this match
    case MySome(a) => MySome(f(a))
    case MyNone => MyNone

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f).getOrElse(MyNone)

  def getOrElse[B >: A](default: => B): B = this match
    case MySome(a) => a
    case MyNone => default

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(MySome(_)).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if f(a) then MySome(a) else MyNone)

object MyOption:
  def apply[A](a: A): MyOption[A] = if(a == null) MyNone else MySome(a)

  def mean(xs: Seq[Double]): MyOption[Double] =
    if xs.isEmpty then MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

