package fpinscala
package datastructures
import Option.*

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None => None

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None => default

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

object Option:
  def apply[A](a: A): Option[A] = if(a == null) None else Some(a)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f:(A,B) => C): Option[C] =
    for(x <- a;
        y <- b) yield f(x,y)

    // Or a.flatMap(a => b.map(f(a,_))) without for comprehension syntax

  def traverse[A, B](as: scala.List[A])(f: A => Option[B]): Option[scala.List[B]] =
    as.foldRight(Some(Nil):Option[scala.List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](as: scala.List[Option[A]]): Option[scala.List[A]] =
    traverse(as)(identity)
    // or without traverse as.foldRight[MyOption[scala.List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

