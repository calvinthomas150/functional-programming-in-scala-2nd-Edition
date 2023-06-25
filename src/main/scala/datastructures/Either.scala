package fpinscala
package datastructures


import scala.util.control.NonFatal
import Either.*

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e) => Left(e)

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => Right(a)
    case Left(e) => b

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for a <- this
        b <- that
    yield f(a, b)

object Either:
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(e) => Left(e)

  def traverse[E, A, B](as: scala.List[A])(f: A => Either[E, B]): Either[E, scala.List[B]] =
    as.foldRight[Either[E, scala.List[B]]](Right(scala.Nil))((a,b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](as: scala.List[Either[E, A]]): Either[E, scala.List[A]] =
    traverse(as)(identity)
