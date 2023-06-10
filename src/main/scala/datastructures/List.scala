package fpinscala
package datastructures

import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:

  def tail[A](xs: List[A]): List[A] = xs match
    case Nil => throw new NoSuchElementException("Tail of empty list!")
    case Cons(_, xs) => xs

  def setHead[A](h: A, as: List[A]): List[A] = as match
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match
    case Nil => as
    case Cons(x, xs) if n == 0 => as
    case Cons(x, xs) => drop(xs, n-1)

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => as
    case Cons(x, xs) => if f(x) then dropWhile(xs, f) else as

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def init[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def length[A](as: List[A]): Int = foldRight(as, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0, _ + _)

  def product(ds: List[Double]): Double =
    foldLeft(ds, 1.0, _ * _)

  def length_v2[A](as: List[A]): Int =
    foldLeft(as, 0, (acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, a:A) => Cons(a, acc))

  def foldRight_v2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b,a) => f(a,b))

  def append_v2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight_v2(a1, a2, (b, a) => Cons(b, a))

  def concat[A](xxs: List[List[A]]): List[A] =
    foldRight_v2(xxs, Nil: List[A], append_v2)

  def map[A, B](as: List[A], f: A => B): List[B] = as match
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs,f))

  def filter[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs, f))
    case Cons(x, xs) => filter(xs, f)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = as match
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs, f))

  def filter_v2[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def zipWith[A,B,C](a1: List[A], b1: List[B], f: (A,B) => C): List[C] = (a1, b1) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as, bs, f))

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

