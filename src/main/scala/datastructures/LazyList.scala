package fpinscala
package datastructures

import LazyList.*

import scala.Option
import scala.Option.*
import scala.List
import scala.annotation.tailrec


enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    @tailrec
    def loop(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Cons(h,t) => loop(t(), h() :: acc)
      case Empty => acc.reverse

    loop(this, Nil)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), LazyList.empty)
    case _ => LazyList.empty

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty)((a,acc) =>
      if p(a) then cons(a, acc) else LazyList.empty)
    /* Or directly
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => LazyList.empty
    */

  def headOption: Option[A] =
    foldRight(None:Option[A])((a,_) => Some(a))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A]):
      (a, acc) => if p(a) then cons(a, acc) else acc

  def append[A2 >: A](ll: => LazyList[A2]): LazyList[A2] =
    foldRight(ll:LazyList[A2]):
      (a, acc) => cons(a, acc)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a,acc) => f(a).append(acc))


  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _ => acc

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

object LazyList:

  /** This is an example of a smart constructor, by convention they lower case the first letter of the
      real constructor name. They're used when we want to ensure some additional invariant or provide a
      different signature. Here it is used for the memoization of the head and tail and ensures the thunk
      only does its worth once.
  */
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  // In this case the benefit of the smart constructor is to annotate the empty case as a LazyList which helps
  // with type inference
  def empty[A]: LazyList[A] = Empty

  // Note this is strict, the whole of as will be evaluated!
  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

