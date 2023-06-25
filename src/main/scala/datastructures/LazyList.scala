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

  def take(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), 1) => Some((h(), (LazyList.empty, 0)))
      case (Cons(h, t), n) if n >= 1 => Some(h(), (t(), n - 1))
      case _ => None
    /* or directly
    this match
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), LazyList.empty)
      case _ => LazyList.empty
    */

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None

    /* Or via foldRight
    foldRight(LazyList.empty)((a,acc) =>
      if p(a) then cons(a, acc) else LazyList.empty)
  
    or directly
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => LazyList.empty
    */

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) =>
        Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))

  def headOption: Option[A] =
    foldRight(None:Option[A])((a,_) => Some(a))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def map[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None

    /* Or via foldRight
    foldRight(LazyList.empty[B])((a, acc) => cons(f(a), acc))
    */

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

  val ones: LazyList[Int] =
    unfold(())(_ => Some((1, ())))
    /* or directly
    cons(1, ones)

    or via continally
    continually(1)
    */


  def continually[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a, ())))
    /* or directly
    cons(a, continually(a))
    */

  def from(n: Int):LazyList[Int] =
    unfold(n)(x => Some(x, x + 1))

  /* or directly
    cons(n, from(n+1)) */

  val fibs: LazyList[Int] =
    unfold((0, 1)):
      case (current, next) => Some((current, (next, current + next)))
    // or directly
    /*def loop(curr:Int, next:Int): LazyList[Int] =
      cons(curr, loop(next, curr + next))
    loop(0,1)*/

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => LazyList.empty

    // Note this is strict, the whole of as will be evaluated!
  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then LazyList.empty
    else cons(as.head, apply(as.tail*))

