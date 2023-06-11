package fpinscala
package datastructures

import datastructures.Tree.Branch

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l,r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g : (B,B) => B): B = this match
    case Leaf(x) => f(x)
    case Branch(l,r) => g(l.fold(f,g), r.fold(f,g))

  def size_v2: Int = fold(_ => 1, _ + _ + 1)

  def depth_v2: Int = fold(_ => 0, (a, b) => (a max b) + 1)

  def map_v2[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))

object Tree:
  extension (t: Tree[Int])
    def maximum : Int = t match
      case Leaf(i) => i
      case Branch(l,r) => l.maximum.max(r.maximum)

    def maximum_v2: Int = t.fold(a => a, _ max _)
