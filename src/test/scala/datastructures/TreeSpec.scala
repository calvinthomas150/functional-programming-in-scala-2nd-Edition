package fpinscala
package datastructures

import Tree.{Branch, Leaf}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TreeSpec extends AnyFlatSpec with should.Matchers {
  val tree :Tree[Int] = Branch(Branch(Leaf(1), Leaf(5)), Leaf(3))

  "A tree" should "return the maximum value from an integer tree based on the highest leaf value" in {
    tree.maximum should be (5)
  }

  "it" should "have size equal to the number of branches and leaves in the tree" in {
    tree.size should be (5)
  }

  "it" should "give the maximum depth of the tree from the root node via the depth function" in {
    tree.depth should be (2)
  }

  "it" should "allow the map function to apply a function to all the values in every Leaf" in {
    tree.map(_ + 1) should be (Branch(Branch(Leaf(2), Leaf(6)), Leaf(4)))
  }

  "it" should "give the same results for v2 of size, depth, maximum and map functions as the original versions" in {
    tree.size should be (tree.size_v2)
    tree.depth should be (tree.depth_v2)
    tree.maximum should be (tree.maximum_v2)
    tree.map(_ + 1) should be (tree.map_v2(_ + 1))
  }
}