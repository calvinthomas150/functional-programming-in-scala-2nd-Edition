package fpinscala
package state

trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    if i >= 0 then (i, r)
    else (Math.abs(i + 1), r)

  def randomPair(rng: RNG): ((Int, Int), RNG) =
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)

  def intDouble(rng:RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

