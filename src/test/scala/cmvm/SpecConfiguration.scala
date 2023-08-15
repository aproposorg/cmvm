package cmvm

import scala.util.Random

/** Reusable test data and configurations for software and hardware tests
 * 
 * @note Currently implements:
 *       - a hardcoded example matrix
 *       - a random collection of matrix sizes
 *       - a sequence of `P` values
 *       - a sequence of `E` values
 *       - a sequence of `nBits` values
 *       - a collection of random matrices (lazily generated to match the
 *         above sizes)
 * All tests assume `minSqnr` = 45 dB.
 */
trait SpecConfiguration {
  // A common RNG for generation is kept here
  final val rng: Random = new Random(42)

  // Collection of matrix sizes
  final val Sizes: Seq[(Int, Int)] = {
    // Some control parameters
    val MinLog2Size  = 4
    val MaxLog2Size  = 5 // @todo 8
    val NoRandoms    = 10
    // ... some common matrix sizes from the original paper and ML workloads
    val common = (MinLog2Size to MaxLog2Size).flatMap { r => (r to MaxLog2Size).map (c => (1 << r, 1 << c)) }
    // ... some random matrix sizes
    val random = (0 until NoRandoms).map { _ =>
      val r = rng.between(1 << MinLog2Size, 1 << MaxLog2Size)
      val c = rng.between(1 << MinLog2Size, 1 << MaxLog2Size)
      (r, c)
    }
    random // @todo common ++ random
  }

  // Collection of hardcoded example matrices
  final val Example: Array[Array[Double]] = Array(
    Array(.5377, .3188), Array(1.8339, -1.3077), Array(-2.2588, -.4336), Array(.8622, .3426)
  )

  // Collection of randomly generated matrices
  final lazy val Randoms: Seq[Array[Array[Double]]] = Sizes.map { case (r, c) => rand(r, c) }

  // Sequence of `P` values
  final val Ps: Seq[Int] = (2 to 2).toSeq // @todo (2 to 4).toSeq

  // Sequence of `E` values
  final val Es: Seq[Int] = (2 to 2).toSeq // @todo (1 to 4).toSeq

  // Sequence of `nBits` values
  final val NBits: Seq[Int] = (4 to 4).toSeq // @todo (4 to 6).toSeq

  // Constant `minSqnr` value
  final val MinSqnr = 45

  /** Generate a random matrix of the given size (r, c)
   * 
   * @note Assumes a Gaussian distribution of elements.
   */
  def rand(size: (Int, Int)): Array[Array[Double]] = rand(size._1, size._2)

  /** Generate a random matrix of the given size (r, c)
   * 
   * @note Assumes a Gaussian distribution of elements.
   */
  def rand(r: Int, c: Int): Array[Array[Double]] = (0 until r).map { _ =>
    Array.fill(c) { rng.nextGaussian() }
  }.toArray
}
