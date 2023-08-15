import scala.math.{abs, log10, min, pow, sqrt}
import scala.collection.mutable

import chisel3.getVerilogString
import chisel3.util.isPow2

package object cmvm {
  /** Convert a 1D vector to a flat string
   * @param v the vector
   * @return a string representation of `v`
   */
  private[cmvm] def vec2String[T](v: Array[T]): String = v.mkString("[", ", ", "]")

  /** Convert a 2D array to a multi-line string
   * @param fctr the 2D array
   * @return a multi-line string representation of `fctr`
   */
  private[cmvm] def fctr2String(fctr: Array[Array[Double]]): String = fctr.map(vec2String(_)).mkString("[", "\n", "]")

  /** Convert a 2D array to a one-line string
   * @param fctr the 2D array
   * @return a one-line string representation of `fctr`
   */
  private[cmvm] def fctr2FlatString(fctr: Array[Array[Double]]): String = fctr.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")

  /** Convert a 3D array to a multi-line string with rows of internal
   * 2D arrays aligned horizontally
   * @param slc the 3D array
   * @return a multi-line string representation of `slc`
   */
  private[cmvm] def slc2String (slc: Array[Array[Array[Double]]]): String = ???

  /** Convert a 4D array to a multi-line string with internal 3D arrays
   * aligned vertically
   * @param dec the 4D array
   * @return a multi-line string representation of `dec`
   */
  private[cmvm] def dec2String (dec: Array[Array[Array[Array[Double]]]]): String = dec.map(slc2String(_)).mkString("[", "\n", "]")

  /** Helpful vector operations
    * @param v a vector
    */
  private[cmvm]implicit class VectorOps(v: Array[Double]) {
    def +(that: Array[Double]): Array[Double] = {
      require(this.v.length == that.length)
      this.v.zip(that).map { case (a, b) => a + b }
    }

    def -(that: Array[Double]): Array[Double] = {
      require(this.v.length == that.length)
      this.v.zip(that).map { case (a, b) => a - b }
    }

    def *(scale: Double): Array[Double] = this.v.map(a => scale * a)
  }

  /** Compute the base-2 logarithm of number
    * @param v the number
    * @return `log2(v)`
    */
  private[cmvm] def log2(v: Double): Double = log10(v)/log10(2.0)

  /** Convert a number to Decibel
    * @param v the number
    * @return `10 * log10(v)`
    */
  private[cmvm] def todB(v: Double): Double = 10 * log10(v)

  /** Compute the 0-norm of a vector
    * @param vs the vector
    * @return the 0-norm (`sum[i](vs[i] != 0)`) of `vs`
    */
  private[cmvm] def norm0(vs: Array[Double]): Double = vs.count(_ != 0.0)

  /** Compute the 2-norm of a vector
    * @param vs the vector
    * @return the 2-norm (`sqrt(sum[i](vs(i) ^ 2.0))`) of `vs`
    */
  private[cmvm] def norm2(vs: Array[Double]): Double = sqrt(vs.map(pow(_, 2.0)).sum)

  /** Compute the Frobenius norm of a matrix
    * @param ms the matrix
    * @return the Frobenius norm (`sqrt(sum[i](sum[j](ms[i,j] ^ 2.0)))`) of `ms`
    */
  private[cmvm] def normF(ms: Array[Array[Double]]): Double = sqrt(ms.flatMap(_.map(pow(_, 2.0))).sum)

  /** Generate an all-zero vector
    * @param cols the length of the vector
    * @return a vector of `cols` zeros
    */
  private[cmvm] def zeros(cols: Int): Array[Double] = Array.fill(cols)(0.0)

  /** Generate an all-zero matrix
    * @param rows the number of rows
    * @param cols the number of cols
    * @return a matrix of `[rows x cols]` zeros
    */
  private[cmvm] def zeros(rows: Int, cols: Int): Array[Array[Double]] = Array.fill(rows)(zeros(cols))

  /** Generate an identity matrix
    * @param num the number of rows and columns
    * @return an identity matrix of size `[num x num]`
    */
  private[cmvm] def identity(num: Int): Array[Array[Double]] = {
    (0 until num).map(i => zeros(i) :+ 1.0 :++ zeros(num-i-1)).toArray
  }

  /** Evaluate if two vectors are element-wise equal
    * @param v1 the first vector
    * @param v2 the second vector
    * @return true iff the vectors are equal
    * 
    * @note Returns false if `v1` and `v2` have different lengths.
    */
  private[cmvm] def equal(v1: Array[Double], v2: Array[Double]): Boolean = {
    (v1.length == v2.length) && v1.zip(v2).forall { case (a, b) => a == b }
  }

  /** Compute the dot product of two vectors
    * @param v1 the first vector
    * @param v2 the second vector
    * @return the dot product (`sum[i](v1[i] * v2[i])`)
    * 
    * @note The vectors `v1` and `v2` must have the same length.
    * @note Returns 0 if the vectors are empty.
    */
  private[cmvm] def dot(v1: Array[Double], v2: Array[Double]): Double = {
    require(v1.length == v2.length)
    v1.zip(v2).map { case (a, b) => a * b }.sum
  }

  /** Compute the dot product of a vector with a matrix
    * @param v the vector
    * @param m the matrix
    * @return the dot product (`dot(v, m[:,0]) & ... & dot(v, m(:,N-1))`)
    * 
    * @note The matrix `m` must be rectangular.
    * @note The vector `v` must have length equal to the number of rows of `m`.
    */
  private[cmvm] def dot(v: Array[Double], m: Array[Array[Double]]): Array[Double] = {
    require(!m.isEmpty && v.length == m.length && m.drop(1).forall(_.length == m(0).length))
    (0 until m(0).length).map(col => dot(v, m.map(_(col)).toArray)).toArray
  }

  /** Compute the dot product of a vector with a factorized matrix
   * @param v the vector
   * @param m the factorized matrix
   * @return the dot product
   * 
   * @note The matrix `m` must be uniform in the number of slices and matrix
   *       factors per slice. The sum of the number of columns in the slices'
   *       trivial matrix factors must equal the length of `v`.
   */
  private[cmvm] def dotCMVM(v: Array[Double], m: Array[Array[Array[Array[Double]]]]): Array[Double] = {
    val flat = flatten(m)
    assume(!flat.isEmpty && v.length == flat(0).length)
    dot(v, flat)
  }

  /** Compute the dot product of two matrices
    * @param m1 the first matrix
    * @param m2 the second matrix
    * @return the dot product (`dot(m1[0,:], m2[:,0]) & ... & dot(m1[M-1,:], m2[:,P-1])`)
    * 
    * @note The matrices `m1` and `m2` must be rectangular.
    * @note The rows of matrix `m1` must have length equal to the number of rows of `m2`.
    */
  private[cmvm] def dot(m1: Array[Array[Double]], m2: Array[Array[Double]]): Array[Array[Double]] = {
    require(!m1.isEmpty && !m2.isEmpty && m1(0).length == m2.length &&
      m1.drop(1).forall(_.length == m1(0).length) && m2.drop(1).forall(_.length == m2(0).length))
    m1.map(v => dot(v, m2))
  }

  /** Flatten a factorized matrix
   * @param m the factorized matrix
   * @return the flattened version of the factorized matrix
   */
  private[cmvm] def flatten(m: Array[Array[Array[Array[Double]]]]): Array[Array[Double]] = {
    require(!m.isEmpty && !m(0).isEmpty && m.drop(1).forall(slc => !slc.isEmpty && slc.head.length == m(0).head.length))
    val flatSlices = m.map { slc =>
      slc.tail.foldLeft(slc.head) { case (acc, fctr) => dot(acc, fctr) }
    }
    (0 until flatSlices.head.length).map { i =>
      flatSlices.foldLeft(Array.empty[Double]) { case (acc, fctr) => acc ++ fctr(i) }
    }.toArray
  }

  /** Compute the Euclidean distance between two vectors
    * @param v1 the first vector
    * @param v2 the second vector
    * @return the Euclidean distance (`norm2(v1 - v2)`)
    */
  private[cmvm] def euclideanDistance(v1: Array[Double], v2: Array[Double]): Double = norm2(v1 - v2)

  /** Compute the signal-to-quantization-noise ratio between two vectors
    * @param v1 the first vector
    * @param v2 the second vector
    * @return the signal-to-quantization-noise ratio (`(norm2(v1) ^ 2.0) / (norm2(v1-v2) ^ 2.0)`)
    * 
    * @note The vectors `v1` and `v2` must have the same length.
    */
  private[cmvm] def sqnr(v1: Array[Double], v2: Array[Double]): Double = {
    pow(norm2(v1), 2.0) / pow(norm2(v1 - v2), 2.0)
  }

  /** Compute the signal-to-quantization-noise ratio between two matrices
    * @param m1 the first matrix
    * @param m2 the second matrix
    * @return the signal-to-quantization-noise ratio (`(normF(m1) ^ 2.0) / (normF(m1-m2) ^ 2.0)`)
    * 
    * @note The matrices `m1` and `m2` must have the same size.
    */
  private[cmvm] def sqnr(m1: Array[Array[Double]], m2: Array[Array[Double]]): Double = {
    require(!m1.isEmpty && !m2.isEmpty && m1.length == m2.length && m1(0).length == m2(0).length &&
      m1.zip(m2).forall { case (v1, v2) => v1.length == v2.length })
    pow(normF(m1), 2.0) / pow(normF(m1.zip(m2).map { case (v1, v2) => v1 - v2 }), 2.0)
  }

  /** Generate a "dictionary" of signed powers-of-two factors used for decomposition
    * @param width the number of bits used to represent factors
    * @return a vector of sorted factors
    * 
    * @note Requires the width to be at least two bits to represent factors in 
    *       some combined sign-magnitude and two's complement inspired format.
    */
  private[cmvm] def generateFactors(width: Int): Array[Double] = {
    require(width >= 2, 
      "cannot generate powers of two factors for representation with fewer than two bits")
    val positives = (0 until ((1 << (width-1)) - 2)).map(v => pow(2.0, v - ((1 << (width-2)) - 1)))
    val negatives = positives.map(-_)
    (positives :+ 0.0 :++ negatives).sortWith { case (a, b) => a < b }.toArray
  }

  /** Recursively generate all valid extensions to a given matching by one more 
    * non-zero element picked for a dictionary of factors
    * @param phi the matching to extend upon
    * @param w the vector to match against
    * @param factors the dictionary of factors
    * @param codebook the current coding matrix used as basis for the matching
    * @param e the number of non-zero elements in a matrix factor row
    * @return a list of valid matchings for `w` with (at most) `E` non-zero elements
    * 
    * @note This function should be considered a helper and not called outside 
    *       the scope of `decomposeSlice`.
    */
  private[cmvm] def generateMatchings(phi: Array[Double], w: Array[Double], factors: Array[Double],
                                      codebook: Array[Array[Double]], e: Int): Array[Array[Double]] = {
    // We keep a mutable collection of matchings here
    val matchingSets = mutable.ArrayBuffer.empty[Array[Array[Double]]]

    // For each column in the weight vector, we want to select the set of best 
    // matchings with the best factors for each vector entry (skip columns whose
    // result is 0)
    for (col <- 0 until w.length if w(col) != 0) {
      // Iterate over the rows of the codebook (skip rows whose input vector
      // entry is non-zero)
      val colMatchings = mutable.ArrayBuffer.empty[Array[Double]]
      for (row <- 0 until codebook.length if phi(row) == 0.0) {
        // Extract the current row of the original codebook
        val codeRow = codebook(row)
        // Precompute the dot product of the input vector and this codebook
        val precProd = dot(phi, codebook)
        // Iterate over the factors to find the best one
        val (_, bestFactor) = factors
          .foldLeft((Double.PositiveInfinity, Double.NegativeInfinity)) { case ((bDst, bFctr), fctr) =>
          // Scale the row of the codebook and add it to the precomputed result
          val est = (codeRow * fctr) + precProd

          // Compute and compare the Euclidean distance to the weight vector
          val dst = euclideanDistance(w, est)
          if (dst < bDst) (dst, fctr) else (bDst, bFctr)
        }
        // Create a corresponding matching
        val bestMatching  = phi.clone()
        bestMatching(row) = bestFactor

        // Store the best matching (if it is not equal to the input vector)
        if (!equal(phi, bestMatching)) {
          colMatchings += bestMatching
        }
      }

      // Store the best matchings
      matchingSets += colMatchings.toArray
    }

    // The intersection of the selected sets contains viable matchings that fit 
    // all entries in the weight vector
    val validMatchings = {
      if (matchingSets.isEmpty) {
        Array.empty[Array[Double]]
      } else {
        matchingSets.drop(1).foldLeft(matchingSets(0)) {
          case (acc, mtchset) => acc.filter(vm => !mtchset.filter(pm => equal(vm, pm)).isEmpty)
        }
      }
    }

    // If there are no valid matchings, return the input vector
    if (validMatchings.isEmpty) {
      Array(phi)
    } else if (!validMatchings.filter(norm0(_) == min(w.length, e)).isEmpty) {
      // If any of the valid matchings has E non-zero entries, stop recursion
      validMatchings
    } else {
      // Otherwise, recurse on the valid matchings and return the union of the 
      // returned results
      validMatchings.foldLeft(Array.empty[Array[Double]]) { case (acc, mtch) =>
        acc ++ generateMatchings(mtch, w, factors, codebook, e)
      }
    }
  }

  /** Decompose a matrix slice using computation coding with a given dictionary 
    * of valid factors
    * @param slice the matrix slice
    * @param factors the dictionary of factors
    * @param p the number of non-trivial matrix factors
    * @param e the number of non-zero elements in a matrix factor row
    * @param minSqnr minimum signal-to-quantization-noise ratio for which to 
    *                terminate the algorithm early
    * @return a list `P+1` of matrix factors to make up the slice
    */
  private[cmvm] def decomposeSlice(slice: Array[Array[Double]], factors: Array[Double],
                                   p: Int, e: Int, minSqnr: Int): Array[Array[Array[Double]]] = {
    require(!slice.isEmpty && !slice(0).isEmpty, "cannot decompose an empty slice")
    require(slice(0).length <= 8,
      "cannot decompose matrix slices with more than eight columns, use decompose to slice the matrix")
    // Get the first dimension of the matrix
    val m = slice.length

    if (p == 0) {
      // If P is zero, we return the trivial matrix factor
      val (rows, cols) = (slice.length, slice(0).length)
      val id  = identity(cols)
      Array(if (rows < cols) id.take(rows) else id :++ zeros(rows - cols, cols))
    } else {
      // Otherwise we do the processing to generate the relevant matrix factors

      // To generate this matrix factor, we need the P-1 previous ones, thus 
      // we start by generating those and use them to generate this step's codebook
      val previousMFactors = decomposeSlice(slice, factors, p-1, e, minSqnr)
      val codebook = previousMFactors.reduce(dot(_, _))
      
      // Now we're ready to compute this matrix factor
      val res = Array.fill(m)(zeros(m))

      // Run through the rows of the matrix till it is of sufficient quality
      for (row <- 0 until m if !(todB(sqnr(slice, dot(res, codebook))) >= minSqnr)) {
        // Generate "all matchings" and pick the one with the least 
        // Euclidean distance to the original
        val matchings = generateMatchings(zeros(m), slice(row), factors, codebook, e)
        val (_, bestMatching) = matchings
          .foldLeft((Double.PositiveInfinity, zeros(m))) { case ((bDst, bMtch), mtch) =>
          // Compute the Euclidean distance
          val dst = euclideanDistance(slice(row), dot(mtch, codebook))

          // Update the best distance and matching accordingly
          if (dst < bDst) (dst, mtch) else (bDst, bMtch)
        }

        // Store the best matching in the result
        res(row) = bestMatching
      }

      // Return this matrix factor and the previous ones
      Array(res) ++ previousMFactors
    }
  }

  /** Slice a given matrix column-wise
    * @param target the matrix
    * @param wSlice the width of the slices
    * @return a list of matrix slices of width `wSlice`, except the last slice 
    *         that may be narrower if `target`s width does not divide by `wSlice`
    */
  private[cmvm] def slice(target: Array[Array[Double]], wSlice: Int): Array[Array[Array[Double]]] = {
    require(!target.isEmpty && !target(0).isEmpty, "cannot slice empty matrix")
    require(wSlice >= 1, "cannot create matrix slices with width less than one")
    val cols = target(0).length
    val slices = mutable.ArrayBuffer.empty[Array[Array[Double]]]
    // Create full-width slices
    (0 until cols/wSlice).foreach { offsetFactor => 
      slices += target.map(_.drop(offsetFactor*wSlice).take(wSlice))
    }
    // If any columns are left over, they form a narrower slice
    if (wSlice*(cols/wSlice) != cols) {
      slices += target.map(_.drop((cols/wSlice)*wSlice))
    }
    slices.toArray
  }

  /** Simple heuristic to pick a slice width from the number of non-zero factors 
    * to use in each slice row
    * @param e the number of non-zero factors in a slice row
    * @return a heuristically picked slice width
    * 
    * @note Currently, we implement a simple heuristic that picks the slice width 
    *       as the nearest, greater power-of-two to `2*E` or at most 8.
    */
  private[cmvm] def computeN(e: Int): Int = {
    def _helper(e: Int, shft: Int = 0): Int = {
      if (e == 0) 1 << shft else _helper(e >> 1, shft+1)
    }
    val heur = if (isPow2(e)) 2*e else _helper(2*e)
    min(8, heur)
  }

  /** Decompose a matrix using computation coding with a given dictionary of 
    * valid factors
    * @param target the matrix
    * @param factors the dictionary of factors (will be generated if empty)
    * @param p the number of non-trivial matrix factors
    * @param e the number of non-zero elements in a matrix factor row
    * @param numBits the number of bits to use in the factor representations
    * @param minSqnr minimum signal-to-quantization-noise ratio for which to 
    *                terminate the algorithm early
    * @return a list of slices with `P+1` matrix factors each
    */
  def decompose(target: Array[Array[Double]], factors: Array[Double] = Array.empty[Double], p: Int = 2,
                e: Int = 2, numBits: Int = 6, minSqnr: Int = 45): Array[Array[Array[Array[Double]]]] = {
    require(!target.isEmpty && !target(0).isEmpty,
      "cannot decompose an empty matrix or a matrix with empty rows")
    require(p >= 0, "cannot generate a negative number of non-trivial matrix factors")
    require(e >= 1, "cannot use fewer than one non-zero elements in the matrix factor rows")
    require(numBits >= 2, "cannot use fewer than two bits to represent matrix factor elements")

    // If no factors are given, we generate them here
    val _factors = if (!factors.isEmpty) factors else generateFactors(numBits)

    // Depending on the size of the matrix, we slice it with different widths 
    // and adjust the E parameter accordingly
    val n = computeN(e)

    // Gather the slices and compute their decompositions one by one
    val slices = slice(target, n)

    // Now simply decompose each of the slices and return their results
    slices.map(decomposeSlice(_, _factors, p, e, minSqnr))
  }

  /** Decompose a matrix using computation coding with a given dictionary of
   * valid factors and generate its hardware representation
   * @param target the matrix
   * @param dataW the width of the data path in the hardware
   * @param pipe whether to pipeline the data path in the hardware
   * @param factors the dictionary of factors (will be generated if empty)
   * @param p the number of non-trivial matrix factors
   * @param e the number of non-zero elements in a matrix factor row
   * @param numBits the number of bits to use in the factor representations
   * @param minSqnr minimum signal-to-quantization-noise ratio for which to
   *                terminate the algorithm early
   * @return a string-formatted hardware description of the decomposed matrix
   */
  def generateConstant(target: Array[Array[Double]], dataW: Int, pipe: Boolean = false,
                       factors: Array[Double] = Array.empty[Double], p: Int = 2, e: Int = 2,
                       numBits: Int = 6, minSqnr: Int = 45): String = {
    generateConstant(decompose(target, factors, p, e, numBits, minSqnr), dataW, pipe, numBits)
  }

  /** Generate the hardware description of a matrix in computation coding
   * @param dec the decomposed matrix
   * @param dataW the width of the data path in the hardware
   * @param pipe whether to pipeline the data path in the hardware
   * @param numBits the number of bits to use in the factor representations
   * @return a string-formatted hardware description of the decomposed matrix
   */
  def generateConstant(dec: Array[Array[Array[Array[Double]]]], dataW: Int,
                       pipe: Boolean, numBits: Int): String = {
    getVerilogString(new ConstantCMVM(dec, numBits, dataW, pipe))
  }
}
