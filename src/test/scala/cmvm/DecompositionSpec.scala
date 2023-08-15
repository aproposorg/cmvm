package cmvm

import scala.collection.mutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecompositionSpec extends AnyFlatSpec with Matchers with SpecConfiguration {
  behavior of "Matrix decomposition"

  /** START BASIC SANITY CHECKS */
  def equal(arr1: Array[Array[Double]], arr2: Array[Array[Double]]): Boolean = {
    (arr1.length == arr2.length) && arr1.zip(arr2).forall { case (av, bv) =>
      (av.length == bv.length) && av.zip(bv).forall { case (a, b) => a == b }
    }
  }
  it should "slice matrices correctly" in {
    for (e <- Es; mtrx <- Randoms) {
      val slices = slice(mtrx, computeN(e))
      val flat   = (0 until slices.head.length).foldLeft(Array.empty[Array[Double]]) { case (mtrxAcc, r) =>
        mtrxAcc :+ slices.foldLeft(Array.empty[Double]) { case (rowAcc, slc) =>
          rowAcc ++ slc(r)
        }
      }
      assert(equal(mtrx, flat))
    }
  }

  it should "fail to decompose empty matrices" in {
    an [IllegalArgumentException] should be thrownBy(decompose(Array.empty[Array[Double]]))
    an [IllegalArgumentException] should be thrownBy(decompose(Array(Array.empty[Double])))
  }

  it should "fail to decompose a non-rectangular matrix" in {
    val triangular = (0 until 4).map { r => Array.fill(r)(0.0) }.toArray
    an [IllegalArgumentException] should be thrownBy(decompose(triangular))
  }

  it should "produce a correctly sized example decomposition" in {
    val dec = decompose(Example, p = 2, e = 2, numBits = 6, minSqnr = MinSqnr)
    dec should have size 1
    dec.head should have size 3
    dec.head.dropRight(1).foreach { fctr =>
      fctr should have size 4
      fctr.foreach { row => row should have size 4 }
    }
    dec.head.last should have size 4
    dec.head.last.foreach { _ should have size 2 }
  }
  /** END BASIC SANITY CHECKS */

  /** START ADHERENCE CHECKS */
  // Generate all the decompositions for various configurations of parameters
  type Decomposition = Array[Array[Array[Array[Double]]]]
  type DecSpec       = (Int, Int, Int, Array[Array[Double]], Decomposition)
  lazy val decs: Seq[DecSpec] = Ps.flatMap { p =>
    Es.flatMap { e =>
      NBits.flatMap { numBits =>
        Randoms.map { mtrx =>
          val dec = decompose(mtrx, p = p, e = e, numBits = numBits, minSqnr = MinSqnr)
          (p, e, numBits, mtrx, dec)
        }
      }
    }
  }

  // Check that the first matrix factor in each slice is always trivial
  type MFactor = Array[Array[Double]]
  def isTrivial(fctr: MFactor): Boolean = {
    !fctr.isEmpty && !fctr(0).isEmpty && fctr.zipWithIndex.forall { case (row, i) =>
      if (i < row.length) {
        row.count(_ != 0.0) == 1 && row(i) == 1.0
      } else {
        row.count(_ != 0.0) == 0
      }
    }
  }
  it should "always produce trivial matrix factors" in {
    val failures = mutable.ArrayBuffer.empty[(DecSpec, MFactor)]
    for ((p, e, numBits, mtrx, dec) <- decs) {
      var failed = false
      for (slc <- dec if !failed && !isTrivial(slc.last)) {
        failed = true
        failures.append(((p, e, numBits, mtrx, null), slc.last))
      }
    }
    assert(failures.isEmpty,
      failures.map { case ((p, e, numBits, mtrx, _), trv) =>
        s"""Matrix decomposition of ${fctr2FlatString(mtrx)} for P=$p, E=$e, and numBits=$numBits
            |  has least significant matrix factor that is non-trivial: ${fctr2FlatString(trv)}""".stripMargin
      }.mkString("\n"))
  }

  // Check the number of non-trivial matrix factors
  type Slice = Array[Array[Array[Double]]]
  def isValidSlice(slc: Slice, p: Int): Boolean = (slc.length - 1) == p
  it should "adhere to value P" in {
    val failures = mutable.ArrayBuffer.empty[(DecSpec, Slice)]
    for ((p, e, numBits, mtrx, dec) <- decs) {
      val invalids = dec.filter(!isValidSlice(_, p))
      if (!invalids.isEmpty) failures.append(((p, e, numBits, mtrx, null), invalids.head))
    }
    assert(failures.isEmpty,
      failures.map { case ((p, e, numBits, mtrx, _), slc) =>
        s"""Matrix decomposition of ${fctr2FlatString(mtrx)} for P=$p, E=$e, and numBits=$numBits
            |  has invalid slice: ${slc2String(slc)}""".stripMargin
      }.mkString("\n"))
  }

  // Check the maximum number of non-zero elements in a matrix factor row
  def isValidRow(row: Array[Double], e: Int): Boolean = row.count(_ != 0.0) <= e
  it should "adhere to value E" in {
    val failures = mutable.ArrayBuffer.empty[(DecSpec, (Int, Int, Array[Double]))]
    for ((p, e, numBits, mtrx, dec) <- decs) {
      var failed = false
      for ((slc, slcInd) <- dec.zipWithIndex if !failed) {
        for ((fctr, fctrInd) <- slc.reverse.zipWithIndex if !failed) {
          for (row <- fctr if !failed && !isValidRow(row, e)) {
            failed = true
            failures.append(((p, e, numBits, mtrx, null), (slcInd, fctrInd, row)))
          }
        }
      }
    }
    assert(failures.isEmpty,
      failures.map { case ((p, e, numBits, mtrx, _), (slcInd, fctrInd, row)) =>
        s"""Matrix decomposition of ${fctr2FlatString(mtrx)} for P=$p, E=$e, and numBits=$numBits
            |  has invalid row: ${vec2String(row)} in slice ${slcInd}, matrix factor ${fctrInd}""".stripMargin
      }.mkString("\n"))
  }

  // Check the number of bits used to represent matrix factor elements
  it should "adhere to value numBits" in {
    val failures = mutable.ArrayBuffer.empty[(DecSpec, (Int, Int, Double))]
    for ((p, e, numBits, mtrx, dec) <- decs) {
      val factors = generateFactors(numBits).toSet
      var failed  = false
      for ((slc, slcInd) <- dec.zipWithIndex if !failed) {
        for ((fctr, fctrInd) <- slc.reverse.zipWithIndex if !failed) {
          for (row <- fctr if !failed) {
            for (elem <- row if !failed && elem != 0.0 && !factors.contains(elem)) {
              failed = true
              failures.append(((p, e, numBits, mtrx, null), (slcInd, fctrInd, elem)))
            }
          }
        }
      }
    }
    assert(failures.isEmpty,
      failures.map { case ((p, e, numBits, mtrx, _), (slcInd, fctrInd, elem)) =>
        s"""Matrix decomposition of ${fctr2FlatString(mtrx)} for P=$p, E=$e, and numBits=$numBits
            |  has invalid factor: ${elem} in slice ${slcInd}, matrix factor ${fctrInd}""".stripMargin
      }.mkString("\n"))
  }
  /** END ADHERENCE CHECKS */

  /** START VALIDITY CHECKS */

  /**
   * @todo Still not completely sure how to check whether the output
   *       matrices from the decomposition flow are "correct". Maybe
   *       correlation indeed is a good metric?
   */

  /** END VALIDITY CHECKS */
}
