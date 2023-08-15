package cmvm

import scala.math.abs
import scala.collection.mutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import chiseltest._

trait ConstantCMVMSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers with SpecConfiguration {
  // The hardware tests need a number of input vectors to test for ...
  final val RedNoInputVecs = 100
  final val NoInputVecs    = RedNoInputVecs * 100
  // ... and some common data bit widths
  final val BitWidths = Seq(8, 16, 32)

  // Generate all the decompositions for various configurations of parameters
  type MFactor       = Array[Array[Double]]
  type Slice         = Array[Array[Array[Double]]]
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

  /** Abstract the functionality of a factor's operation
   * @param fctr the factor
   */
  case class FactorOp(fctr: Double) {
    val isZero = fctr == 0.0
    val sgn    = fctr  < 0.0
    val shft   = log2(abs(fctr)).toInt

    /** Apply this factor to a piece of data
     * @param data the data to operate on
     * @param wData the width of the data
     */
    def perform(data: BigInt, wData: Int): BigInt = if (isZero) BigInt(0) else {
      val signum  = data.testBit(wData-1)
      val shftRes = if (shft == 0) data else if (shft < 0) data >> (-shft) else data << shft
      for (b <- wData-1 to wData+shft by -1 if shft < 0 && signum) shftRes.setBit(b)
      if (sgn) -shftRes else shftRes
    }
  }

  /** Apply a matrix factor to a vector of data
   * @param mFctr the matrix factor
   * @param v the vector of data
   * @param wData the width of the data
   * @return the result of applying the matrix to the input vector
   */
  def applyMFctr(mFctr: MFactor, v: Array[BigInt], wData: Int): Array[BigInt] = {
    require(!mFctr.isEmpty && v.length == mFctr(0).length)
    val mask = (BigInt(1) << wData) - 1
    val res  = Array.fill(mFctr.length)(BigInt(0))
    for (r <- 0 until mFctr.length) {
      res(r) = (mFctr(r).zip(v).map { case (fctr, data) => FactorOp(fctr).perform(data, wData) }.sum) & mask
    }
    res
  }

  /** Apply a slice to a vector of data
   * @param slc the slice
   * @param v the vector of data
   * @param wData the width of the data
   * @return the result of applying the slice to the input vector
   */
  def applySlc(slc: Slice, v: Array[BigInt], wData: Int): Array[BigInt] = slc
    .dropRight(1).reverse
    .foldLeft(applyMFctr(slc.last, v, wData)) { case (acc, mFctr) =>
      applyMFctr(mFctr, acc, wData)
    }

  /** Apply a decomposition to a vector of data
   * @param dec the decomposition
   * @param v the vector of data
   * @param wData the width of the data
   * @return the result of applying the decomposition to the input vector
   */
  def applyCmvm(dec: Decomposition, v: Array[BigInt], wData: Int): Array[BigInt] = {
    val mask = (BigInt(1) << wData) - 1
    val (slcRes, _) = dec.foldLeft((Array.empty[Array[BigInt]], 0)) { case ((res, offset), slc) =>
      val nIns = slc.last(0).length
      (res :+ applySlc(slc, v.slice(offset, offset+nIns), wData), offset+nIns)
    }
    val combRes = (0 until slcRes.head.length).map(r => slcRes.map(_(r)).sum & mask)
    combRes.toArray
  }
}

class ConstantCMVMFactorSpec extends ConstantCMVMSpec {
  behavior of "ConstantCMVM matrix factors"
  
  def pokeAndExpect(dut: ConstantCMVMFactor)(inVec: Array[BigInt], exp: Array[BigInt]): Unit = {
    dut.io.ins.zip(inVec).foreach { case (port, data) => port.poke(data.U) }
    dut.clock.step()
    dut.io.outs.zip(exp).foreach { case (port, data) => port.expect(data.U) }
  }

  // Test various configurations of trivial matrix factors
  for (n <- 4 to 8 by 2) {
    // We assume the minimal data bit width
    val wData = BitWidths.head

    for (i <- 0 until n) {
      it should s"do trivial passthrough of input $i of $n inputs" in {
        // We only test for the minimal used factor and data bit widths
        val numBits = NBits.head
        val wData   = BitWidths.head
        val mFctr   = Array(Array.fill(n)(0.0))
        mFctr(0)(i) = 1.0

        test(new ConstantCMVMFactor(mFctr, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(n)(BigInt(0)), Array(BigInt(0)))

          // Generate some random inputs and validate their outputs
          val inVec = Array.fill(n)(BigInt(0))
          for (_ <- 0 until RedNoInputVecs) {
            inVec(i) = BigInt(wData, rng)
            val exp  = Array(inVec(i))
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }

    for (i <- rng.shuffle(Seq.range(0, n)).take(2)) {
      it should s"do trivial negation of input $i of $n inputs" in {
        // We only test for the minimal used factor and data bit widths
        val numBits = NBits.head
        val wData   = BitWidths.head
        val mFctr   = Array(Array.fill(n)(0.0))
        mFctr(0)(i) = -1.0
        val mask    = (BigInt(1) << wData) - 1

        test(new ConstantCMVMFactor(mFctr, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(n)(BigInt(0)), Array(BigInt(0)))

          // Generate some random inputs and validate their outputs
          val inVec = Array.fill(n)(BigInt(0))
          for (_ <- 0 until RedNoInputVecs) {
            inVec(i) = BigInt(wData, rng)
            val exp  = Array((-inVec(i)) & mask)
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }

    for (numBits <- NBits; i <- rng.shuffle(Seq.range(0, n)).take(2)) {
      // Generate the valid factors for this number of bits
      val factors = generateFactors(numBits)

      // Generate and test trivial, single-row matrix factors with one non-zero element
      it should s"compute a random multiple of input $i of $n inputs" in {
        // Pick a random factor for the matrix factor
        val mFctr = Array(Array.fill(n)(0.0))
        mFctr(0)(i) = factors(rng.nextInt(factors.length))

        test(new ConstantCMVMFactor(mFctr, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(n)(BigInt(0)), Array(BigInt(0)))

          // Generate some random inputs and validate their outputs
          for (_ <- 0 until RedNoInputVecs) {
            val inVec = Array.fill(n) { BigInt(wData, rng) }
            val exp   = applyMFctr(mFctr, inVec, wData)
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }

    for (numBits <- NBits; i <- rng.shuffle(Seq.range(0, n-1)).take(2)) {
      val j = rng.between(i+1, n)

      // Generate the valid factors for this number of bits
      val factors = generateFactors(numBits)

      // Generate and test single-row matrix factors with two non-zero elements
      it should s"compute two random multiples of inputs $i and $j of $n inputs" in {
        // Pick two random factors for the matrix factor
        val mFctr = Array(Array.fill(n)(0.0))
        mFctr(0)(i) = factors(rng.nextInt(factors.length))
        mFctr(0)(j) = factors(rng.nextInt(factors.length))

        test(new ConstantCMVMFactor(mFctr, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(n)(BigInt(0)), Array(BigInt(0)))

          // Generate some random inputs and validate their outputs
          for (_ <- 0 until RedNoInputVecs) {
            val inVec = Array.fill(n) { BigInt(wData, rng) }
            val exp   = applyMFctr(mFctr, inVec, wData)
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }
  }

  // Test some actual, random matrix factors
  for (wData <- BitWidths) {
    it should s"compute correctly at data width $wData" in {
      for ((p, e, numBits, _, dec) <- decs) {
        // Pick a random matrix factor from the decomposition
        val mFctr = {
          val all = dec.flatten
          val ind = rng.nextInt(all.length)
          all(ind)
        }
        val nIns  = mFctr(0).length
        val nOuts = mFctr.length

        // Build its hardware and test it
        test(new ConstantCMVMFactor(mFctr, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(nIns)(BigInt(0)), Array.fill(nOuts)(BigInt(0)))

          // Generate a bunch of random input vectors and compare their outputs
          // to a software model
          for (_ <- 0 until NoInputVecs) {
            val inVec = Array.fill(nIns) { BigInt(wData, rng) }
            val exp   = applyMFctr(mFctr, inVec, wData)
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }
  }
}

class ConstantCMVMSliceSpec extends ConstantCMVMSpec {
  behavior of "ConstantCMVM slices"
  
  def pokeAndExpect(dut: ConstantCMVMSlice)(inVec: Array[BigInt], exp: Array[BigInt]): Unit = {
    dut.io.ins.zip(inVec).foreach { case (port, data) => port.poke(data.U) }
    dut.clock.step()
    dut.io.outs.zip(exp).foreach { case (port, data) => port.expect(data.U) }
  }

  // Test some actual, random slices
  for (wData <- BitWidths) {
    it should s"compute correctly at data width $wData" in {
      for ((p, e, numBits, _, dec) <- decs) {
        // Pick a random slice from the decomposition and test it
        val slc   = dec(rng.nextInt(dec.length))
        val nIns  = slc.last(0).length
        val nOuts = slc.last.length
        
        // Build its hardware and test it
        test(new ConstantCMVMSlice(slc, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(nIns)(BigInt(0)), Array.fill(nOuts)(BigInt(0)))

          // Generate a bunch of random input vectors and compare their outputs
          // to a software model
          for (_ <- 0 until NoInputVecs) {
            val inVec = Array.fill(nIns) { BigInt(wData, rng) }
            val exp = applySlc(slc, inVec, wData)
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }
  }
}

class ConstantCMVMDecompositionSpec extends ConstantCMVMSpec {
  behavior of "ConstantCMVM decomposition"
  
  def pokeAndExpect(dut: ConstantCMVM)(inVec: Array[BigInt], exp: Array[BigInt]): Unit = {
    dut.io.ins.zip(inVec).foreach { case (port, data) => port.poke(data.U) }
    dut.clock.step()
    dut.io.outs.zip(exp).foreach { case (port, data) => port.expect(data.U) }
  }

  // Test some actual, random decompositions
  for (wData <- BitWidths) {
    it should s"compute correctly at data width $wData" in {
      for ((p, e, numBits, _, dec) <- decs) {
      val nIns  = dec.map(_.last(0).length).sum
      val nOuts = dec.map(_.last.length).sum

      // Build its hardware and test it
        test(new ConstantCMVM(dec, numBits, wData, false))
          .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          // Supply some default values to the inputs and check the outputs
          pokeAndExpect(dut)(Array.fill(nIns)(BigInt(0)), Array.fill(nOuts)(BigInt(0)))

          // Generate a bunch of random input vectors and compare their outputs
          // to a software model
          for (_ <- 0 until NoInputVecs) {
            val inVec = Array.fill(nIns) { BigInt(wData, rng) }
            val exp = applyCmvm(dec, inVec, wData)
            pokeAndExpect(dut)(inVec, exp)
          }
        }
      }
    }
  }
}
