package cmvm

import scala.math.abs

import chisel3._
import chisel3.util._

/** IO bundle common to all CMVM blocks
 * @param nIns the number of inputs
 * @param nOuts the number of outputs
 * @param wIns the width of the inputs
 * @param wOuts the width of the outputs
 */
class ConstantCMVMIO(nIns: Int, nOuts: Int, wIns: Int, wOuts: Int) extends Bundle {
  val ins  = Input(Vec(nIns, UInt(wIns.W)))
  val outs = Output(Vec(nOuts, UInt(wOuts.W)))
}

/** CMVM module for a single matrix factor
 * @param mFactor the matrix factor to generate hardware from
 * @param wFactor the width of the factor elements
 * @param wData the width of the input and output data
 * @param pipe whether to pipeline the outputs
 * @param name the suggested name of this module
 */
class ConstantCMVMFactor(mFactor: Array[Array[Double]], wFactor: Int,
                         wData: Int, pipe: Boolean, name: String = "") extends Module {
  /** For now, we accumulate in the same data width as the inputs. This 
    * should probably be changed or adapted somehow. Not sure what's the 
    * best way if we want to avoid exploding numbers through the design.
    */
  val io = IO(new ConstantCMVMIO(mFactor(0).length, mFactor.length, wData, wData))
  if (!name.isEmpty()) suggestName(name)

  /** For each row of the matrix, we select the inputs on whose indices 
    * there are non-zero factors, "multiply" them by these factors, and 
    * reduce the result in a tree
    */
  for ((row, i) <- mFactor.zipWithIndex) {
    // Extract non-zero factors in the row
    val nonZeros = row.zipWithIndex.filter { case (fctr, _) => fctr != 0.0 }

    // Create vectors to keep track of intermediate results
    val prods = WireDefault(VecInit(Seq.fill(row.length) { 0.U(wData.W) }))

    // For each non-zero weight, compute the scaled input
    for ((fctr, j) <- nonZeros) {
      // Check if the factor is negative and compute its shift amount
      val neg = fctr < 0.0
      val (shftAmnt, shftDir) = {
        val fctrLog2 = log2(abs(fctr))
        (abs(fctrLog2).toInt, fctrLog2 < 0.0)
      }

      // Use this information to compute the "product"
      val prod = if (shftDir) { // shift right
        Fill(shftAmnt, false.B) ## io.ins(j)(wData-1, shftAmnt)
      } else { // shift left
        if (shftAmnt == 0)
          io.ins(j)
        else
          io.ins(j)(wData-shftAmnt, 0) ## 0.U(shftAmnt.W)
      }
      if (neg) prods(j) := ~prod + 1.U else prods(j) := prod
    }

    // Accumulate and output the result (pipelined if requested)
    val sum = prods.reduceTree(_ + _)
    if (pipe) io.outs(i) := RegNext(sum) else io.outs(i) := sum
  }
}

/** CMVM module for a slice of several matrix factors
 * @param slice the slice to generate hardware from
 * @param wFactor the width of the factor elements
 * @param wData the width of the input and output data
 * @param pipe whether to pipeline the outputs
 * @param name the suggested name of this module
 */
class ConstantCMVMSlice(slice: Array[Array[Array[Double]]],
                        wFactor: Int, wData: Int, pipe: Boolean,
                        name: String = "") extends Module {
  // Compute the size of the IO and create it accordingly
  private val trivial = slice.last
  private val nIns  = trivial(0).length // number of columns in the trivial matrix
  private val nOuts = trivial.length    // number of rows in the trivial matrix
  val io = IO(new ConstantCMVMIO(nIns, nOuts, wData, wData))
  if (!name.isEmpty()) suggestName(name)

  // Generate multipliers for all the matrix factors (in reverse order) ...
  private val subMods = slice.reverse.zipWithIndex.map { case (mfctr, i) =>
    val subModName = if (!name.isEmpty()) s"${name}_mfctr$i" else ""
    Module(new ConstantCMVMFactor(mfctr, wFactor, wData, pipe, name)) }

  // ... and interconnect them accordingly, starting with the IO of the 
  // outer-most matrix factors
  subMods.head.io.ins := io.ins
  io.outs := subMods.last.io.outs
  // If there is only one matrix factor, all its IO is already assigned
  if (slice.length != 1) {
    subMods.sliding(2).foreach { window =>
      window.last.io.ins := window.head.io.outs
    }
  }
}

/** Full CMVM module for several slices of matrix factors
 * @param slices the slices to generate hardware from
 * @param wFactor the width of the factor elements
 * @param wData the width of the input and output data
 * @param pipe whether to pipeline the outputs
 * @param name the suggested name of this module
 */
class ConstantCMVM(slices: Array[Array[Array[Array[Double]]]], 
                   wFactor: Int, wData: Int, pipe: Boolean = false,
                   name: String = "") extends Module {
  // Perform some sanity checks before generating anything
  require(wFactor > 0, "matrix factor representation must have positive width")
  require(wData > 0, "data representation must have positive width")
  require(!slices.isEmpty && !slices(0).isEmpty &&
    slices.drop(1).forall(_.length == slices(0).length),
    "slices must have the same number of matrix factors")
  require(slices.forall(slc => slc.forall(mfctr => 
    !mfctr.isEmpty && mfctr.drop(1).forall(_.length == mfctr(0).length))), 
    "matrix factors must be rectangular")
  private def checkCompatibility(slice: Array[Array[Array[Double]]]): Boolean = {
    slice.take(2) match {
      case Array(mfctr1, mfctr2) => 
        mfctr1(0).length == mfctr2.length && checkCompatibility(slice.drop(1))
      case _ => true
    }
  }
  require(slices.forall(checkCompatibility(_)), "matrix factors must have compatible sizes")
  if (!name.isEmpty()) suggestName(name)

  // Compute the size of the IO and create it accordingly
  private val trivials = slices.map(_.last)
  private val nIns  = trivials.map(_(0).length).sum // sum of number of columns in the trivial matrices
  private val nOuts = trivials.head.length          // number of rows in a trivial matrix
  val io = IO(new ConstantCMVMIO(nIns, nOuts, wData, wData))

  // Generate multipliers for all the slices ...
  private val subMods = slices.zipWithIndex.map { case (slc, i) =>
    val subModName = if (!name.isEmpty()) s"${name}_slice$i" else ""
    Module(new ConstantCMVMSlice(slc, wFactor, wData, pipe, name)) }

  // ... and interconnect them accordingly. The inputs are split
  // and the outputs are summed across the slices
  subMods.flatMap(_.io.ins).zip(io.ins).foreach { case (subModIn, in) => subModIn := in }
  (0 until nOuts).foreach { i =>
    io.outs(i) := VecInit(subMods.map(_.io.outs(i)).toSeq).reduceTree(_ + _)
  }
}
