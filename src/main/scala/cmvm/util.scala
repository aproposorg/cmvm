package cmvm

import scala.collection.mutable

object Util {
  /** Time a piece of code and, optionally, store the measured time in a mutable buffer
   * @param block the block of code to execute
   */
  def time[R](block: => R)(strg: mutable.ArrayBuffer[Long] = null): R = {
    val start = System.nanoTime()
    val res = block
    val end = System.nanoTime()
    if (strg != null) strg.append(end - start) else println(s"Elapsed time: ${end - start} ns")
    res
  }
}
