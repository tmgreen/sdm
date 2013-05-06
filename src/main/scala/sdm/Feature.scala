package sdm

import scala.collection._
import scala.collection.mutable.ConcurrentMap
import scala.math.Ordered
import Feature._

/** Feature has two components, a length and mask.  The length is used to 
  * limit the width of the mask that is used for the actual feature cells.  Because
  * mask is a 32 bit int, the length is currently limited to 31 (which shouldn't
  * be any problem as the rest of SDM will barf long before that.
  */
case class Feature (length: Int, mask: Int) {

  // 0-based left-to-right cell indexing
  def cell(i: Int) = {
    (mask >>> (length - i - 1)) & 1
  }

  /** What feature results from AND'ing together two features?
    */
  def &(f2: Feature) = new Feature(length, mask & f2.mask)

  def toBitString = {
    val unpadded = mask.toBinaryString
    val padding = "0" * (length - unpadded.length)
    padding + unpadded
  }

  def asInt = mask
  
}

object Feature {

  def apply(bitString: String): Feature = {
    val nums = bitString map (c => c.toString.toInt)
    fromBits(nums: _*)
  }

  def fromBits(bits: Int*): Feature = {
    if (bits.length > 31)
      throw new IndexOutOfBoundsException("Maximum feature length is 31 cells")
    var i = bits.length - 1
    var mask = 0
    bits foreach { b =>
      b match {
        case 1 =>
          mask |= (1 << i)
        case 0 =>
        case _ =>
          throw new Exception(getClass().getSimpleName() + " may contain only 0s and 1s")
      }
      i -= 1
    }
    Feature(bits.length, mask)
  }

}