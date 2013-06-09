package sdm

import scala.collection._
import scala.collection.mutable.ConcurrentMap
import scala.math.Ordered
import Feature._

/** Feature has two components, a length and mask.  The length is used to
  * limit the width of the mask that is used for the actual feature cells.  Because
  * mask is a 32 bit signed int, the length is currently limited to 31 (which shouldn't
  * be any problem as the rest of SDM will barf long before that.
  */
case class Feature private[sdm] (mask: Int) extends AnyVal {

  /** `nth` is a 0-based index counting *left-to-right* based on a stipulated
   *  total feature length of `ncells`.  Used in matrix displays.
    */
  final def nthHighestBit(nth: Int, ncells: Int) = nthBit(ncells - nth - 1)

  final def nthBit(nth: Int) = (mask >>> nth) & 1
  
  /** What feature results from AND'ing together two features?  Returns None if
    * features are completely disjoint (this avoids the illegal 0-mask feature).
    */
  def &(f2: Feature): Option[Feature] = {
    val newMask = mask & f2.mask
    if (newMask > 0)
      Some(new Feature(newMask))
    else
      None
  }

  def toBitString(length: Int) = {
    val unpadded = mask.toBinaryString
    val padding = "0" * (length - unpadded.length)
    padding + unpadded
  }

  def asInt = mask

}

object Feature {

  /** `bitString` is something like "01011".  Leading 0s are important
    * as the length of the string is used to determine the number of
    * cells in the Feature
    */
  def apply(bitString: String): Feature = {
    val nums = bitString map (c => c.toString.toInt)
    fromBits(nums: _*)
  }

  /** `bits` is read from left-to-right, with *most significant* bit (cell 0)
    * coming first.
    */
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
    Feature(mask)
  }

}