package sdm

import scala.collection._
import scala.collection.mutable.ConcurrentMap
import scala.math.Ordered

/**
 * Feature is a very shallow layer over a simple 32-bit mask.  Therefore, 
 * the max value for <code>ncells</code> is 31 (maybe 32 but could get weird 
 * due to the Int sign bit).
 */
class Feature(val ncells: Int, val mask: Int) extends Ordered[Feature] with IndexedSeq[Int] {

  override def apply(i: Int) = {
    if (i >= ncells) throw new IndexOutOfBoundsException(i.toString)
    (mask >>> i) & 1
  }

  /**
   * For faster indexed lookup (avoids IOOBE check above)
   */
  def unsafeApply(i: Int) = {
    (mask >>> i) & 1
  }

  /**
   * Warning: this is not the same "length" as used in the SDM literature: it is just
   * the number of cells.  This method is required to implement IndexedSeq.
   */
  override def length = ncells

  /**
   * count of bits set (cf. SDM "length")
   */
  val bitsSet = sum

  override def hashCode = mask

  override def equals(o: Any) = o match {
    case f: Feature =>
      f.eq(this) ||
        (f.canEqual(this) && ncells == f.ncells && mask == f.mask)
    case _ => false
  }

  override def canEqual(o: Any): Boolean = o.isInstanceOf[Feature]

  /**
   * implementation of Ordered[Feature]
   */
  override def compare(that: Feature) = {
    val deltaLen = bitsSet - that.bitsSet
    if (deltaLen == 0)
      mask - that.mask
    else
      deltaLen
  }

}

object Feature {

  def apply(bits: Int*): Feature = {
    var i = 0
    var mask = 0
    bits foreach { b =>
      b match {
        case 1 =>
          mask |= (1 << i)
        case 0 =>
        case _ =>
          throw new Exception(getClass().getSimpleName() + " may contain only 0s and 1s")
      }
      i += 1
    }
    new Feature(bits.length, mask)
  }

  def fromMask(size: Int, mask: Int): Feature = {
    new Feature(size, mask)
  }

}