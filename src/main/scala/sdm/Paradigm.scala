package sdm

import math.{ floor, ceil, log }
import Paradigm._

/** Value class for treating a Long bitmask as a Paradigm.  If code is careful, hopefully
  * there will never need to be any actual Paradigm wrapper objects created.
  *
  * The lower 60 bits of a long are used, 4 bits per cell, yielding a maximum
  * of 15 cells (each with a maximum value of 15).  In an n-cell paradigm, all
  * n cells must be non-zero.  This allows the implementation to calculate
  * the number of cells (i.e. `length`) simply as floor(log16(mask)) + 1.
  */
case class Paradigm(mask: Long) extends AnyVal {

  def apply(index: Int): Int = {
    if (index > 15) throw new IndexOutOfBoundsException(index.toString)
    unsafeApply(index)
  }

  private[sdm] final def unsafeApply(index: Int): Int = {
    ((mask >>> shiftBitsForIndex(index)) & 0xFL).toInt
  }

  /** number of cells in this Paradigm */
  def length: Int = floor(log(mask) / log(16)).toInt + 1

  /** return new Paradigm with new value `x` at index `i` */
  def updated(i: Int, x: Int): Paradigm = {
    val shiftAmount = shiftBitsForIndex(i)
    Paradigm((~(0xFL << shiftAmount) & mask) | (x.toLong << shiftAmount))
  }

  def toSeq: IndexedSeq[Int] =
    (0 until length) map unsafeApply

  override def toString: String = this.toSeq.mkString("{", ",", "}")

}

object Paradigm {

  def apply(cells: Int*): Paradigm = {
    require(cells.length < 16, "Max supported number of cells in a Paradigm is 15")
    var mask: Long = 0
    cells.reverseIterator foreach { b =>
      require(b < 16, "Max supported number of cells in a Paradigm is 15")
      mask = (mask << 4) + b
    }
    new Paradigm(mask)
  }

  def apply(cells: Array[Int]): Paradigm = apply(cells: _*)

  /** since 4 bits are used for each cell, multiply index
    * by 4 to determine how many bits to shift to access cell with
    * index `i`.
    */
  private[sdm] final def shiftBitsForIndex(i: Int): Int = i << 2

}