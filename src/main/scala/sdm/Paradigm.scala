package sdm

import math.{ floor, ceil, log }

/** Value class for treating a Long bitmask as a Paradigm.  If code is careful, hopefully
  * there will never need to be any actual Paradigm wrapper objects created.
  *
  * The lower 60 bits of a long are used, 4 bits per cell, yielding a maximum
  * of 15 cells (each with a maximum value of 15).  In an n-cell paradigm, all
  * n cells must be non-zero.  This allows the implementation to calculate
  * the number of cells simply as floor(log16(mask)) + 1.
  */
case class Paradigm(mask: Long) extends AnyVal {

  def apply(index: Int): Int = {
    if (index > 15) throw new IndexOutOfBoundsException(index.toString)
    ((mask >> (index << 2)) & 0xF).toInt
  }

  def unsafeApply(index: Int): Int = {
    ((mask >> (index << 2)) & 0xF).toInt
  }

  def length: Int = floor(log(mask) / log(16)).toInt + 1

  def shiftBitsForIndex(i: Int): Int = i << 2

  def updated(i: Int, x: Int): Paradigm = {
    val shiftAmount = shiftBitsForIndex(i)
    Paradigm((~(0xF << shiftAmount) & mask) | (x << shiftAmount))
  }

  def toSeq: IndexedSeq[Int] =
    (0 until length) map unsafeApply

  override def toString: String = {
    "Paradigm(" + toSeq.mkString(", ") + ")"
  }
}

object Paradigm {

  def apply(cells: Int*): Paradigm = {
    require(cells.length < 16, "Max supported number of cells in a Paradigm is 15")
    var mask: Long = 0
    cells.reverse foreach { b =>
      require(b < 16, "Max supported number of cells in a Paradigm is 15")
      mask = (mask << 4) + b
    }
    new Paradigm(mask)
  }

  def apply(cells: Array[Int]): Paradigm = apply(cells: _*)

}