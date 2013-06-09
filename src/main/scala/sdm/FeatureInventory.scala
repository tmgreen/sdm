package sdm

import collection._
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

/** Example for ncells = 4:
  *
  * 15 total features:
  *
  * 0  0  0  0  0  0  0  1  1  1  1  1  1  1  1
  * 0  0  0  1  1  1  1  0  0  0  0  1  1  1  1
  * 0  1  1  0  0  1  1  0  0  1  1  0  0  1  1
  * 1  0  1  0  1  0  1  0  1  0  1  0  1  0  1
  *
  * (each feature must contain at least one 1, so "0 0 0 0" is excluded)
  * 
  * The order is standard binary, with least significant bit at *bottom*.
  *
  * NOTE: this order no longer recreates the  output from Uli's original R code.
  * A feature can now be defined simply as a number, and this number *is* the
  * binary specification of each cell (that is, we have unified the mask with
  * the index).
  */
case class FeatureInventory(implicit theory: Theory) extends MatrixLike[Int] {

  import theory._
  
  /** for MatrixLike impl */
  override final def nrows = ncells

  /** for MatrixLike impl */
  override final def ncols = maxFeatureMask

  /** for MatrixLike impl */
  override def cell(row: Int, col: Int): Int = Feature(col + 1).nthHighestBit(row, ncells)

}
