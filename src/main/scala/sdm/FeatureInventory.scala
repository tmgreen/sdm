package sdm

import collection._
import scala.math.{ ceil, log }
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

/** Example for ncells = 4:
  *
  * 15 total features (each feature must contain at least one 1):
  *
  * 1  0  1  0  1  0  1  0  1  0  1  0  1  0  1
  * 0  1  1  0  0  1  1  0  0  1  1  0  0  1  1
  * 0  0  0  1  1  1  1  0  0  0  0  1  1  1  1
  * 0  0  0  0  0  0  0  1  1  1  1  1  1  1  1
  *
  * The order is standard binary, with least significant bit at *top*.
  *
  * NOTE: this order no longer recreates the  output from Uli's original R code.
  * A feature can now be defined simply as a number, and this number *is* the
  * binary specification of each cell (that is, we have unified the mask with
  * the index).
  *
  */
case class FeatureInventory(ncells: Int) extends MatrixLike[Int] {

  /** A mask is a column of bits representing a single feature.  Each feature contains one digit
    * position for each cell, so if ncells = 3, each feature mask is three binary digits.  In this
    * case, maxMask is thus 2^^3 - 1, or binary 111
    */

  final val length = FeatureInventory.maxMask(ncells)

  override final def nrows = ncells

  override final def ncols = length

  /** for MatrixLike impl */
  override def cell(row: Int, col: Int): Int = Feature(ncells, col + 1).cell(row)

  /** min number of features needed to full paradigm with `ncells` distinct cells.  
   *  In other words, how many binary digits do you need to represent the number `ncells'?
   */
  def featuresNeeded = ceil(log(ncells + 1) / log(2)).toInt

  def feature(mask: Int) = Feature(ncells, mask)

  def featureSet(masks: Int*) = {
    val feats = masks map { cm => Feature(ncells, cm) }
    FeatureSet(ncells, feats)
  }

  def featureSets(setSize: Int): Iterator[FeatureSet] = {
    setSize match {
      case 2 => {
        (for {
          i <- 1 to length - 1
          fi = featureSet(i)
          j <- i + 1 to length
          fj = fi + feature(j)
        } yield fj)(breakOut)
      }
      case 3 => {
        (for {
          i <- 1 to length - 2
          fi = featureSet(i)
          j <- i + 1 to length - 1
          fj = fi + feature(j)
          k <- j + 1 to length
          fk = fj + feature(k)
        } yield fk)(breakOut)
      }
      case 4 => {
        (for {
          i <- 1 to length - 3
          fi = featureSet(i)
          j <- i + 1 to length - 2
          fj = fi + feature(j)
          k <- j + 1 to length - 1
          fk = fj + feature(k)
          l <- k + 1 to length
          fl = fk + feature(l)
        } yield fl)(breakOut)
      }
      case _ =>
        Iterator.empty
    }
  }

  // (1,2,3), (1,2,4), (1,2,5), (1,3,4), (1,3,5), (1,4,5)
  def foreachSetRootedAt(root: Int, setSize: Int)(f: (FeatureSet => Unit)) {
    require(root <= length - setSize + 1)
    val rootFs = featureSet(root)
    setSize match {
      case 2 => {
        for {
          i <- root + 1 to length
          fi = rootFs + feature(i)
        } f(fi)
      }
      case 3 => {
        for {
          i <- root + 1 to length - 1
          fi = rootFs + feature(i)
          j <- i + 1 to length
          fj = fi + feature(j)
        } f(fj)
      }
      case 4 => {
        for {
          i <- root + 1 to length - 2
          fi = rootFs + feature(i)
          j <- i + 1 to length - 1
          fj = fi + feature(j)
          k <- j + 1 to length
          fk = fj + feature(k)
        } f(fk)
      }
    }
  }

}

object FeatureInventory {

  def maxMask(ncells: Int): Int = (1 << ncells) - 1

}