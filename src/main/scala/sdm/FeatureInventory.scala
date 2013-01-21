package sdm

import collection._
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

class FeatureInventory(val ncells: Int) extends IndexedSeq[Feature] with MatrixLike[Int] {

  val maxMask = (1 << ncells) - 1

  val index: Array[Feature] = {
    val features = (1 to maxMask).toArray map { Feature.fromMask(ncells, _) }
    Sorting.quickSort(features)
    features
  }

  lazy val mask2feature: Map[Int, Feature] = {
    (index map (f => (f.mask -> f))).toMap
  }

  /**
   * lookup a single column
   */
  override def apply(col: Int) = index(col)

  override def length = index.length
  
  override def nrows = ncells

  override def ncols = length

  override def cell(row: Int, col: Int): Int = index(col)(row)

  def featureForMask(mask: Int) = mask2feature(mask)

  def and(f1: Feature, f2: Feature) = featureForMask(f1.mask & f2.mask)

  def featureSet(indices: Int*) = {
    val masks = indices map (index(_).mask)
    new FeatureSet(this, masks: _*)
  }

}