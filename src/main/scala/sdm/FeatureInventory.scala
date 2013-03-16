package sdm

import collection._
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

/** Example for ncells = 4:
  *
  * 15 total features (each feature must contain at least one 1):
  *
  * 1  0  0  0  1  1  0  1  0  0  1  1  1  0  1
  * 0  1  0  0  1  0  1  0  1  0  1  1  0  1  1
  * 0  0  1  0  0  1  1  0  0  1  1  0  1  1  1
  * 0  0  0  1  0  0  0  1  1  1  0  1  1  1  1
  *
  * The order is significant, mostly to recreate output from original R code
  * (though this implementation is 0-based rather than 1-based as in R).
  *
  */
class FeatureInventory(val ncells: Int) extends IndexedSeq[Feature] with MatrixLike[Int] {

  /** A mask is a column of bits representing a single feature.  Each feature contains one digit
    * position for each cell, so if ncells = 3, each feature mask is three binary digits.  In this
    * case, maxMask is thus 2^^3 - 1, or binary 111
    */
  val maxMask = (1 << ncells) - 1

  /** Build index for retrieval of nth (0-based) Feature in this inventory, according to the stable
    * natural order defined by Feature
    */
  val index: Array[Feature] = {
    val features = (1 to maxMask).toArray map { Feature.fromMask(ncells, _) }
    Sorting.quickSort(features)
    features
  }

  /** in addition to the position-based index above, this is a mask-based index
    */
  lazy val mask2feature: Map[Int, Feature] = {
    (index map (f => (f.mask -> f))).toMap
  }

  /** lookup a single column
    */
  override def apply(col: Int) = index(col)

  override def length = index.length

  override def nrows = ncells

  override def ncols = length

  override def cell(row: Int, col: Int): Int = index(col)(row)

  def featureForMask(mask: Int) = mask2feature(mask)

  /** What feature results from AND'ing together two features?
    */
  def and(f1: Feature, f2: Feature) = featureForMask(f1.mask & f2.mask)

  /** create a new FeatureSet as a series of numeric indices into this master
    * feature inventory matrix.  E.g. this.featureSet(0,1) would create a
    * FeatureSet with 2 features, corresponding to the first two columns
    * in the feature matrix of this inventory.
    */
  def featureSet(indices: Int*) = {
    val masks = indices map (index(_).mask)
    new FeatureSet(this, masks: _*)
  }

}