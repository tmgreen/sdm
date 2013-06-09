package sdm

import scala.math.{ ceil, log }

case class Theory(ncells: Int) {

  /** A mask is a column of bits representing a single feature.  Each feature contains one digit
    * position for each cell, so if ncells = 3, each feature mask is three binary digits.  In this
    * case, maxMask is thus 2^^3 - 1, or binary 111
    */
  val maxFeatureMask: Int = (1 << ncells) - 1

  /** the number of total features defined by this theory (also happens to equal the
    * mask of the maximum feature number.
    */
  final def totalFeatures = maxFeatureMask

  /** The minimum number of features needed to represent a full paradigm with `ncells` distinct cells.
    * In other words, how many binary digits do you need to represent the number `ncells'?
    */
  val featuresNeeded = ceil(log(ncells + 1) / log(2)).toInt

  def featureSet(masks: Int*) = {
    val feats = masks map { cm => Feature(cm) }
    FeatureSet(feats: _*)(this)
  }

}