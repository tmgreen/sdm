package sdm

import scala.annotation.tailrec
import scala.collection.SortedSet

/** A `FeatureSetSpace` is a device used for efficient generation of all possible
  * feature sets for a given theory.  It is defined by a `Theory` and a (possibly
  * empty) set of "root" Feature masks `rootMasks` which form the "seed" for all
  * `FeatureSet`s in the space.  `Theory.featuresNeeded` dictates how many `Feature`s
  * are needed in order to realize a complete paradigm given the number of cells
  * in the theory.  `rootMasks` will normally be smaller than featuresNeeded (it
  * wouldn't strictly be an error for rootMasks to contain all `featuresNeeded`
  * elements but it would make the `FeatureSetSpace` useless).
  *
  * When adding features to form complete feature sets, the maximum mask in
  * `rootMasks` determines the lower bound for the range of masks for the next
  * feature in the set, and each of these in turn must be less than all masks
  * for the next feature, and so on.  So in a theory where `ncells` == 3, and thus
  * where `featuresNeeded` is 2 and `totalFeatures` is 7, a FeatureSetSpace
  * with the root feature 4 would yield the following 3 feature sets and no others:
  * {4, 5}, {4, 6}, and {4, 7}.
  */
case class FeatureSetSpace(rootMasks: SortedSet[Int])(implicit theory: Theory) extends Iterable[FeatureSet] {

  import theory._

  rootMasks.foreach { mask =>
    require(mask <= maxFeatureMask - featuresToAdd, s"FeatureSetSpace root mask $mask too large.")
  }

  require(rootMasks.size <= setSize, "FeatureSetSpace is already larger than the intended set size.")

  def featuresToAdd = setSize - rootMasks.size

  def setSize = featuresNeeded

  override def iterator: Iterator[FeatureSet] = {
    val nextFeat = if (rootMasks.isEmpty) 1 else rootMasks.max + 1
    val maxFeat = maxFeatureMask
    featuresToAdd match {
      case 0 => {
        // weird case: space is already fully specified by the required number
        // of "root" features. Shouldn't happen but not strictly wrong, so just
        // return an iterator on the single feature set containing all the root 
        // features
        val fs = featureSet(rootMasks.toSeq: _*)
        Iterator(fs)
      }
      case 1 => {
        val rootFs = featureSet(rootMasks.toSeq: _*)
        (nextFeat to maxFeat).iterator map { mask =>
          rootFs + Feature(mask)
        }
      }
      case _ => {
        // there's more than one feature to add: add another level of
        // root features and recurse
        (nextFeat to maxFeat - featuresToAdd + 1).iterator flatMap { mask =>
          val newRoot = rootMasks + mask
          val newSpace = FeatureSetSpace(newRoot)
          newSpace.iterator
        }
      }
    }
  }

  def featureSet(masks: Int*) = {
    val feats = masks map { cm => Feature(cm) }
    FeatureSet(feats: _*)
  }

}

object FeatureSetSpace {

  def apply(mask: Int)(implicit theory: Theory) = new FeatureSetSpace(SortedSet(mask))
  
  /** contains all features in the available inventory */
  def universe(implicit theory: Theory) = new FeatureSetSpace(SortedSet[Int]())

}