package sdm

import math.{ ceil, log }
import scala.collection.mutable.ListBuffer

/**
 * The main command-line interface.  
 * 
 * Usage: java -jar sdm.jar SDM <ncells>
 * 
 */
object SDM {

  def main(args: Array[String]): Unit = {
    require(args.length >= 1)
    val ncells = args(0).toInt
    val inv = new FeatureInventory(ncells)
    println(inv)

    // NOTE: ParadigmInventory is not computed yet as it is not needed 
    // until the lexemes and accHomph code is added
    
    val totFeatures = inv.length
    // how many features are needed to represent paradigm with ncells
    val featuresNeeded = ceil(log(ncells + 1) / log(2)).toInt
    println(featuresNeeded + " features needed.")
    
    var count = 0
    var ngood = 0
    (0 to (totFeatures - featuresNeeded)) foreach { i =>
      val fi = inv.featureSet(i)
      ((i + 1) to (totFeatures - featuresNeeded) + 1) foreach { j =>
        val fj = fi + inv(j)
        if (featuresNeeded > 2) {
          ((j + 1) to (totFeatures - featuresNeeded) + 2) foreach { k =>
            val fk = fj + inv(k)
            if (featuresNeeded > 3) {
              ((k + 1) to (totFeatures - featuresNeeded) + 3) foreach { l =>
                val fl = fk + inv(l)
                count += 1
                if (checkFeatureSet(fl)) {
                  ngood += 1
                  println((i + 1, j + 1, k + 1, l + 1))
                }
              }
            } else {
              count += 1
              if (checkFeatureSet(fk)) {
                ngood += 1
                println((i + 1, j + 1, k + 1))
                // val fstar = fk.andComplete
                // val paradigm = fk.finestParadigm
              }
            }
          }
        } else {
          count += 1
          if (checkFeatureSet(fj)) {
            ngood += 1
            println((i + 1, j + 1))
          }
        }
      }
    }

    println("Total featuresets considered: " + count)
    println("Total fine featuresets: " + ngood)
  }

  def checkFeatureSet(fs: FeatureSet): Boolean = {
    if (fs.isFine) {
      true
    } else {
      false
    }
  }

}