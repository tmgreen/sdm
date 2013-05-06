package sdm

import math.{ ceil, log }
import scala.collection.mutable.ListBuffer
import scala.collection.Set
import scala.collection.mutable
import scala.concurrent._
import ExecutionContext.Implicits.global

/** The main command-line interface.
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
    val neededFeatures = inv.featuresNeeded
    println(neededFeatures + " features needed.")

    case class Result(count: Int, goodCount: Int, profiles: Set[Set[Long]]) {
      override def toString = s"$count\t$goodCount\t${profiles.size}"
    }

    def handleSetsRootedAt(root: Int): Result = {
      val mpartProfiles: mutable.Set[Set[Long]] = mutable.Set.empty
      var count = 0
      var ngood = 0

      def handleFs(fs: FeatureSet) {
        count += 1
        if (checkFeatureSet(fs)) {
          ngood += 1
          tallyParadigms(fs, mpartProfiles)
        }
      }

      inv.foreachSetRootedAt(root + 1, neededFeatures)(handleFs)

      val res = Result(count, ngood, mpartProfiles)
      println(s"$root\t$res")
      res
    }

    val rawResults = (0 to (totFeatures - neededFeatures)).par map handleSetsRootedAt
    
    def reducer(r1: Result, r2: Result): Result = {
      Result(count = r1.count + r2.count,
        goodCount = r1.goodCount + r2.goodCount,
        profiles = r1.profiles ++ r2.profiles)
    }
    
    val finalResult = rawResults reduce reducer

    /*
    println("OLD")

    (0 to (totFeatures - featuresNeeded)) foreach { i =>
      val fi = inv.featureSet(i + 1)
      ((i + 1) to (totFeatures - featuresNeeded) + 1) foreach { j =>
        val fj = fi + inv.feature(j + 1)
        if (featuresNeeded > 2) {
          ((j + 1) to (totFeatures - featuresNeeded) + 2) foreach { k =>
            // val fk = fj + inv.feature(k + 1)
            val fk = inv.featureSet(i + 1, j + 1, k + 1)
            if (featuresNeeded > 3) {
              ((k + 1) to (totFeatures - featuresNeeded) + 3) foreach { l =>
                val fl = fk + inv.feature(l + 1)
                handleFs(fl)
              }
            } else {
              handleFs(fk)
            }
          }
        } else {
          handleFs(fj)
        }
      }
    }
*/


    println("Total featuresets considered: " + finalResult.count)
    println("Total fine featuresets: " + finalResult.goodCount)
    println("Total paradigm sets: " + finalResult.profiles.size)
  }

  def checkFeatureSet(fs: FeatureSet): Boolean = {
    if (fs.isComplete && fs.isFine) {
      true
    } else {
      false
    }
  }

  def tallyParadigms(fs: FeatureSet, profiles: mutable.Set[Set[Long]]) {
    val pars = fs.andComplete.allParadigms
    profiles add pars
  }
}