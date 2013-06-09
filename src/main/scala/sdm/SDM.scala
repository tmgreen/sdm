package sdm

import math.{ ceil, log }
import scala.collection.mutable.ListBuffer
import scala.collection.Set
import scala.collection.mutable
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.SortedSet

/** The main command-line interface.
  *
  * Usage: java -jar sdm.jar SDM <ncells>
  *
  */
object SDM {

  def main(args: Array[String]): Unit = {
    require(args.length >= 1)
    val ncells = args(0).toInt
    implicit val theory = Theory(ncells)
    import theory._
    
    println(ncells + " cells in model.")
    println(totalFeatures + " total features available.")
    println(s"$featuresNeeded features needed for the $ncells cell case.")

    case class Result(count: Int, goodCount: Int, profiles: Set[Set[Long]]) {
      override def toString = s"$count\t$goodCount\t${profiles.size}"
    }

    def handleSpace(rootMask: Int): Result = {
      val mpartProfiles: mutable.Set[Set[Long]] = mutable.Set.empty
      var count = 0
      var ngood = 0

      def handleFs(fs: FeatureSet) {
        count += 1
        if (fs.isFine) {
          ngood += 1
          tallyParadigms(fs, mpartProfiles)
        }
      }

      val space = FeatureSetSpace(rootMask)
      space.foreach(handleFs)

      val res = Result(count, ngood, mpartProfiles)
      println(s"$rootMask\t$res")
      res
    }

    def reducer(r1: Result, r2: Result): Result = {
      Result(count = r1.count + r2.count,
        goodCount = r1.goodCount + r2.goodCount,
        profiles = r1.profiles ++ r2.profiles)
    }
    
    val finalResult = (1 to (totalFeatures - featuresNeeded + 1)).par map handleSpace reduce reducer
    
    println("Total featuresets considered: " + finalResult.count)
    println("Total fine featuresets: " + finalResult.goodCount)
    println("Total paradigm sets: " + finalResult.profiles.size)
  }

  def tallyParadigms(fs: FeatureSet, profiles: mutable.Set[Set[Long]]) {
    val pars = fs.andComplete.allParadigms
    profiles add pars
  }
}