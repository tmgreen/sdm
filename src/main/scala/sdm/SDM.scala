package sdm

import scala.collection.Set
import scala.collection.mutable
import scala.concurrent._

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
    val allStartTime = System.currentTimeMillis()

    case class Result(count: Int, goodCount: Int, profiles: Set[Set[Long]], elapsedMillis: Int) {
      override def toString = s"$count\t$goodCount\t${profiles.size}\t${timeString(elapsedMillis)}"

      def +(r2: Result): Result =
        Result(
          count = count + r2.count,
          goodCount = goodCount + r2.goodCount,
          profiles = profiles ++ r2.profiles,
          elapsedMillis = elapsedMillis + r2.elapsedMillis)
    }

    def handleSpace(rootMask: Int): Result = {
      val startTime = System.currentTimeMillis()
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

      val elapsed = System.currentTimeMillis() - startTime
      val res = Result(count, ngood, mpartProfiles, elapsed.toInt)
      println(s"$rootMask\t$res")
      res
    }

    def reducer(r1: Result, r2: Result): Result = { r1 + r2 }

    val finalResult = (1 to (totalFeatures - featuresNeeded + 1)).par map handleSpace reduce reducer
    val allElapsedMillis = System.currentTimeMillis() - allStartTime

    println("Total featuresets considered: " + finalResult.count)
    println("Total fine featuresets: " + finalResult.goodCount)
    println("Total paradigm sets: " + finalResult.profiles.size)
    println("Total elapsed compute time: " + timeString(finalResult.elapsedMillis))
    println("Total elapsed actual time: " + timeString(allElapsedMillis))
  }

  def timeString(millis: Long): String = {
    if (millis < 2000) {
      millis + "ms"
    } else if (millis < 120000) {
      (millis / 1000) + "s"
    } else {
      (millis / 60000) + "m"
    }
  }

  def tallyParadigms(fs: FeatureSet, profiles: mutable.Set[Set[Long]]) {
    val pars = fs.andComplete.allParadigms
    profiles add pars
  }
}