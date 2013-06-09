package sdm

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

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

    val fsSpaces = (1 to (totalFeatures - featuresNeeded + 1)) map { mask => FeatureSetSpace(mask) }

    val finalResult = resultForSpacesUsingFutures(fsSpaces)

    val allElapsedMillis = System.currentTimeMillis() - allStartTime

    println("Total featuresets considered: " + finalResult.count)
    println("Total fine featuresets: " + finalResult.goodCount)
    println("Total paradigm sets: " + finalResult.profiles.size)
    println("Total elapsed compute time: " + timeString(finalResult.elapsedMillis))
    println("Total elapsed actual time: " + timeString(allElapsedMillis))
  }

  /** combine two SdmResults into one */
  def reduceResults(r1: SdmResult, r2: SdmResult): SdmResult = { r1 + r2 }

  /** Method using collection.par to implement the parallelization
    */
  def resultForSpacesUsingPar(fsSpaces: Iterable[FeatureSetSpace]): SdmResult = {
    fsSpaces.par map { fsSpace =>
      val result = computeBulkResult(fsSpace)
      println(s"$fsSpace\t$result")
      result
    } reduce (reduceResults)
  }

  /** Method using Futures to implement the parallelization.  Though more complex,
    * hopefully this will even out the processor load a bit better.
    */
  def resultForSpacesUsingFutures(fsSpaces: Iterable[FeatureSetSpace]): SdmResult = {
    val futures = fsSpaces map { fsSpace =>
      future {
        val result = computeBulkResult(fsSpace)
        println(s"$fsSpace\t$result")
        result
      }
    }
    val futureResult = Future.reduce(futures)(reduceResults)
    // block till all futures have returned
    Await.result(futureResult, Duration.Inf)
  }

}