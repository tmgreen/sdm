package sdm

import fs._
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
    val memoryStrategy = if (args.length > 1) args(1) else "ram"
    require(Set("ram", "disk") contains memoryStrategy, s"Unknown memory strategy '$memoryStrategy'")
    // only used for "disk" strategy
    val fileStoreDir = if (memoryStrategy == "disk") {
      if (args.length > 2) Some(args(2)) else Some("sdm_out")
    } else
      None

    implicit val theory = Theory(ncells)
    import theory._

    println(ncells + " cells in model.")
    println(totalFeatures + " total features available.")
    println(s"$featuresNeeded features needed for the $ncells cell case.")
    val allStartTime = System.currentTimeMillis()

    val fsSpaces = (1 to (totalFeatures - featuresNeeded + 1)) map { mask => FeatureSetSpace(mask) }

    val finalResult = memoryStrategy.toLowerCase match {
      case "ram" => resultForSpacesUsingFutures(fsSpaces)
      case "disk" => {
        require(fileStoreDir.isDefined)
        val path = fileStoreDir.get
        println(s"Using disk storage in $path")
        resultForSpacesUsingFileSystem(fsSpaces, SdmFs(path))
      }
    }
    
    val allElapsedMillis = System.currentTimeMillis() - allStartTime

    println("Total featuresets considered: " + finalResult.count)
    println("Total fine featuresets: " + finalResult.fineCount)
    println("Total paradigm sets: " + finalResult.profiles.size)
    println("Total elapsed compute time: " + timeString(finalResult.elapsedMillis))
    println("Total elapsed actual time: " + timeString(allElapsedMillis))
  }

  def computeAndLogBulkResult(fsSpace: FeatureSetSpace): SdmResult = {
    val result = computeBulkResult(fsSpace)
    println(s"$fsSpace\t$result")
    result
  }

  /** combine two SdmResults into one */
  def reduceResults(r1: SdmResult, r2: SdmResult): SdmResult = { r1 + r2 }

  def resultForSpacesUsingFileSystem(fsSpaces: Iterable[FeatureSetSpace], store: SdmFs): SdmResult = {
    val futures = Future.traverse(fsSpaces) { fsSpace =>
      future {
        if (!store.isDone(fsSpace)) {
          val result = computeAndLogBulkResult(fsSpace)
          store.storeResults(fsSpace, result)
        }
        fsSpace
      }
    }

    // block till all futures have returned
    val finishedSpaces = Await.result(futures, Duration.Inf)

    // now read results from files, sequentially so not all in mem at once
    finishedSpaces flatMap { fsSpace =>
      assert(store.isDone(fsSpace), s"Results for $fsSpace expected but not found")
      store.readResults(fsSpace)
    } reduce reduceResults
  }

  /** Method using collection.par to implement the parallelization
    */
  def resultForSpacesUsingPar(fsSpaces: Iterable[FeatureSetSpace]): SdmResult = {
    fsSpaces.par map computeAndLogBulkResult reduce reduceResults
  }

  /** Method using Futures to implement the parallelization.  Though more complex,
    * hopefully this will even out the processor load a bit better.
    */
  def resultForSpacesUsingFutures(fsSpaces: Iterable[FeatureSetSpace]): SdmResult = {
    val futures = fsSpaces map { fsSpace =>
      future { computeAndLogBulkResult(fsSpace) }
    }
    val futureResult = Future.reduce(futures)(reduceResults)
    // block till all futures have returned
    Await.result(futureResult, Duration.Inf)
  }

}