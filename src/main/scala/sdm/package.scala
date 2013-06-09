import scala.collection._

package object sdm {

  def computeBulkResult(sets: Traversable[FeatureSet]): SdmResult = {
    val startTime = System.currentTimeMillis()
    val mpartProfiles: mutable.Set[Set[Long]] = mutable.Set.empty
    var count = 0
    var ngood = 0

    def tallyParadigms(fs: FeatureSet, profiles: mutable.Set[Set[Long]]) {
      val pars = fs.andComplete.allParadigms
      profiles add pars
    }
    
    def handleFs(fs: FeatureSet) {
      count += 1
      if (fs.isFine) {
        ngood += 1
        tallyParadigms(fs, mpartProfiles)
      }
    }

    sets.foreach(handleFs)

    val elapsed = System.currentTimeMillis() - startTime
    val res = SdmResult(count, ngood, mpartProfiles, elapsed.toInt)
    res
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

}