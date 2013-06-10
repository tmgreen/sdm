package sdm

import scala.collection.Set

case class SdmResult(count: Int, fineCount: Int, profiles: Set[Set[Long]], elapsedMillis: Int) {
  override def toString = s"$count\t$fineCount\t${profiles.size}\t${timeString(elapsedMillis)}"

  def +(r2: SdmResult): SdmResult =
    SdmResult(
      count = count + r2.count,
      fineCount = fineCount + r2.fineCount,
      profiles = profiles ++ r2.profiles,
      elapsedMillis = elapsedMillis + r2.elapsedMillis)
}

