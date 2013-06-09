package sdm

import scala.collection.Set

case class SdmResult(count: Int, goodCount: Int, profiles: Set[Set[Long]], elapsedMillis: Int) {
  override def toString = s"$count\t$goodCount\t${profiles.size}\t${timeString(elapsedMillis)}"

  def +(r2: SdmResult): SdmResult =
    SdmResult(
      count = count + r2.count,
      goodCount = goodCount + r2.goodCount,
      profiles = profiles ++ r2.profiles,
      elapsedMillis = elapsedMillis + r2.elapsedMillis)
}

