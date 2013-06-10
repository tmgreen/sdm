package sdm.fs

import scala.collection._
import scala.language.postfixOps
import sdm._
import java.io.File
import java.io.PrintWriter
import scala.io.Source

class SdmFs(parentDir: File)(implicit theory: Theory) {

  parentDir.mkdirs()

  private[this] val rootDir = newDir(parentDir, s"intrinsic_${theory.ncells}cells")

  def isDone(space: FeatureSetSpace): Boolean = resultsDir(space).exists

  def storeResults(space: FeatureSetSpace, result: SdmResult) {
    val dir = resultsDir(space)
    if (!dir.exists) {
      // do it all in a tmp dir and then rename dir at end, for some degree of atomicity
      val tmpDir = creating(resultsDir(space, "."))
      val profilesFile = new File(tmpDir, "profiles.txt")
      writeProfilesToFile(profilesFile, result)
      val summaryFile = new File(tmpDir, "summary.txt")
      writeSummaryToFile(summaryFile, space, result)
      tmpDir.renameTo(dir)
    }
  }

  def readResults(space: FeatureSetSpace): Option[SdmResult] = {
    val dir = resultsDir(space)
    if (!dir.exists)
      None
    else {
      val profilesFile = new File(dir, "profiles.txt")
      val profiles = readProfilesFromFile(profilesFile)
      val summaryFile = new File(dir, "summary.txt")
      val (count, fineCount, elapsedMillis) = readSummaryFromFile(summaryFile)
      Some(SdmResult(count, fineCount, profiles, elapsedMillis))
    }
  }

  private[this] def resultsDir(space: FeatureSetSpace, prefix: String = ""): File = {
    new File(rootDir, prefix + space.toString)
  }

  /** Writes a single tab-separated line like:
    * {{{
    * 46._._._   1499784 175008  76023   27121190
    * }}}
    *
    * The values are: the FeatureSetSpace, total count of FeatureSets considered,
    * count of "fine" FeatureSets, count of unique Paradigm sets realized by the
    * fine FeatureSets, and the elapsed millis of the calculation
    */
  private[this] def writeSummaryToFile(file: File, space: FeatureSetSpace, result: SdmResult) {
    printToFile(file) { pw =>
      val fields = List(
        space.toString,
        result.count,
        result.fineCount,
        result.profiles.size,
        result.elapsedMillis)
      pw.println(fields.mkString("\t"))
    }
  }

  /** Each line of file will look something like:
    * {{{
    * {1,43,45} {2,33,45} {42,44,122}
    * }}}
    */
  private[this] def writeProfilesToFile(file: File, result: SdmResult) {
    printToFile(file) { pw =>
      val profileStrings = result.profiles map { prof =>
        prof.toVector.sorted map { l => new Paradigm(l).toString } mkString (" ")
      } toVector;
      profileStrings.sorted foreach { ps => pw.println(ps) }
    }
  }

  /** returns (count, fineCount, elapsedMillis), which must be the first line of the file */
  private[this] def readSummaryFromFile(file: File): (Int, Int, Int) = {
    val line = Source.fromFile(file).getLines.next
    val intFields = line.split("\t").tail map { _.toInt }
    assert(intFields.length == 4, s"Wrong number of fields in file $file: '$line'")
    (intFields(0), intFields(1), intFields(3))
  }

  private[this] def readProfilesFromFile(file: File): Set[Set[Long]] = {
    Source.fromFile(file).getLines map { l =>
      val paradigms = l.split(" ") map {
        case SdmFs.ParadigmStringRegex(cells) => {
          val cellNums = cells.split(",") map { _.toInt }
          Paradigm(cellNums: _*).mask
        }
      }
      Set(paradigms: _*)
    } toSet
  }

  private[this] def creating(dir: File) = {
    dir.mkdir
    dir
  }

  private[this] def newDir(parent: File, name: String) = {
    val newDir = new File(parentDir, name)
    newDir.mkdir()
    newDir
  }

  private[this] def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}

object SdmFs {

  def apply(path: String)(implicit theory: Theory) = new SdmFs(new File(path))

  private[SdmFs] val ParadigmStringRegex = """^\{(.+)\}$""".r

}