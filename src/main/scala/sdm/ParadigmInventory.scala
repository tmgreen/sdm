package sdm

/** The full inventory of paradigms possible given a number of cells.  Each paradigm is
  * represented internally as a Long mask (exposed via a Paradigm value class wrapper).
  * Due to space constraints, the maximum number of cells/lexemes in a given paradigm is 15,
  * but because of the combinatorial explosion in generating the full paradigm, we will
  * max out elsewhere before reaching this limit (in current testing I can't
  * make it to ncells = 13 without running out of memory).
  */
class ParadigmInventory(val ncells: Int) extends MatrixLike[Int] with IndexedSeq[Paradigm] {

  require(ncells <= 127)

  val matrix = {
    // temporary tree structure used for computing all possible paradigms
    val tree = new PTree()
    val m = Array.ofDim[Long](tree.size)
    tree.fillMatrix(m)
    m
  }

  /** IndexedSeq[Paradigm] impl
    */
  override def apply(col: Int): Paradigm = Paradigm(matrix(col))

  /** IndexedSeq[Paradigm] impl
    */
  override def length = matrix.length

  /** MatrixLike[Int] impl
    */
  override def ncols = matrix.length

  /** MatrixLike[Int] impl
    */
  override def nrows = ncells

  /** MatrixLike[Int] impl
    */
  override def cell(row: Int, col: Int): Int = Paradigm(matrix(col))(row)

  /** internal class used to build a temporary tree for computing paradigm cell values
    */
  private[ParadigmInventory] class PTree(val value: Int = 1, parent: Option[PTree] = None) {

    val level: Int = parent map { _.level + 1 } getOrElse (1)

    def maxInPath: Int = {
      val ancestorMax = parent map { _.maxInPath } getOrElse (0)
      if (ancestorMax > value) ancestorMax else value
    }

    val children: Option[Array[PTree]] = {
      if (level >= ncells) {
        None
      } else {
        Some(
          for {
            v <- (1 to (maxInPath + 1)).toArray
          } yield new PTree(v, Some(this)))
      }
    }

    def size: Int = children match {
      case None => 1
      case Some(kids) =>
        kids.foldLeft(0) { _ + _.size }
    }

    private[ParadigmInventory] def fillMatrix(m: Array[Long], col: Int = 0): Unit = {
      children match {
        case None => fillCol(m(col))
        case Some(kids) => {
          var offset = 0
          for (k <- kids) {
            k.fillMatrix(m, col + offset)
            offset += k.size
          }
        }
      }
    }

    private[PTree] def fillCol(ar: Long): Unit = {
      val newAr = Paradigm(ar) updated (level - 1, value)
      parent foreach (_.fillCol(newAr.mask))
    }

    override def toString = {
      val kids = children map { _.mkString } getOrElse ("")
      "[" + value + kids + "]"
    }

  }

}

