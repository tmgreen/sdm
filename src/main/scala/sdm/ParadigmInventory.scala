package sdm

/**
 * The full inventory of paradigms possible given a number of cells.  Each paradigm is 
 * represented simply as an Array[Byte] (i.e. it currently has no user-friendly wrapper class).
 * Due to the use of Byte, the maximum number of cells/lexemes in a given paradigm is 127.  
 * But Feature can only go up to 31 anyway and due to combinatorial explosion we will 
 * max out elsewhere long before reaching this limit (in current testing I can't
 * make it to ncells = 13 without running out of memory).
 */
class ParadigmInventory(val ncells: Int) extends MatrixLike[Byte] with IndexedSeq[Array[Byte]] {

  require(ncells <= 127)

  val matrix = {
    // temporary tree structure used for computing all possible paradigms
    val tree = new PTree()
    val m = Array.ofDim[Byte](tree.size, ncells)
    tree.fillMatrix(m)
    m
  }

  /**
   * IndexedSeq[Byte] impl
   */
  override def apply(col: Int): Array[Byte] = matrix(col)
  
  /**
   * IndexedSeq[Byte] impl
   */
  override def length = matrix.length
  
  /**
   * MatrixLike[Byte] impl
   */
  override def ncols = matrix.length

  /**
   * MatrixLike[Byte] impl
   */
  override def nrows = ncells

  /**
   * MatrixLike[Byte] impl
   */
  override def cell(row: Int, col: Int): Byte = matrix(col)(row)

  /**
   * internal class used to build a temporary tree for computing paradigm cell values
   */
  private[ParadigmInventory] class PTree(val value: Byte = 1, parent: Option[PTree] = None) {

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
          } yield new PTree(v.toByte, Some(this)))
      }
    }

    def size: Int = children match {
      case None => 1
      case Some(kids) =>
        kids.foldLeft(0) { _ + _.size }
    }

    private[ParadigmInventory] def fillMatrix(m: Array[Array[Byte]], col: Int = 0): Unit = {
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

    private[PTree] def fillCol(ar: Array[Byte]): Unit = {
      ar(level - 1) = value
      parent foreach (_.fillCol(ar))
    }

    override def toString = {
      val kids = children map { _.mkString } getOrElse ("")
      "[" + value + kids + "]"
    }

  }

}

