package sdm

/**
 * 0-based row x col matrix
 */
trait MatrixLike[T] {

  def nrows: Int

  def ncols: Int

  /** 0-based with 0,0 at top left */
  def cell(row: Int, col: Int): T

  override def toString = {
    val sb = new StringBuilder
    val showCols = math.min(40, ncols)
    0 until nrows foreach { row =>
      if (row > 0)
        sb.append("\n")
      0 until showCols map { cell(row, _) } addString (sb, "  ")
      if (showCols < ncols) {
        sb.append("  ...")
        if (row == 0)
          sb.append(" (").append(ncols).append(" total)")
      }
    }
    sb.toString
  }

}