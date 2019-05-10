
class Sudoku(val rawBoard: String) {

  var board: Array[Array[Int]] = rawBoard.split("\n").map(line => {
    line.split(" ").map(_.toInt)
  })

  def solved(board: Array[Array[Int]]): Boolean = {

    def checkColumnSum(col: Int): Boolean = board.foldLeft(0)(_ + _(col))
    
    def checkColumnDuplicates(col: Int): Boolean = ???

    def checkRowSum(row: Int): Boolean = board(row).sum

    def checkRowDuplicates(row: Int): Boolean = ???

    def checkBoxSum(row: Int, col: Int): Boolean = {
      val row_i: Int = row % 3
      val col_i: Int = col % 3
    }

    def checkBoxDuplicates(rowBox: Int, colBox: Int): Boolean = ???

    true

  }

  def solve() = ???

  def possibleNumbers(row: Int, col: Int): Set[Int] = ???

  def solveEasy() = ???

  override def toString: String = board.map(_.mkString(" ")).mkString("\n")

}
