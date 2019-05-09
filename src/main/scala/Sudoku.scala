
class Sudoku(val rawBoard: String) {

  var board: Array[Array[Int]] = rawBoard.split("\n").map(line => {
    line.split(" ").map(_.toInt)
  })

  def solved(board: Array[Array[Int]]): Boolean = {

    def checkColumnSum(col: Int): Boolean = ???
    
    def checkColumnDuplicates(col: Int): Boolean = ???

    def checkRowSum(row: Int): Boolean = ???

    def checkRowDuplicates(row: Int): Boolean = ???

    def checkBoxSum(row: Int, col: Int): Boolean = ???

    def checkBoxDuplicates(rowBox: Int, colBox: Int): Boolean = ???

    true

  }

  def solve() = ???

  def possibleNumbers(row: Int, col: Int): Set[Int] = ???

  def solveEasy() = ???

  override def toString: String = board.map(_.mkString(" ")).mkString("\n")

}
