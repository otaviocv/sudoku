package sudoku

class Sudoku(val rawBoard: String) {

  var board: Array[Array[Int]] = rawBoard.split("\n").map(line => {
    line.split(" ").map(_.toInt)
  })

  def solved(board: Array[Array[Int]]): Boolean = {
    (0 until 9).foldLeft(true)((status, index) => {
      status && checkColumnSum(board, index) &&
                checkColumnDuplicates(board, index) &&
                checkRowSum(board, index) &&
                checkRowDuplicates(board, index)
    }) &&
    (0 until 3).foldLeft(true)((status, i) => {
      (0 until 3).foldLeft(true)((smallStatus, j) => {
        checkBoxSum(board, i, j) && checkBoxDuplicates(board, i, j)
      })
    })
  }

  def checkColumnSum(board: Array[Array[Int]], col: Int): Boolean = {
    board.foldLeft(0)(_ + _(col)) == 45
  }
  
  def checkColumnDuplicates(board: Array[Array[Int]], col: Int): Boolean = {
    (board.map(_(col)).toSet - 0) == 9
  }

  def checkRowSum(board: Array[Array[Int]], row: Int): Boolean = {
    board(row).sum == 45
  }

  def checkRowDuplicates(board: Array[Array[Int]], row: Int): Boolean = {
    (board(row).toSet - 0).size == 9
  }

  def checkBoxSum(board: Array[Array[Int]], row: Int, col: Int): Boolean = {
    val row_i: Int = row % 3
    val col_i: Int = col % 3
    board.slice(row_i, row_i + 3).map(_.slice(col_i, col_i + 3).sum).sum == 45
  }

  def checkBoxDuplicates(board: Array[Array[Int]], row: Int,
                         col: Int): Boolean = {
    val row_i: Int = row % 3
    val col_i: Int = col % 3
    board.slice(row_i, row_i + 3).foldLeft(Set.empty((currentSet, otherArray) => {
      currentSet + otherArray.slice(col_i, col_i + 3).toSet
    }).size == 9
  }

  def solve() = ???

  def possibleNumbers(row: Int, col: Int): Set[Int] = ???

  def solveEasy() = ???

  override def toString: String = board.map(_.mkString(" ")).mkString("\n")

}
