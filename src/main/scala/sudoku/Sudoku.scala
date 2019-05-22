package sudoku

class Sudoku(val rawBoard: String) {

  var board: Array[Array[Int]] = rawBoard.split("\n").map(line => {
    line.split(" ").map(_.toInt)
  })

  def solved(board: Array[Array[Int]]): Boolean = {
    checkColsSolved(board) && checkRowsSolved(board) && checkBoxesSolved(board)
  }

  def checkColsSolved(board: Array[Array[Int]]): Boolean = {
    (0 until 9).foldLeft(true)(
      (status, index) => {
        status &&
        checkColumnSum(board, index) &&
        checkColumnDuplicates(board, index)
      }
    )
  }

  def checkRowsSolved(board: Array[Array[Int]]): Boolean = {
    (0 until 9).foldLeft(true)(
      (status, index) => {
        status &&
        checkRowSum(board, index) &&
        checkRowDuplicates(board, index)
      }
    )
  }

  def checkBoxesSolved(board: Array[Array[Int]]): Boolean = {
    List(0, 3, 6).foldLeft(true)(
      (status, i) => {
        status && List(0, 3, 6).foldLeft(true)(
          (smallStatus, j) => {
            smallStatus && 
            checkBoxSum(board, i, j) &&
            checkBoxDuplicates(board, i, j)
          }
        )
      }
    )
  }

  def checkColumnSum(board: Array[Array[Int]], col: Int): Boolean = {
    board.foldLeft(0)(_ + _(col)) == 45
  }
  
  def checkColumnDuplicates(board: Array[Array[Int]], col: Int): Boolean = {
    (board.map(_(col)).toSet - 0).size == 9
  }

  def checkRowSum(board: Array[Array[Int]], row: Int): Boolean = {
    board(row).sum == 45
  }

  def checkRowDuplicates(board: Array[Array[Int]], row: Int): Boolean = {
    (board(row).toSet - 0).size == 9
  }

  def checkBoxSum(board: Array[Array[Int]], row: Int, col: Int): Boolean = {
    val row_i: Int = row - row % 3
    val col_i: Int = col - col % 3
    board.slice(row_i, row_i + 3).map(
      _.slice(col_i, col_i + 3).sum
    ).sum == 45
  }

  def checkBoxDuplicates(board: Array[Array[Int]], row: Int,
                         col: Int): Boolean = {
    val row_i: Int = row - row % 3
    val col_i: Int = col - col % 3
    board.slice(row_i, row_i + 3)
         .foldLeft(Set[Int]())(
           (currentSet, otherArray) => {
        currentSet ++ otherArray.slice(col_i, col_i + 3).toSet
      }
    ).size == 9
  }

  def solve() = ???

  def solveEasy() = ???

  def possibleNumbers(row: Int, col: Int): Set[Int] = ???

  override def toString: String = board.map(_.mkString(" ")).mkString("\n")

}
