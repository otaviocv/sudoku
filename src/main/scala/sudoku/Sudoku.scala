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
    (presentValuesColumn(board, col)).size == 9
  }

  def presentValuesColumn(board: Array[Array[Int]], col: Int): Set[Int] = {
    board.map(_(col)).toSet - 0
  }

  def checkRowSum(board: Array[Array[Int]], row: Int): Boolean = {
    board(row).sum == 45
  }

  def checkRowDuplicates(board: Array[Array[Int]], row: Int): Boolean = {
    presentValuesRow(board, row).size == 9
  }

  def presentValuesRow(board: Array[Array[Int]], row: Int): Set[Int] = {
    board(row).toSet - 0
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
    presentValuesBox(board, row, col).size == 9
  }

  def presentValuesBox(board: Array[Array[Int]], row: Int,
                       col: Int): Set[Int] = {
    // println("   finding present box values")
    // println(s"   looking for box of pos $row $col")
    val row_i: Int = row - row % 3
    val col_i: Int = col - col % 3
    // println(s"   starting in pos $row_i $col_i")
    board.slice(row_i, row_i + 3)
         .foldLeft(Set[Int]())(
           (currentSet, otherArray) => {
        currentSet ++ otherArray.slice(col_i, col_i + 3).toSet
      } - 0)
  }

  def solve(board: Array[Array[Int]]) = ???

  def solveEasy(board: Array[Array[Int]]): Array[Array[Int]] = {
    // println("solveEasy")

    def solveEasyRemaining(board: Array[Array[Int]], i: Int,
                           j: Int): Array[Array[Int]] = {
                             // println(s"sovling pos $i $j")
      if (j == 9) board
      else if (i == 9) {
        solveEasyRemaining(board, 0, j+1)
      } else {
        // println("old board:")
        // println(board.map(_.mkString(" ")).mkString("\n"))
        val newBoard = solveEasyPosition(board, i, j)
        // println("new board:")
        // println(newBoard.map(_.mkString(" ")).mkString("\n"))
        solveEasyRemaining(newBoard, i+1, j)
      }
    }

    val newBoard1: Array[Array[Int]] = solveEasyRemaining(board, 0, 0)
    val newBoard2: Array[Array[Int]] = solveEasyRemaining(newBoard1, 0, 0)
    if (newBoard1 != newBoard2) solveEasy(newBoard2)
    else newBoard2
  } 

  def solveEasyPosition(board: Array[Array[Int]], i: Int, j: Int): Array[Array[Int]] = { 
    if (board(i)(j) != 0) board
    else {
      val possibleNumbersSet: Set[Int] = possibleNumbers(board, i, j)
      if (possibleNumbersSet.size == 1) {
        // println("solveEasyPosition step")
        // println(s"filling in line $i and col $j the value")
        // println(possibleNumbersSet.head)
        board(i)(j) = possibleNumbersSet.head
      }
    }
    board
  }

  def possibleNumbers(board: Array[Array[Int]], row: Int,
                      col: Int): Set[Int] = {
      val possibleValues = (1 to 9).toSet
      val presentCol = presentValuesColumn(board, col)
      val presentRow = presentValuesRow(board, row)
      val presentBox = presentValuesBox(board, col, row)
      val presentValues = presentCol ++ presentRow ++ presentBox
      // println("v-- possible")
      // println(possibleValues)
      // println("v-- present col")
      // println(presentCol)
      // println("v-- present row")
      // println(presentRow)
      // println("v-- present box")
      // println(presentBox)
      // println("v-- present unioun")
      // println(presentValues)
      // println("V-- difference")
      // println(possibleValues -- presentValues)
      // println("V-- size")
      // println((possibleValues -- presentValues).size)
      // println("---")
      possibleValues -- presentValues
  }

  override def toString: String = board.map(_.mkString(" ")).mkString("\n")

}
