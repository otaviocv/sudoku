package sudoku

import org.scalatest.FunSuite

class SudokuTest extends FunSuite {

  val unsolvedBoard = 
    """0 4 0 0 0 0 1 7 9 
      |0 0 2 0 0 8 0 5 4 
      |0 0 6 0 0 5 0 0 8 
      |0 8 0 0 7 0 9 1 0 
      |0 5 0 0 9 0 0 3 0 
      |0 1 9 0 6 0 0 4 0 
      |3 0 0 4 0 0 7 0 0 
      |5 7 0 1 0 0 2 0 0 
      |9 2 8 0 0 0 0 6 0 """.stripMargin

  test("test all columns sums fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 9) {
        assert(!checkColumnSum(board, i))
      }
    }
  }

  test("test all columns duplicates fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 9) {
        assert(!checkColumnDuplicates(board, i))
      }
    }
  }

  test("test all rows sums fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 9) {
        assert(!checkRowSum(board, i))
      }
    }
  }

  test("test all rows duplicates fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 9) {
        assert(!checkRowDuplicates(board, i))
      }
    }
  }

  test("test all box sums fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 9; j <- 0 until 9) {
        assert(!checkBoxSum(board, i, j))
      }
    }
  }

  test("test all box duplicates fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 9; j <- 0 until 9) {
        assert(!checkBoxDuplicates(board, i, j))
      }
    }
  }

  test("test unsolved board is not solved") {
    new Sudoku(unsolvedBoard) {
        assert(!solved(board))
    }
  }

  test("test unsolved easy solver") {
    new Sudoku(unsolvedBoard) {
      val solved = solveEasy(board)
      println(solved.map(_.mkString(" ")).mkString("\n"))
    }
  }

  val solvedBoard = 
    """8 4 5 6 3 2 1 7 9
      |7 3 2 9 1 8 6 5 4
      |1 9 6 7 4 5 3 2 8
      |6 8 3 5 7 4 9 1 2
      |4 5 7 2 9 1 8 3 6
      |2 1 9 8 6 3 5 4 7
      |3 6 1 4 2 9 7 8 5
      |5 7 4 1 8 6 2 9 3
      |9 2 8 3 5 7 4 6 1""".stripMargin

  test("test all columns sums pass for solved board") {
    new Sudoku(solvedBoard) {
      for (i <- 0 until 9) {
        assert(checkColumnSum(board, i))
      }
    }
  }

  test("test all columns duplicates pass for solved board") {
    new Sudoku(solvedBoard) {
      for (i <- 0 until 9) {
        assert(checkColumnDuplicates(board, i))
      }
    }
  }

  test("test all rows sums pass for solved board") {
    new Sudoku(solvedBoard) {
      for (i <- 0 until 9) {
        assert(checkRowSum(board, i))
      }
    }
  }

  test("test all rows duplicates pass for solved board") {
    new Sudoku(solvedBoard) {
      for (i <- 0 until 9) {
        assert(checkRowDuplicates(board, i))
      }
    }
  }

  test("test all box sums pass for solved board") {
    new Sudoku(solvedBoard) {
      for (i <- 0 until 9; j <- 0 until 9) {
        assert(checkBoxSum(board, i, j))
      }
    }
  }

  test("test all box duplicates pass for solved board") {
    new Sudoku(solvedBoard) {
      for (i <- 0 until 9; j <- 0 until 9) {
        assert(checkBoxDuplicates(board, i, j))
      }
    }
  }

  test("test solved board is solved") {
    new Sudoku(solvedBoard) {
        assert(solved(board))
    }
  }

}
