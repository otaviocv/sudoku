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
      for (i <- 0 until 3; j <- 0 until 3) {
        assert(!checkBoxSum(board, i, j))
      }
    }
  }

  test("test all box duplicates fail for unsolved board") {
    new Sudoku(unsolvedBoard) {
      for (i <- 0 until 3; j <- 0 until 3) {
        assert(!checkBoxDuplicates(board, i, j))
      }
    }
  }

}
