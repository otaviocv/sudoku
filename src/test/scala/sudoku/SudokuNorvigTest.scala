package sudoku

import org.scalatest.FunSuite

class SudokuNorvigTest extends FunSuite {

  test("Test squares length is 81") {
    new SudokuNorvig("") {
        assert(squares.length == 81)
    }
  }

  test("Test unitslist length is 27") {
    new SudokuNorvig("") {
        assert(unitslist.length == 27)
    }
  }

  test("Test units length for each square is 3") {
    new SudokuNorvig("") {
      squares.map(x => {
        assert(units(x).length == 3)
      })
    }
  }

  test("Test peers size for each square is 20") {
    new SudokuNorvig("") {
      squares.map(x => {
        assert(peers(x).size == 20)
      })
    }
  }

  test("Test C2 units") {
    new SudokuNorvig("") {
      assert(
        units(('C', '2')) == List(
          List(('A', '2'), ('B', '2'), ('C', '2'), 
               ('D', '2'), ('E', '2'), ('F', '2'),
               ('G', '2'), ('H', '2'), ('I', '2')),
          List(('C', '1'), ('C', '2'), ('C', '3'), 
               ('C', '4'), ('C', '5'), ('C', '6'),
               ('C', '7'), ('C', '8'), ('C', '9')),
          List(('A', '1'), ('A', '2'), ('A', '3'), 
               ('B', '1'), ('B', '2'), ('B', '3'),
               ('C', '1'), ('C', '2'), ('C', '3'))
          )
        )
    }
  }

  test("Test C2 peers") {
    new SudokuNorvig("") {
      assert(
        peers(('C', '2')) == Set(
          ('A', '2'), ('B', '2'), ('D', '2'), ('E', '2'),
          ('F', '2'), ('G', '2'), ('H', '2'), ('I', '2'),
          ('C', '1'), ('C', '3'), ('C', '4'), ('C', '5'),
          ('C', '6'), ('C', '7'), ('C', '8'), ('C', '9'),
          ('A', '1'), ('A', '3'), ('B', '1'), ('B', '3')
        )
      )
    }
  }

}
