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

  test("Test newValues size for each square is 9") {
    new SudokuNorvig("") {
      newValues.keys.map(k => {
        assert(newValues(k).size == 9)
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

  val simpleSudokuGame = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
  val matrixSudokuGame = """
400000805
030000000
000700000
020000060
000080400
000010000
000603070
500200000
104000000
"""
  val prettySudokuGame = """
4 . . |. . . |8 . 5 
. 3 . |. . . |. . . 
. . . |7 . . |. . . 
------+------+------
. 2 . |. . . |. 6 . 
. . . |. 8 . |4 . . 
. . . |. 1 . |. . . 
------+------+------
. . . |6 . 3 |. 7 . 
5 . . |2 . . |. . . 
1 . 4 |. . . |. . . 
"""

  test("Test clean chars 1") {
    new SudokuNorvig("") {
      assert(cleanChars(simpleSudokuGame).length == 81)
    }
  }

  test("Test clean chars 2") {
    new SudokuNorvig("") {
      assert(cleanChars(matrixSudokuGame).length == 81)
    }
  }

  test("Test clean chars 3") {
    new SudokuNorvig("") {
      val cleanedChars = cleanChars(prettySudokuGame)
      assert(cleanedChars.length == 81)
    }
  }

  test("Test grid values") {
    new SudokuNorvig("") {
      val gridVals = gridValues(prettySudokuGame)
      assert(gridVals(('A', '1')) == '4')
      assert(gridVals(('B', '2')) == '3')
      assert(gridVals(('C', '3')) == '.')
    }
  }

  test("Test current possible values") {
    new SudokuNorvig("") {
      val gridVals = gridValues(prettySudokuGame)
      val posVals = currentPossibleValues(gridVals)
      assert(posVals(('A', '1')) == Set('4'))
      assert(posVals(('A', '2')) == digits.toSet)
    }
  }

  test("Test toString method") {
    new SudokuNorvig("") {
      val gridVals = gridValues(prettySudokuGame)
      val posVals = currentPossibleValues(gridVals)
      println(posVals)
      val prettySudoku = toString(posVals)
      println(prettySudoku)
    }
  }

}
