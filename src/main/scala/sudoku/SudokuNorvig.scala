package sudoku

class SudokuNorvig(val rawGrid: String) {

  def cross[A, B](a: List[A], b: List[B]): List[(A, B)] = {
    a.flatMap(x => { b.map(y => { (x, y) }) })
  }

  val digits: List[Char] = "123456789".toList
  val rows: List[Char] = "ABCDEFGHI".toList
  val cols: List[Char] = digits

  val rowsSquares: List[List[Char]] = List("ABC".toList) ++
    List("DEF".toList) ++ List("GHI".toList)

  val colsSquares: List[List[Char]] = List("123".toList) ++
    List("456".toList) ++ List("789".toList)

  val squares: List[(Char, Char)] = cross(rows, cols)
  val unitslist: List[List[(Char, Char)]] = 
    cols.map(c => cross(rows, List(c))) ++ 
    rows.map(r => cross(List(r), cols)) ++
    rowsSquares.flatMap(x => colsSquares.map(y => cross(x, y)))

  val units: Map[(Char, Char), List[List[(Char, Char)]]] = {
    squares.foldLeft(Map[(Char, Char), List[List[(Char, Char)]]]())(
      (map, s) => {
        map + (s -> unitslist.filter(_.contains(s)))
    })
  }

  val peers: Map[(Char, Char), Set[(Char, Char)]] = {
    squares.foldLeft(Map[(Char, Char), Set[(Char, Char)]]())(
      (map, s) => {
        map + (s -> (units(s).flatMap(x => x.map(y => y)).toSet - s))
      })
  }

  val newValues: Map[(Char, Char), Set[Char]] = {
    squares.foldLeft(Map[(Char, Char), Set[Char]]())(
      (map, s) =>
        map + (s -> digits.toSet)
      )
  }

  def gridValues(rawGrid: String): Map[(Char, Char), Char] = {
    def gridValues_r(ss: List[(Char, Char)], cs: List[Char],
                     values: Map[(Char, Char), Char]): 
                     Map[(Char, Char), Char] = {
      if (ss.isEmpty) values
      else {
        gridValues_r(ss.tail,
                     cs.tail,
                     values + (ss.head -> cs.head))
      }
    }

    val cleanedChars: List[Char] = cleanChars(rawGrid) 
    gridValues_r(squares, cleanedChars, Map[(Char, Char), Char]())
  }

  def cleanChars(rawGrid: String): List[Char] = {
    rawGrid.foldLeft(List[Char]())((list, c) => {
      if (digits.contains(c) || c == '0' || c == '.') {
        list :+ c
      } else list
    })
  }

  def toString(values: Map[(Char, Char), Set[Char]]): String = {
    val width:Int = 1 + values.maxBy({ case (k, v) => v.size })._2.size
    // println("width: " + width.toString)
    val section = "-"*(width*3)
    val line: String = "\n" + List(section, section, section).mkString("+") +
                       "\n"
    rows.map(r => {
      cols.map(c => {
        center(values((r, c)).mkString, width) + 
        (if (Set('3', '6').contains(c)) "|" else "")
      }).mkString + (if (Set('C', 'F').contains(r)) (line) else "\n")
    }).mkString
  }

  def center(s: String, width: Int): String = {
    if (s.length > width) s
    else {
      val remainingChars = width - s.length
      if (remainingChars%2 == 0) {
        val pad = " "*(remainingChars/2)
        pad + s + pad
      } else {
        val lpad = " "*(remainingChars/2 + 1)
        val rpad = " "*(remainingChars/2)
        lpad + s + rpad
      }
    }
  }

  def currentPossibleValues(gridVals: Map[(Char, Char), Char]):
  Map[(Char, Char), Set[Char]] = {
    gridVals.foldLeft(Map[(Char, Char), Set[Char]]())(
      (map, pair) => pair match {
        case (s, d) => map + (s -> (
          if (digits.contains(d)) Set(d)
          else digits.toSet
          ))
      })
  }

  // def parseGrid(rawGrid: String): Map[(Char, Char), Set[Char]] = {
  //   var values = newValues
  //   gridValues(rawGrid).foldLeft(newValues)((vals, (s, d)) => {
  //     if (digits.contains(d) &&
  //         isPossibleAssign(vals, s, d)) assign(vals, s, d)
  //     else vals
  //   })
  // }

  def isPossibleAssign(values: Map[(Char, Char), Char],
                       s: (Char, Char),
                       d: Char): Boolean = ???

  // def assign(values: Map[(Char, Char), Char],
  //            s: (Char, Char),
  //            d: Char): Map[(Char, Char), Set[Char]] = {
  //   orherValues = values(s) - d
  // }

  def isPossibleEliminate(values: Map[(Char, Char), Char],
                          s: (Char, Char),
                          d: Char): Boolean = ???

  def eliminate(values: Map[(Char, Char), Char],
                          s: (Char, Char),
                          d: Char) = ???

}
