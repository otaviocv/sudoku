package sudoku

class SudokuNorvig(val rawGrid: String) {

  val isSolved = false
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

  val puzzleValues: Map[(Char, Char), Set[Char]] = {
    squares.foldLeft(Map[(Char, Char), Set[Char]]())(
      (map, s) =>
        map + (s -> digits.toSet)
      )
  }

  def cross[A, B](a: List[A], b: List[B]): List[(A, B)] = {
    a.flatMap(x => { b.map(y => { (x, y) }) })
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

  def prettySudoku(values: Map[(Char, Char), Set[Char]]): String = {
    val width:Int = 1 + values.maxBy({ case (k, v) => v.size })._2.size
    val section = "-"*(width*3)
    val line: String = "\n" + List.fill(3)(section).mkString("+") + "\n"
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

  def parseGrid(rawGrid: String): Map[(Char, Char), Set[Char]] = {
    var values = puzzleValues
    gridValues(rawGrid).foldLeft(values)((map, p) => {
      if (digits.contains(p._2)) {
        println("Pair: " + p.toString + "!")
        println("Contains: " + p._2.toString + "!")
        println(assign(map, p._1, p._2))
        map ++ assign(map, p._1, p._2)
      } 
      else map
    })
  }

  def assign(values: Map[(Char, Char), Set[Char]],
             s: (Char, Char),
             d: Char): Map[(Char, Char), Set[Char]] = {
    val otherValues = values(s) - d
    if (isPossibleEliminateAll(values, s, otherValues)) {
      eliminateAll(values, s, otherValues)
    } else values
  }

  def isPossibleAssign(values: Map[(Char, Char), Char],
                       s: (Char, Char),
                       d: Char): Boolean = ???

  def isPossibleEliminateAll(values: Map[(Char, Char), Set[Char]],
                             s: (Char, Char),
                             ds: Set[Char]): Boolean = {
    ds.map(d => {
     isPossibleEliminate(values, s, d)
    }).reduce(_ && _)
  }
  def isPossibleEliminate(values: Map[(Char, Char), Set[Char]],
                          s: (Char, Char),
                          d: Char): Boolean = {
    val newValues = values(s) - d
    if (newValues.isEmpty) false
    else true
  }

  def eliminateAll(values: Map[(Char, Char), Set[Char]],
                   s: (Char, Char),
                   ds: Set[Char]) = {
    ds.foldLeft(values)((map, d) => {
      if (isPossibleEliminate(values, s, d)) map ++ eliminate(map, s, d)
      else map
    })
  }

  // def eliminate(values: Map[(Char, Char), Set[Char]],
  //                         s: (Char, Char),
  //                         d: Char): Map[(Char, Char), Set[Char]] = ???

  def eliminate(values: Map[(Char, Char), Set[Char]],
                          s: (Char, Char),
                          d: Char): Map[(Char, Char), Set[Char]] = {
    if (!values(s).contains(d)) values
    var newValues = (values(s) - d)
    if (newValues.isEmpty) values
    else values + (s -> newValues)
    // else if (values(s).size == 1) {
    //   values
    // }
  }

  // def eliminateOnPeers(values: Map[(Char, Char), Set[Char]], s: (Char, Char),
  //                      d: Char): Map[(Char, Char), Set[Char]] = {
  // peers(s).flatMap(values)((map, s) => { 
  //   map ++ eliminate(map, s, d)
  // })}

  def solve(): Unit = {

  }

  def checkSolved(values: Map[(Char, Char), Set[Char]]): Boolean = {
    values.map({case (s, ds) => ds.size == 1}).reduce(_ && _)
  }

  override def toString(): String = {
    prettySudoku(puzzleValues)
  }

}
