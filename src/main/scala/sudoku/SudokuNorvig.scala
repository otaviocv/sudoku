package sudoku

class SudokuNorvig(val rawGrid: String) {

  def cross[A, B](a: List[A], b: List[B]): List[(A, B)] = {
    a.flatMap(x => { b.map(y => { (x, y) }) })
  }

  val digits: List[Char] = "123456789".toList
  val rows: List[Char] = "ABCDEFGHI".toList
  val cols: List[Char] = digits

  val rowsSquares: List[List[Char]] = List("ABC".toList) ++ List("DEF".toList) ++ List("GHI".toList)

  val colsSquares: List[List[Char]] = List("123".toList) ++ List("456".toList) ++ List("789".toList)

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

  def gridValues(rawGrid: String): Map[(Char, Char), Char] = {
    cleanChars(rawGrid).foldLeft(Map[(Char, Char), Char]())((map, c) => { 
      squares.foldLeft(map)((newMap, s) => {
        newMap + (s -> c)
      })
    })
  }

  def cleanChars(rawGrid: String): List[Char] = {
    rawGrid.foldLeft(List[Char]())((list, c) => {
      if (digits.contains(c) || c == '0' || c == '.') {
        list :+ c
      } else list
    })
  }
}
