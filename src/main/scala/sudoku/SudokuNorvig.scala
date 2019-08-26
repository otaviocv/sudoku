package sudoku

class SudokuNorvig(val rawGrid: String) {

  def cross[A, B](a: List[A], b: List[B]): List[(A, B)] = {
    a.flatMap(x => { b.map(y => { (x, y) }) })
  }

  val digits: List[Char] = "123456789".toList
  val rows: List[Char] = "ABCDEFGHI".toList
  val cols: List[Char] = digits

  val rowsSquares: List[List[Char]] = List("ABC") ++
    List("DEF".toList) ++
    List("GHI")

  val colsSquares: List[List[Char]] = List("123") ++
    List("456".toList) ++
    List("789")
  val squares: List[(Char, Char)] = cross(rows, cols)
  val unitlist: List[List[(Char, Char)]] = 
    cols.map(c => cross(rows, List(c))) ++ 
    rows.map(r => cross(cols, List(r))) ++

  def parseGrid(rawGrid: String): Map[String, String] = ???

}
