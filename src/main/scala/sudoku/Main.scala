package sudoku

import scala.io.Source

object Main extends App {

  println("Easy Puzzles")
  val easyPuzzlesPath = "examples/easy/easy50.txt"
  val easyPuzzles = load_puzzles_from_file(easyPuzzlesPath, "========")
  val solvedPuzzles = easyPuzzles.map(p => {
    val s = new SudokuNorvig(p)
    s.solve()
  })
  for {
    s <- solvedPuzzles
  } yield println(s)

  def load_puzzles_from_file(path: String, sep: String): List[String] = {
    val bufferedSource = Source.fromFile(path)
    val contentString = bufferedSource.getLines.mkString
    bufferedSource.close
    contentString.split(sep).toList
  }

}

