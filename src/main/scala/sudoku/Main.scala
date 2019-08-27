package sudoku

object Main extends App {
  val s = new SudokuNorvig("")
  println("Hello, World!")
  println(s.digits.toString)
  println(s.squares.toString)
}
