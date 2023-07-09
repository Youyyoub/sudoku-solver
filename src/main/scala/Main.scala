package sudoku

import zio._
import zio.json._
import zio.nio._

object Main extends ZIOAppDefault {
  
  val sudokuTestGrid = Array(
    Array(5, 3, 0, 0, 7, 0, 0, 0, 0),
    Array(6, 0, 0, 1, 9, 5, 0, 0, 0),
    Array(0, 9, 8, 0, 0, 0, 0, 6, 0),
    Array(8, 0, 0, 0, 6, 0, 0, 0, 3),
    Array(4, 0, 0, 8, 0, 3, 0, 0, 1),
    Array(7, 0, 0, 0, 2, 0, 0, 0, 6),
    Array(0, 6, 0, 0, 0, 0, 2, 8, 0),
    Array(0, 0, 0, 4, 1, 9, 0, 0, 5),
    Array(0, 0, 0, 0, 8, 0, 0, 7, 9),
  )

  def run: ZIO[Any, Throwable, Unit] = {

    for {
      // _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem:")
      // path <- Console.readLine
      // _ <-  Console.printLine(s"You entered: $path")
    
      // _ <- Console.printLine(Sudoku.prettyString(sudokuTestGrid))
      _ <- Console.printLine(Sudoku.validate(sudokuTestGrid, 5, 2, 4))
      _ <- Console.printLine(Sudoku.validate(sudokuTestGrid, 5, 2, 5))
      _ <- Console.printLine(Sudoku.validate(sudokuTestGrid, 5, 2, 7))
    } yield ()

  }
}

object Sudoku {
  // Definition of the Board type used to store a sudoku grid
  type Board = Array[Array[Int]]

  case class SudokuGrid(
    row1: String,
    row2: String,
    row3: String,
    row4: String,
    row5: String,
    row6: String,
    row7: String,
    row8: String,
    row9: String,
  )

  implicit val SudokuGridDecoder: JsonDecoder[SudokuGrid] = DeriveJsonDecoder.gen[SudokuGrid]

  def prettyString(sudoku: Board): String = {
    sudoku.grouped(3).map { bigGroup =>
      bigGroup.map { row =>
        row.grouped(3).map { smallGroup =>
          smallGroup.mkString(" ", " ", " ")
        }.mkString("|", "|", "|")
      }.mkString("\n")
    }.mkString("+-------+-------+-------+\n", "\n+-------+-------+-------+\n", "\n+-------+-------+-------+")
  }

  def validate(sudoku: Board, x: Int, y: Int, value: Int): Boolean = {
    val row = sudoku(y)
    val rowPropertiy = !row.contains(value) // Checking if the row already contains the value we're trying to fill

    val column = sudoku.map(r => r.apply(x))
    val columnProperty = !column.contains(value)

    val boxX = x / 3
    val boxY = y / 3
    val box = for {
      yb <- (boxY * 3) until(boxY * 3 + 3)
      xb <- (boxX * 3) until(boxX * 3 + 3)
    } yield sudoku(yb)(xb)

    val boxProperty = !box.contains(value)

    rowPropertiy && columnProperty && boxProperty
  }

}