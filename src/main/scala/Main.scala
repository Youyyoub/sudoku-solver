package sudoku

import zio._
import scala.io.Source
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
      _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem:")
      path <- Console.readLine
      _ <-  Console.printLine(s"You entered: $path")

      jsonSudokuData <- openFile(path).catchAll { error =>
        for {
          _ <- printLine("Could not open primary file")
          _ <- printLine("Enter another or the right path to a JSON file:")
          backupPath <- readLine
          file <- openFile(backupPath)
        } yield file
      }

      _ <- Console.printLine("Loaded Sudoku:")
      _ <- Console.printLine(Sudoku.prettyString(sudokuTestGrid) + "\n")

    } yield {
      // Resolve the grid
      Sudoku.solve(sudokuTestGrid)
    }
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

  /** Function used to display a soduku in the console in an attractive way.
    * 
    * Input:
    * @param sudoku the sudoku grid we want to display
    * 
    * Example Output:
    * +-------+-------+-------+
    * | 5 3 4 | 6 7 8 | 9 1 2 |
    * | 6 7 2 | 1 9 5 | 3 4 8 |
    * | 1 9 8 | 3 4 2 | 5 6 7 |
    * +-------+-------+-------+
    * | 8 5 9 | 7 6 1 | 4 2 3 |
    * | 4 2 6 | 8 5 3 | 7 9 1 |
    * | 7 1 3 | 9 2 4 | 8 5 6 |
    * +-------+-------+-------+
    * | 9 6 1 | 5 3 7 | 2 8 4 |
    * | 2 8 7 | 4 1 9 | 6 3 5 |
    * | 3 4 5 | 2 8 6 | 1 7 9 |
    * +-------+-------+-------+
    */
  def prettyString(sudoku: Board): String = {
    sudoku.grouped(3).map { bigGroup =>
      bigGroup.map { row =>
        row.grouped(3).map { smallGroup =>
          smallGroup.mkString(" ", " ", " ")
        }.mkString("|", "|", "|")
      }.mkString("\n")
    }.mkString("+-------+-------+-------+\n", "\n+-------+-------+-------+\n", "\n+-------+-------+-------+")
  }

  /** Function to determine whether a value placed at the (x, y) coordinates
    * of a sudoku grid validates or invalidates the grid.
    * 
    * Input:
    * @param sudoku the sudoku grid we want to test
    * @param x the x coordinate at which we want to test our value
    * @param y the y coordinate at which we want to test our value
    * @param value the value we want to test
    * 
    * Output:
    *   Boolean indicating whether the value tested at coordinates (x, y) is valid
    *   - true: the value can be placed at these coordinates
    *   - false: the value invalidate the sudoku grid
    */
  def validate(sudoku: Board, x: Int, y: Int, value: Int): Boolean = {
    val row = sudoku(y)
    val rowPropertiy = !row.contains(value)

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

  /**
    * Function that solves a sudoku grid by testing all possible values using the `validate` function.
    * 
    * The function end when:
    * - we reached the end of the board, i.e. y >= 9, which means we’ve filled the board and we have a solution
    * - we’ve tested all possible values
    *
    * @param sudoku the sudoku grind we want to solve
    * @param x
    * @param y
    */
  def solve(sudoku: Board, x: Int = 0, y: Int = 0): Unit = {
    if (y >= 9) println(prettyString(sudoku)) // We reached the final solution
    else if (x >= 9) solve(sudoku, 0, y + 1)
    else if (sudoku(y)(x) > 0) solve(sudoku, x + 1, y)
    else (1 to 9).filter(value => validate(sudoku, x, y, value)).foreach { value =>
      sudoku(y)(x) = value  // fill the sudoku grid with the value
      solve(sudoku, x + 1, y)  // try the next cell
      sudoku(y)(x) = 0  // remove the value
    }
  }
}