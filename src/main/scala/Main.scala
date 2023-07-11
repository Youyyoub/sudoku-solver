package sudoku

import zio._
import scala.io.Source
import zio.json._
import zio.nio._
import io.circe.parser._
import io.circe._
import io.circe.generic.auto._
import cats.syntax.either._
import io.circe.DecodingFailure
import io.circe.{Error => CirceError}

import scala.util.{Failure, Success, Try}
import scala.collection.mutable

object JsonConverter {
  case class SudokuData(row1: List[Option[Int]], row2: List[Option[Int]], row3: List[Option[Int]], row4: List[Option[Int]], row5: List[Option[Int]], row6: List[Option[Int]], row7: List[Option[Int]], row8: List[Option[Int]], row9: List[Option[Int]])

  implicit val decoder: JsonDecoder[SudokuData] = DeriveJsonDecoder.gen[SudokuData]

  def convert(jsonString: String): Either[io.circe.Error, Sudoku.Board] = {
    jsonString.fromJson[SudokuData].leftMap(DecodingFailure(_, Nil)).map(data => List(data.row1, data.row2, data.row3, data.row4, data.row5, data.row6, data.row7, data.row8, data.row9))
  }
}


object NullConverter {
  def NullConvert(table: Sudoku.Board): Sudoku.Board = {
    table.map { row =>
      row.map { value =>
        if (value == Some(0)) None else value
      }
    }
  }
}


object Main extends ZIOAppDefault {

  def run: ZIO[Any, Throwable, Unit] = {
    for {

      // Get filepath
      _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem: ")
      path <- Console.readLine
      _ <- Console.printLine(s"You entered: $path")

      // Load file
      jsonSudokuData <- Sudoku.openFile(path).catchAll { error =>
        for {
          _ <- Console.printLine("Could not open primary file")
          _ <- Console.printLine("Enter another or the right path to a JSON file:")
          backupPath <- Console.readLine
          jsonSudokuData <- Sudoku.openFile(backupPath)
        } yield jsonSudokuData
      }

      _ <- Console.printLine("Loaded Sudoku: ")

      // Convert Json into Board
      result = JsonConverter.convert(jsonSudokuData)
      board <- ZIO.fromEither(result)
      convertedBoard = NullConverter.NullConvert(board)
      _ <- Console.printLine(Sudoku.prettyString(convertedBoard) + "\n")

      
      _ <- Console.printLine("Solved Sudoku: ")
      solvedBoard = Sudoku.solve(convertedBoard)
      _ <- Console.printLine(Sudoku.prettyString(solvedBoard.get) + "\n")
      
    } yield ()
  }
}

object Sudoku {
  // Definition of the Board type used to store a sudoku grid
  type Board = List[List[Option[Int]]]

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

  def openFile(path: String): ZIO[Any, Throwable, String] =
    ZIO.succeed(Source.fromFile(path).mkString)

  /** Function used to display a soduku in the console in an attractive way.
    *
    * Input:
    * @param sudoku the sudoku grid we want to display
    *
    * Example Output:
    * 5 3 4 | 6 7 8 | 9 1 2 
    * 6 7 2 | 1 9 5 | 3 4 8 
    * 1 9 8 | 3 4 2 | 5 6 7 
    * ---------------------
    * 8 5 9 | 7 6 1 | 4 2 3 
    * 4 2 6 | 8 5 3 | 7 9 1 
    * 7 1 3 | 9 2 4 | 8 5 6 
    * ---------------------
    * 9 6 1 | 5 3 7 | 2 8 4 
    * 2 8 7 | 4 1 9 | 6 3 5 
    * 3 4 5 | 2 8 6 | 1 7 9 
    * ---------------------
    */
  def prettyString(sudoku: Board): String = {
    sudoku.map { row =>
      row.map {
        case Some(value) => value.toString
        case None => " "
      }.grouped(3).map(_.mkString(" ")).mkString(" | ")
    }.grouped(3).map(_.mkString("\n")).mkString("\n---------------------\n")
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
    // Check if the value already exists in the same line
    if (sudoku(x).contains(Some(value)))
      return false

    // Check if the value already exists in the same column
    if (sudoku.exists(row => row(y) == Some(value)))
      return false

    // Check if the value already exists in the same 3x3 region
    val regionX = (x / 3) * 3
    val regionY = (y / 3) * 3
    for {
      i <- regionX until regionX + 3
      j <- regionY until regionY + 3
    } {
      if (sudoku(i)(j) == Some(value))
        return false
    }

    true
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
  def solve(sudoku: Board, x: Int = 0, y: Int = 0): Option[Board] = {
    if (x == 9) {
      // All cells have been filled in, the Sudoku is solved
      Some(sudoku)
    } else {
      val nextX = if (y == 8) x + 1 else x
      val nextY = (y + 1) % 9

      if (sudoku(x)(y).isDefined) {
        // The cell is already filled, go to the next one
        solve(sudoku, nextX, nextY)
      } else {
        val validValues = mutable.ListBuffer[Int]()
        for (value <- 1 to 9) {
          if (validate(sudoku, x, y, value)) {
            validValues += value
          }
        }

        if (validValues.isEmpty) {
          // No valid value for the cell, go back
          None
        } else {
          // Try valid values for cell
          validValues.foreach { value =>
            val updatedSudoku = sudoku.updated(x, sudoku(x).updated(y, Some(value)))
            val result = solve(updatedSudoku, nextX, nextY)
            if (result.isDefined) {
              // A solution has been found: return the solved Sudoku.
              return result
            }
          }

          // No solution found with valid values, go back
          None
        }
      }
    }
  }
}