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

object JsonConverter {
  case class SudokuData(row1: List[Int], row2: List[Int], row3: List[Int], row4: List[Int], row5: List[Int], row6: List[Int], row7: List[Int], row8: List[Int], row9: List[Int])

  implicit val decoder: JsonDecoder[SudokuData] = DeriveJsonDecoder.gen[SudokuData]

  def convert(jsonString: String): Either[io.circe.Error, Sudoku.Board] = {
  jsonString.fromJson[SudokuData].leftMap(DecodingFailure(_, Nil)).map(data => List(data.row1, data.row2, data.row3, data.row4, data.row5, data.row6, data.row7, data.row8, data.row9))
}
 




}

object Main extends ZIOAppDefault {

  def run: ZIO[Any, Throwable, Unit] = {
    for {
      _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem: ")
      path <- Console.readLine
      _ <- Console.printLine(s"You entered: $path")

      jsonSudokuData <- Sudoku.openFile(path).catchAll { error =>
        for {
          _ <- Console.printLine("Could not open primary file")
          _ <- Console.printLine("Enter another or the right path to a JSON file:")
          backupPath <- Console.readLine
          jsonSudokuData <- Sudoku.openFile(backupPath)
        } yield jsonSudokuData
      }

      _ <- Console.printLine("Loaded Sudoku: ")

      result = JsonConverter.convert(jsonSudokuData)
      result <- ZIO.fromEither(result)
      _ <- Console.printLine(Sudoku.prettyString(result) + "\n")

      
    } yield ()
  }
}

object Sudoku {
  // Definition of the Board type used to store a sudoku grid
  type Board = List[List[Int]]

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
  sudoku.map { row =>
    row.map(_.toString).grouped(3).map(_.mkString(" ")).mkString(" | ")
  }.grouped(3).map(_.mkString("\n")).mkString("\n---------------------\n")
}




}
