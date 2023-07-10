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
    Array(0, 0, 0, 0, 8, 0, 0, 7, 9)
  )

  def run: ZIO[Any, Throwable, Unit] = {
    for {
      // _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem: ")
      // path <- Console.readLine
      // _ <- Console.printLine(s"You entered: $path")

      // jsonSudokuData <- Sudoku.openFile(path).catchAll { error =>
      //   for {
      //     _ <- Console.printLine("Could not open primary file")
      //     _ <- Console.printLine("Enter another or the right path to a JSON file:")
      //     backupPath <- Console.readLine
      //     jsonSudokuData <- Sudoku.openFile(backupPath)
      //   } yield jsonSudokuData
      // }

      _ <- Console.printLine("Loaded Sudoku: ")
      _ <- Sudoku.solve(sudokuTestGrid) match {
        case Some(solution) =>
          Console.printLine(Sudoku.prettyString(solution) + "\n")
        case None => Console.printLine("No solution found.\n")
      }

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
      row9: String
  )

  implicit val SudokuGridDecoder: JsonDecoder[SudokuGrid] =
    DeriveJsonDecoder.gen[SudokuGrid]

  def openFile(path: String): ZIO[Any, Throwable, Array[String]] =
    ZIO.succeed(Source.fromFile(path).getLines.toArray)

  def convertStringArray2IntArray(jsonSudoku: Array[String]): Board = {
    return null
  }

  def prettyString(sudoku: Board): String = {
    sudoku
      .grouped(3)
      .map { bigGroup =>
        bigGroup
          .map { row =>
            row
              .grouped(3)
              .map { smallGroup =>
                smallGroup.mkString(" ", " ", " ")
              }
              .mkString("|", "|", "|")
          }
          .mkString("\n")
      }
      .mkString(
        "+-------+-------+-------+\n",
        "\n+-------+-------+-------+\n",
        "\n+-------+-------+-------+"
      )
  }

  def validate(sudoku: Board, x: Int, y: Int, value: Int): Boolean = {
    val row = sudoku(y)
    val rowPropertiy = !row.contains(value)

    val column = sudoku.map(r => r.apply(x))
    val columnProperty = !column.contains(value)

    val boxX = x / 3
    val boxY = y / 3
    val box = for {
      yb <- (boxY * 3) until (boxY * 3 + 3)
      xb <- (boxX * 3) until (boxX * 3 + 3)
    } yield sudoku(yb)(xb)

    val boxProperty = !box.contains(value)

    rowPropertiy && columnProperty && boxProperty
  }

  def solve(sudoku: Board, x: Int = 0, y: Int = 0): Option[Board] = {
    if (y == 9) {
      return Some(sudoku.map(_.clone)) // We reached the final solution
    }

    if (sudoku(y)(x) != 0) {
      nextCell(sudoku, x, y)
    } else {
      (1 to 9).foldLeft[Option[Board]](None) { (acc, value) =>
        if (acc.isDefined) {
          acc
        } else {
          if (validate(sudoku, x, y, value)) {
            sudoku(y)(x) = value
            val solved = nextCell(sudoku, x, y)
            if (solved.isDefined) {
              solved
            } else {
              sudoku(y)(x) = 0
              None
            }
          } else None
        }
      }
    }
  }

  def nextCell(sudoku: Board, x: Int, y: Int): Option[Board] = {
    if (x == 8) {
      solve(sudoku, 0, y + 1)
    } else {
      solve(sudoku, x + 1, y)
    }
  }

}
