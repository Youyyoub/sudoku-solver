package sudoku

import zio._

object Main extends ZIOAppDefault {
  
  val sudoku_test_grid = Array(
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

  def run: ZIO[Environment & (ZIOAppArgs & Scope), Any, Any] = {

    for {
      // _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem:")
      // path <- Console.readLine
      // _ <-  Console.printLine(s"You entered: $path")
    
      // Add your Sudoku solver logic here, utilizing ZIO and interacting with the ZIO Console

      _ <- Console.printLine(Sudoku.prettyString(sudoku_test_grid))
    
    } yield ()

  }
}

object Sudoku {
  type Board = Array[Array[Int]]

  def prettyString(sudoku: Board): String = {
    sudoku.grouped(3).map { bigGroup =>
      bigGroup.map { row =>
        row.grouped(3).map { smallGroup =>
          smallGroup.mkString(" ", " ", " ")
        }.mkString("|", "|", "|")
      }.mkString("\n")
    }.mkString("+-------+-------+-------+\n", "\n+-------+-------+-------+\n", "\n+-------+-------+-------+")
  }

}