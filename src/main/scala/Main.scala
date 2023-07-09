package sudoku

import zio._
import scala.io.Source
import zio.Console._

object Main extends ZIOAppDefault {
  def run: ZIO[Any, Nothing, ExitCode] =
    (for {
      _ <- printLine("Enter the path to the JSON file containing the Sudoku problem:")
      path <- readLine
      _ <- printLine(s"You entered: $path")

      jsonSudokuData <- openFile(path).catchAll { error =>
        for {
          _ <- printLine("Could not open primary file")
          _ <- printLine("Enter another or the right path to a JSON file:")
          backupPath <- readLine
          file <- openFile(backupPath)
        } yield file
      }
      _ <- printLine("Here is a nice view of our sudoku")
      // Add your Sudoku solver logic here, utilizing ZIO and interacting with the ZIO Console

    } yield ()).exitCode

  def openFile(path: String): ZIO[Any, Throwable, Array[String]] =
    ZIO.succeed(Source.fromFile(path).getLines.toArray)
}
