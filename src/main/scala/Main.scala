package sudoku

import zio._

object Main extends ZIOAppDefault {

  

  def run: ZIO[Any, Throwable, Unit] =
    for {
      _ <- Console.print("Enter the path to the JSON file containing the Sudoku problem:")
      path <- Console.readLine
      _ <-  Console.printLine(s"You entered: $path")
      // Add your Sudoku solver logic here, utilizing ZIO and interacting with the ZIO Console

      JSON_Sudoku_Data <- openFile(path).catchAll { error =>
      for {
        _ <- ZIO.logErrorCause("Could not open primary file", Cause.fail(new Throwable(error)))
        _ <- Console.print("Enter another or the right path to a JSON file:")
        backupPath <- Console.readLine
        file <- openFile(backupPath)
      } yield file 
      _ <-  Console.printLine("Here is a nice view of our sudoku")
      prettyString(JSON_Sudoku_Data)
      }




    } yield ()

    object Sudoku {
      
  }

}