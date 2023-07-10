val zioVersion = "2.0.15"
val zioJsonVersion =  "0.6.0"
val zioNioVersion =  "2.0.1"
val scala3Version = "3.3.0"
libraryDependencies += "io.circe" %% "circe-parser" % "0.14.1"
libraryDependencies += "io.circe" %% "circe-core" % "0.14.1"
libraryDependencies += "io.circe" %% "circe-generic" % "0.14.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sudoku-solver",
    version := "1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-json" % zioJsonVersion,
      "dev.zio" %% "zio-nio" % zioNioVersion
    ).map(_ % Compile),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29"
    ).map(_ % Test)
  )