name := "generalized-n-queens-problem"

val scalaV = "2.12.4"
scalaVersion in ThisBuild := scalaV

lazy val generalizedNQueensProblem = project.in(file(".")) settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaV,
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  ))
