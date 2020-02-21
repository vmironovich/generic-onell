lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies ++= Seq(ojSolver, scalaTest),
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.1.1" % Test
lazy val ojSolver   = "org.ojalgo" % "ojalgo" % "48.1.0"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "generic-onell", version := "0.0.0")
