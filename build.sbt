lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies ++= Seq(scalaPar, scalaTest),
  scalaVersion := "2.13.0",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaPar   = "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.1.0" % Test

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "generic-onell", version := "0.0.0")
