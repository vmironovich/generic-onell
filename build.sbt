val circeVersion = "0.12.3"

val scalaTest  = "org.scalatest" %% "scalatest" % "3.2.2" % Test

val commonSettings = Seq(
  organization := "ru.ifmo",
  libraryDependencies ++= Seq(scalaTest),
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)



libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "generic-onell", version := "0.0.0")
