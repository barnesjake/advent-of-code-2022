ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2022"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.1.0" % Test
