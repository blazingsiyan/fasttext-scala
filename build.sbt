organization in ThisBuild := "com.doubajam"

version in ThisBuild := "0.0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fastText.scala"
  )
