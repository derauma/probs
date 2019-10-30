ThisBuild / version := "0.1.1"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "org.infotsu"

lazy val metarep = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "metarep",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.8",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
      "org.scalanlp" %% "breeze" % "1.0",
      "org.scalanlp" %% "breeze-natives" % "1.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    )
)
