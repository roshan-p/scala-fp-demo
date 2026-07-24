ThisBuild / scalaVersion := "3.7.3"
ThisBuild / organization := "com.roshanp"

lazy val root = (project in file("."))
  .settings(
    name    := "scala-fp-demo",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
