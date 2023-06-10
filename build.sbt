ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "fpinscala",
    idePackagePrefix := Some("fpinscala")
  )

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.16",
  "org.scalatest" %% "scalatest" % "3.2.16" % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
)
