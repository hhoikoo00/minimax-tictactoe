name := "scala"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq("-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"