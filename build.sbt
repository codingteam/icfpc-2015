name := "codingteam-icfpc-2015"

version := "0.0.1"

mainClass in (Compile, run) := Some("ru.org.codingteam.icfpc.Application")

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "upickle" % "0.3.4",
  "org.scalatest" %% "scalatest" % "2.1.5" % "test"
)
