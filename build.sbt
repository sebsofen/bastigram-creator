name := "bastigram-creator"

version := "0.1"

scalaVersion := "2.12.4"
organization := "de.bastigram"

libraryDependencies ++= Seq("com.lightbend.akka" %% "akka-stream-alpakka-file" % "0.14")

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"