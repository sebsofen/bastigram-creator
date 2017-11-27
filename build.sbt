name := "bastigram-creator"

version := "0.1"

scalaVersion := "2.12.4"
organization := "de.bastigram"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-http-spray-json" % "10.0.8",
"com.lightbend.akka" %% "akka-stream-alpakka-file" % "0.14",
"com.typesafe.akka" %% "akka-actor" % "2.5.6")

