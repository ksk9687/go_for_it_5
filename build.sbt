import AssemblyKeys._

seq(assemblySettings: _*)

name := "Elevator_scheduling"

version := "1.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

mainClass in assembly := Some("vc.ksk.elevator.Main")

jarName in assembly <<= name {_ + ".jar"}

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.1" % "test"