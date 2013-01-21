import AssemblyKeys._ // for assembly plugin

name := "sdm"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq(
  "Typesafe releases" 
     at "http://repo.typesafe.com/typesafe/repo"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
)

// Tell sbt that we want to see stack traces automatically
traceLevel in run := 0

//jarName in assembly := "sdm.jar"

seq(assemblySettings: _*)

// make assembly skip the tests
test in assembly := {}

// Disable parallel execution of tests (messes up test output)
parallelExecution in Test := false
