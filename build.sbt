
// General

organization := "id.nolimit"

name := """hmm-scala"""

version := "1.1.2"

scalaVersion := "2.10.3"

scalacOptions += "-deprecation"


// Code Formatting

scalariformSettings


// Testing

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.10" % "test",
  "org.scalatest" %% "scalatest" % "2.1.5",
  "org.scalatest" %% "scalatest" % "2.1.5",
  "com.typesafe.play" %% "play-json" % "2.3.9",
  "joda-time" % "joda-time" % "2.9.2")


// Publishing

publishTo := Some("NoLimit Nexus" at "http://nexus.nolimit.id/nexus/content/repositories/hmm-scala")
