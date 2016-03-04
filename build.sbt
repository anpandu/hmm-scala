
// General

organization := "org.example"

name := """hmm-scala"""

version := "0.2.0"

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

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomExtra :=
  <url>http://org.example/hello-sbt</url>
    <licenses>
      <license>
        <name>CC0 1.0 Universal</name>
        <url>https://github.com/exampleorg/hello-sbt/blob/master/LICENSE</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:exampleorg/hello-sbt.git</url>
      <connection>scm:git:git@github.com:exampleorg/hello-sbt.git</connection>
    </scm>
    <developers>
      <developer>
        <id>mr-activator</id>
        <name>Mr. Activator</name>
        <url>http://org.example/mr-activator</url>
      </developer>
    </developers>

