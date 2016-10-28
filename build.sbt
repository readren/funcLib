name := """func-lib"""
organization := "readren"

version := "0.1.1-SNAPSHOT"

scalaVersion := "2.11.8"

// Testing dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

scalacOptions ++= Seq("-feature","-deprecation","-Ywarn-numeric-widen")
