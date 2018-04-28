organization := "com.example"

scalaVersion := "2.13.0-M2"

version      := "0.1.0-SNAPSHOT"

name := "shapeless-type-astronaut"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scala-lang" % "scala-reflect" % "2.13.0-M2",
  "org.scala-lang" % "scala-compiler" % "2.13.0-M2"
)

scalacOptions := Seq("-unchecked", "-deprecation")
