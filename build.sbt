name := "scala-lamda-calculus-sbt"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions"
)


libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"