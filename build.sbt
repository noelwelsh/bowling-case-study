ThisBuild / scalaVersion := "2.13.1"
ThisBuild / useSuperShell := false

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.12.0",
  "com.davegurnell" %% "unindent" % "1.8.0",
  "org.scalacheck" %% "scalacheck" % "1.18.0" % "test",
  "org.scalatest" %% "scalatest" % "3.2.19"
)
