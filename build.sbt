name := "skillView"

version := "0.9"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.8" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

assemblyJarName in assembly := "skillView.jar"
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}
test in assembly := {}
