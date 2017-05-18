
scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

assemblyJarName in assembly := "skillView.jar"
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}
test in assembly := {}
