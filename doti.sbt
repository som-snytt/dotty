
import sbt._

fork in run := true

connectInput in run := true

outputStrategy in run := Some(StdoutOutput)

//libraryDependencies in run ++= Seq("org.scala-lang" % "scala-compiler" % "2.12.0-M3" % Runtime)

//addCommandAlias("repl",   "repl/compile:runMain dotty.tools.dotc.dopler.Main -usejavacp")
addCommandAlias("repl",   "compile:runMain dotty.tools.dotc.dopler.Main -usejavacp -Ydebug -Ycouleurs")
