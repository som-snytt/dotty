
// test argument processing and "execution mode" in runner
object Test:
  import dotty.tools.runner.RichClassLoader.*
  val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(getClass.getClassLoader)
  def main(args: Array[String]): Unit =
    getClass.getClassLoader.run("echo", List("hello", "raw", "world"))
    getClass.getClassLoader.run("dotty.tools.MainGenericRunner", List("echo", "hello", "rawr", "world"))
    // caution: uses "SCALA_OPTS"
    dotty.tools.MainGenericRunner.main(Array("--class-path", classpath, "echo", "hello", "run", "world"))

@main def echo(args: String*): Unit = println {
  args.mkString(" ")
}
