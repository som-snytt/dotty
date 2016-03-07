package dotty.tools
package dotc

import core._
import Contexts.Context
import reporting._
import io.PlainFile
import util.{SourceFile, ScriptSourceFile}
import Names._
import StdNames._
import scala.reflect.internal.util.ScalaClassLoader

/** A script runner that compiles and runs from source.
 */
object Scripted extends Driver {
  // An ordinary compiler with default front-end scripter.
  // TODO settings
  override def newCompiler(implicit ctx: Context): Compiler = new Compiler

  override protected def doCompile(compiler: Compiler, fileNames: List[String])(implicit ctx: Context): Reporter =
    if (fileNames.isEmpty) emptyReporter else
      try {
        val run = compiler.newRun
        val scripts = fileNames map { name =>
          val file = new PlainFile(name)
          ScriptSourceFile(file, file.toCharArray)
        }
        run.compileSources(scripts)
        run.printSummary()
      }
      catch {
        case ex: FatalError  =>
          ctx.error(ex.getMessage) // signals that we should fail compilation.
          ctx.reporter
      }

  /** Run the main thing.
   *
   *  Adds the output dir to class loader and runs the (detected) main class.
   *
   *  Current modus:
   *  scala -J-Xbootclasspath/a:.:target/scala-2.11/classes/ HelloWorld
   */
  protected def runScript(implicit ctx: Context): Reporter = {
    val urls = List(ctx.settings.d.value) map (d => new java.io.File(d).toURI.toURL)
    val loader = ScalaClassLoader.fromURLs(urls, getClass.getClassLoader)
    val main = ctx.settings.mainClass.value match {
      case "" => s"script.${nme.PACKAGE}"
      case p  => s"$p.${nme.PACKAGE}"
    }
    loader.run(main, List.empty[String])
    ctx.reporter
  }

  /** Process args, compile and run. */
  override def process(args: Array[String], rootCtx: Context): Reporter = {
    val (fileNames, setupContext) = setup(args, rootCtx)
    implicit val ctx: Context = setupContext
    val reporter = doCompile(newCompiler, fileNames)
    if (!reporter.hasErrors) runScript else reporter
  }
}
