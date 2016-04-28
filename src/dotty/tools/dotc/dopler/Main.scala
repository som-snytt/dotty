package dotty.tools
package dotc
package dopler

import core._
import Contexts.Context
import Contexts.FreshContext
import reporting._
import io.VirtualFile
import util.{Attachment, SourceFile, ScriptSourceFile}
import Names._
import StdNames._
import Phases.Phase
import repl.{InteractiveReader, SimpleReader, SplashReader, SplashLoop}

import Flags._
import Names._
import ast._
import Trees._
import Decorators._
import Reporter.{Error, ConditionalWarning, MigrationWarning, Warning, Info}

import scala.annotation._
import scala.collection.mutable
import scala.reflect.internal.util.ScalaClassLoader
import scala.util.control.NonFatal
import java.io.{BufferedWriter, OutputStreamWriter}

/** A resident interpreter for dotty.
 */
object Main extends Driver {

  var repl: Option[ReadEvalPrint] = None

  import Implicits._

  object Prompts {
    def prompt1 = "dopler> "
    def prompt2 = {
      val f = f"%%${prompt1.length}s"
      f format "| "
    }
  }
  import Prompts._

  val prompt = f"%n$prompt1"   // scala.tools.nsc.Properties.shellPromptString

  // An ordinary compiler with default front-end scripter.
  // TODO settings
  override def newCompiler(implicit ctx: Context): Compiler = new ReplCompiler

  def newReader(implicit ctx: Context): InteractiveReader = Configuration.newReader(ctx)

  /** Compiles source snippets supplied as strings. */
  override protected def doCompile(compiler: Compiler, snippets: List[String])(implicit ctx: Context): Reporter =
    if (snippets.isEmpty) emptyReporter else
      try {
        val scripts = snippets map { code =>
          val file = new VirtualFile(code)
          val writer = new BufferedWriter(new OutputStreamWriter(file.output, "UTF-8"))
          writer.write(code)
          writer.close()
          ScriptSourceFile(file, file.toCharArray)
        }
        val run = compiler.newRun
        run.compileSources(scripts)
        if (ctx.reporter.hasErrors) run.printSummary() else ctx.reporter
      }
      catch {
        case ex: FatalError  =>
          ctx.error(ex.getMessage) // signals that we should fail compilation.
          ctx.reporter
      }

  override def sourcesRequired = false

  /** Run the current line of scripted thing.
   *
   *  Adds the output dir to class loader and runs the (detected) main class.
   */
  def runScript(repl: ReadEvalPrint)(implicit ctx: Context): Reporter = {
    val urls = List(ctx.settings.d.value) map (d => new java.io.File(d).toURI.toURL)
    val loader = ScalaClassLoader.fromURLs(urls, getClass.getClassLoader)
    val main = s"${repl.line}.${nme.PACKAGE}" //s"script.${nme.PACKAGE}"

    /*
    val main = ctx.settings.mainClass.value match {
      case "" => s"script.${nme.PACKAGE}"
      case p  => s"$p.${nme.PACKAGE}"
    }
    */
    loader.tryToInitializeClass(main) foreach { `class`: Class[_] =>
      val init = `class`.getMethod("main", classOf[Array[String]])
      val eval = `class`.getMethod("toString")

      val res = try loader.asContext {
        init.invoke(null, Array.empty[String])
        eval.invoke(null).toString
      } catch {
        case e: Throwable => e.printStackTrace() ; "<error>"
      }
      ctx.echo(res)
    }
    ctx.reporter
  }

  /** Reader to use before interpreter is online. */
  def preLoop(implicit ctx: Context) = {
    val welcome = Option(Properties.welcome) filter (!_.isEmpty)
    val sr = SplashReader(newReader(ctx)) { r =>
      r.postInit()
    }
    welcome foreach (ctx.echo(_))
    SplashLoop(sr, prompt)
  }

  def postLoop = ()

  /** Warm up the compiler by compiling useful stuff. */
  def init(compiler: Compiler)(implicit ctx: Context): Unit = {
    val code = """println("Hello")"""
      /*
      s"""def exit(): Nothing = sys.exit(0)
          println("initialized...")"""
          */
    doCompile(compiler, List(code))
  }

  /** Start interpreting with the given line of text. */
  def loop(compiler: ReplCompiler, line: String, reader: InteractiveReader)(implicit ctx: Context): Unit = {
    val crashRecovery: PartialFunction[Throwable, Boolean] = {
      case e => println("Oops") ; e.printStackTrace() ; false
    }
    @tailrec def processLine(line: String): Boolean = {
      var incomplete = false
      val reporter = ctx.reporter.withIncompleteHandler(_ => _ => incomplete = true) {
        doCompile(compiler, List(line))
      }
      if (incomplete) {
        reader readLine prompt2 match {
          case null => false
          case rest => processLine(s"$line\n$rest")
        }
      }
      else !reporter.hasErrors
    }
    @tailrec def loopLine(line: String): Unit = line match {
      case null => ()
      case _    => if (try processLine(line) catch crashRecovery) loopLine(reader readLine prompt)
    }
    loopLine(line)
  }

  /** Process args, get an initial line of input, create a compiler, start interpreting. */
  override def process(args: Array[String], rootCtx: Context): Reporter = {
    val (fileNames, setupContext) = setup(args, rootCtx)
    implicit val ctx: FreshContext = setupContext.fresh
    ctx.setReporter(new ReplReporter)

    ctx.echo("[info] started at " + new java.util.Date)
    if (fileNames.nonEmpty) ctx.echo("Processing no files...")

    // don't print verbose diagnostics during startup
    def withSuppressedSettings[A](body: => A): A = {
      body
    }
    // a compiler, a line of input, and a reader
    def startup: (Compiler, Option[String], InteractiveReader) = withSuppressedSettings {
      // let them start typing
      val splash = preLoop
      splash.start()

      // while we go fire up the REPL
      try {
        val repl = newCompiler
        val input = 
          if (ctx.reporter.hasErrors) {
            ctx.echo("Interpreter encountered errors during initialization!")
            None
          } else {
            init(repl)
            // what they typed in while they were waiting, null on ^D
            Some(splash.line)
          }
        (repl, input, splash.reader)
      } finally splash.stop()
    }

    startup match {
      case (repl, Some(null), _) =>
        try ctx echo scala.tools.nsc.Properties.shellInterruptedString
        finally ()  // repl.close()
      case (_, None, _)          => Console println s"line was error"
      case (repl: ReplCompiler, Some(line), reader) => loop(repl, line, reader)
      case _ => ()
    }
    ctx.reporter
  }
}

object Implicits {

  implicit class `msg color`(val sc: StringContext) extends AnyVal {
    import scala.io.AnsiColor.{ RED => Red, YELLOW => Amber, RESET => Reset }
    import Diagnostic._
    def msg(args: Any*)(implicit ctx: Context): String = {
      def colorize(d: Diagnostic, s: String) = {
        if (ctx.settings.couleurs)
          d match {
            case d: Error            => s"${Red}${s}${Reset}"
            case d: MigrationWarning => s"${Amber}${s}${Reset}"
            case d: Warning          => s"${Amber}${s}${Reset}"
            case _                   => s
          }
        else s
      }
      val colorized =
        args map {
          case d: Error            => s"${colorize(d, "error:")} ${d.message}"
          case d: MigrationWarning => s"${colorize(d, "migration warning:")} ${d.message}"
          case d: Warning          => s"${colorize(d, "warning:")} ${d.message}"
          case d: Diagnostic       => d.message
          case other               => other
        }
      sc.s(colorized: _*)
    }
  }
}

object Configuration {
  def newReader(implicit ctx: Context): InteractiveReader = InteractiveReader.newReader
}

object Properties {

  import scala.util.Properties._
  import scala.tools.nsc.Properties._

  def shellWelcomeString = "Welcome to doti"  // property added 2.11.8

  def welcome = shellWelcomeString
}

class ReplReporter extends ConsoleReporter with ColorCommentary

trait ColorCommentary { _: ConsoleReporter =>

  import Implicits._

  override def doReport(d: Diagnostic)(implicit ctx: Context): Unit = d match {
    case d: Error =>
      printMessageAndPos(msg"$d", d.pos)
      if (ctx.settings.prompt.value) displayPrompt()
    case d: ConditionalWarning if !d.enablingOption.value =>
    case d: MigrationWarning =>
      printMessageAndPos(msg"$d", d.pos)
    case d: Warning =>
      printMessageAndPos(msg"$d", d.pos)
    case _ =>
      printMessageAndPos(d.message, d.pos)
  }

}

/** Introduces untyped REPL templater, Reprinter for type-aware templater, and Collector to run the REPL. */
class ReplCompiler extends Compiler {
  // must be def
  override def phases = List(new ReplFrontEnd) :: List(new Reprinter) :: super.phases.tail ::: List(List(new Collector))
}

/** Introduce REPL templating after parser. */
class ReplFrontEnd extends typer.FrontEnd {
  override val postparser: Replater = new Replater
}
