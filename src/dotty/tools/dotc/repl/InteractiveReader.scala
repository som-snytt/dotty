/* NSC -- new Scala compiler
 * Copyright 2005-2016 LAMP/EPFL
 * @author Stepan Koltsov
 */
package dotty.tools
package dotc
package repl

import scala.util.Properties.{isMac}
import scala.util.{Try, Success, Failure}
import scala.tools.nsc.Properties.{isEmacsShell}
import java.io.{BufferedReader, PrintWriter}
import java.io.IOException
import core.Contexts.Context

import InteractiveReader._

/** Reads lines from an input stream */
trait InteractiveReader {
  def postInit(): Unit = {}

  val interactive: Boolean

  def reset(): Unit
  def history: History
  def completion: Completion
  def redrawLine(): Unit

  def readYesOrNo(prompt: String, alt: => Boolean): Boolean = readOneKey(prompt) match {
    case 'y'  => true
    case 'n'  => false
    case -1   => false // EOF
    case _    => alt
  }

  protected def readOneLine(prompt: String): String
  protected def readOneKey(prompt: String): Int

  /** The next line of input, or null for EOF. */
  def readLine(prompt: String): String =
    // hack necessary for OSX jvm suspension because read calls are not restarted after SIGTSTP
    if (isMac) restartSysCalls(readOneLine(prompt), reset())
    else readOneLine(prompt)
}

object InteractiveReader {
  val msgEINTR = "Interrupted system call"
  private def restartSysCalls[R](body: => R, reset: => Unit): R =
    try body catch {
      case e: IOException if e.getMessage == msgEINTR => reset ; body
    }

  /** Tries to create a `scala.repl.reader` aka JLineReader, falling back to SimpleReader.
   *  The constructor of the InteractiveReader must take a Completion strategy,
   *  supplied as a `() => Completion`; the Completion object provides a concrete Completer.
   */
  def newReader(implicit ctx: Context): InteractiveReader =
    Option(ctx) filter (!_.settings.Xnojline && !isEmacsShell) flatMap (createReader(_)) getOrElse SimpleReader()

  private def createReader(implicit ctx: Context): Option[InteractiveReader] = {
    type Completer = () => Completion
    type ReaderMaker = Completer => InteractiveReader

    def instantiater(className: String): ReaderMaker = completer => {
      if (ctx.settings.debug) ctx.println(s"Trying to instantiate an InteractiveReader from $className")
      Class.forName(className).getConstructor(classOf[Completer]).newInstance(completer).asInstanceOf[InteractiveReader]
    }
    def defaultCompleter = NoCompletion // new PresentationCompilerCompleter(intp)
    def mkReader(maker: ReaderMaker) = maker(() => if (ctx.settings.noCompletion) NoCompletion else defaultCompleter)
    //def internalClass(kind: String)  = s"scala.tools.nsc.interpreter.$kind.InteractiveReader"
    val readerClasses = sys.props.get("scala.repl.reader").toStream ++ Stream(/*internalClass("jline"), internalClass("jline_embedded"),*/ "dotty.tools.dotc.repl.jline.InteractiveReader")
    val readers       = readerClasses map (cls => Try { mkReader(instantiater(cls)) })
    val reader        = (readers collect { case Success(reader) => reader }).headOption

    if (ctx.settings.debug) {
      val readerDiags = (readerClasses, readers).zipped map {
        case (cls, Failure(e)) => s"  - $cls --> \n\t" + scala.tools.nsc.util.stackTraceString(e) + "\n"
        case (cls, Success(_)) => s"  - $cls OK"
      }
      ctx.println(s"All InteractiveReaders tried: ${readerDiags.mkString("\n","\n","\n")}")
    }
    reader
  }
}

/** Splash screen to collect user input while the REPL is initializing on the main thread.
 *  The supplied reader is used for input.
 */
case class SplashLoop(reader: InteractiveReader, prompt: String) extends Runnable {
  import java.util.concurrent.SynchronousQueue
  import java.util.concurrent.atomic.AtomicBoolean
  import scala.compat.Platform.EOL

  /** Read one line which can be retrieved with `line`.
   *  If the line resolves to a `:paste` command, the pasted
   *  text is returned.
   */
  def run(): Unit = {
    var line = ""
    try
      do {
        line = reader.readLine(prompt)
        if (line != null) {
          line = process(line.trim)
        }
      } while (line != null && line.isEmpty && isRunning)
    finally {
      result put Option(line)
    }
  }

  private def process(line: String): String = {
    def isPrefix(s: String, p: String, n: Int) = (
      s != null && s.length >= n && s.length <= p.length && line == (p take s.length)
    )
    if (isPrefix(line, ":paste", 3)) {
      // while collecting lines, check running flag
      var help = f"// Entering paste mode (ctrl-D to finish)%n%n"
      def readWhile(cond: String => Boolean) = {
        Iterator continually reader.readLine(help) takeWhile { x =>
          help = ""
          x != null && cond(x)
        }
      }
      val text = (readWhile(_ => isRunning) mkString EOL).trim
      val next =
        if (text.isEmpty) "// Nothing pasted, nothing gained."
        else "// Exiting paste mode, now interpreting."
      Console println f"%n${next}%n"
      text
    } else {
      line
    }
  }

  private val result = new SynchronousQueue[Option[String]]
  private val running = new AtomicBoolean(false)
  private var thread: Thread = _

  def isRunning = running.get

  def start(): Unit = synchronized {
    require(thread == null, "Already started")
    thread = new Thread(this)
    running.set(true)
    thread.start()
  }

  def stop(): Unit = synchronized {
    running.set(false)
    if (thread != null) thread.interrupt()
    thread = null
  }

  def line: String = result.take getOrElse null
}

/** Reader during splash. Handles splash-completion, otherwise delegates. */
class SplashReader(reader: InteractiveReader, postIniter: InteractiveReader => Unit) extends InteractiveReader {
  /** Invoke the postInit action with the underlying reader. */
  override def postInit(): Unit = postIniter(reader)

  override val interactive: Boolean = reader.interactive

  override def reset(): Unit = reader.reset()
  override def history: History = reader.history
  override val completion: Completion = NoCompletion
  override def redrawLine(): Unit = reader.redrawLine()

  override protected def readOneLine(prompt: String): String = ???
  override protected def readOneKey(prompt: String): Int     = ???

  override def readLine(prompt: String): String = reader.readLine(prompt)
}
object SplashReader {
  def apply(reader: InteractiveReader)(postIniter: InteractiveReader => Unit) =
    new SplashReader(reader, postIniter)
}

/** Reads using standard JDK API */
class SimpleReader(
  in: BufferedReader,
  out: PrintWriter,
  val interactive: Boolean)
extends InteractiveReader
{
  val history = NoHistory
  val completion = NoCompletion

  def reset() = ()
  def redrawLine() = ()

  // InteractiveReader internals
  protected def readOneLine(prompt: String): String = {
    echo(prompt)
    readOneLine()
  }
  protected def readOneKey(prompt: String) = sys.error("No char-based input in SimpleReader")

  protected def readOneLine(): String = in.readLine()
  protected def echo(s: String): Unit = if (interactive) {
    out.print(s)
    out.flush()
  }
}
object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new PrintWriter(Console.out)

  def apply(in: BufferedReader = defaultIn, out: PrintWriter = defaultOut, interactive: Boolean = true): SimpleReader =
    new SimpleReader(in, out, interactive)
}

// pretend we are a console for verbose purposes
trait EchoReader extends SimpleReader {
  // if there is more input, then maybe echo the prompt and the input
  override def readOneLine(prompt: String) = {
    val input = readOneLine()
    if (input != null) echo(f"$prompt$input%n")
    input
  }
}
