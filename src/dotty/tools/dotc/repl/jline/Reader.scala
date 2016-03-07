/** NSC -- new Scala compiler
  *
  * Copyright 2005-2015 LAMP/EPFL
  * @author Stepan Koltsov
  * @author Adriaan Moors
  */

package dotty.tools.dotc
package repl
package jline

import java.util.{Collection => JCollection, List => JList}

import _root_.jline.{console => jconsole}
import jconsole.ConsoleReader
import jconsole.completer.{CompletionHandler, Completer}
import jconsole.history.{History => JHistory}
import jconsole.completer.ArgumentCompleter.{ ArgumentDelimiter, ArgumentList }

/** Reads from the console using JLine.
 *
 *  Eagerly instantiates all relevant JLine classes, so that we can detect
 *  linkage errors on `new InteractiveReader` and retry.
 */
class InteractiveReader(completer: () => Completion) extends repl.InteractiveReader {
  val interactive = true

  val history: History = new JLineHistory.JLineFileHistory()

  private val consoleReader = {
    val reader = new JLineConsoleReader()

    reader setPaginationEnabled false // interpreter.`package`.isPaged

    // ASAP
    reader setExpandEvents false

    reader setHistory history.asInstanceOf[JHistory]

    reader
  }

  private[this] var _completion: Completion = NoCompletion
  def completion: Completion = _completion

  override def postInit() = {
    _completion = completer()

    consoleReader.initCompletion(completion)
  }

  def reset()                     = consoleReader.getTerminal().reset()
  def redrawLine()                = consoleReader.redrawLineAndFlush()
  def readOneLine(prompt: String) = consoleReader.readLine(prompt)
  def readOneKey(prompt: String)  = consoleReader.readOneKey(prompt)
}

// implements a jline interface
private class JLineConsoleReader extends jconsole.ConsoleReader with scala.tools.nsc.interpreter.VariColumnTabulator {
  val isAcross   = false // interpreter.`package`.isAcross
  val marginSize = 3

  def width  = getTerminal.getWidth()
  def height = getTerminal.getHeight()

  private def morePrompt = "--More--"

  private def emulateMore(): Int = {
    val key = readOneKey(morePrompt)
    try key match {
      case '\r' | '\n' => 1
      case 'q' => -1
      case _ => height - 1
    }
    finally {
      eraseLine()
      // TODO: still not quite managing to erase --More-- and get
      // back to a scala prompt without another keypress.
      if (key == 'q') {
        putString(getPrompt())
        redrawLine()
        flush()
      }
    }
  }

  override def printColumns(items: JCollection[_ <: CharSequence]): Unit = {
    //import scala.tools.nsc.interpreter.javaCharSeqCollectionToScala
    //printColumns_(items: List[String])
    import scala.collection.JavaConverters._
    printColumns_(items.asScala.toList map ("" + _))
  }

  private def printColumns_(items: List[String]): Unit = if (items exists (_ != "")) {
    val grouped = tabulate(items)
    var linesLeft = if (isPaginationEnabled()) height - 1 else Int.MaxValue
    grouped foreach { xs =>
      println(xs.mkString)
      linesLeft -= 1
      if (linesLeft <= 0) {
        linesLeft = emulateMore()
        if (linesLeft < 0)
          return
      }
    }
  }

  def readOneKey(prompt: String) = {
    this.print(prompt)
    this.flush()
    this.readCharacter()
  }

  def eraseLine() = resetPromptLine("", "", 0)

  def redrawLineAndFlush(): Unit = {
    flush(); drawLine(); flush()
  }

  // A hook for running code after the repl is done initializing.
  def initCompletion(completion: Completion): Unit = {
    this setBellEnabled false

    // adapt the JLine completion interface
    def completer =
      new Completer {
        import Completion.Candidates
        val tc = completion
        def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
          val buf = if (_buf == null) "" else _buf
          val Candidates(newCursor, newCandidates) = completion.complete(buf, cursor)
          newCandidates foreach (candidates add _)
          newCursor
        }
      }

    completion match {
      case NoCompletion       => ()
      case _                  => this addCompleter completer
    }

    // This is a workaround for https://github.com/jline/jline2/issues/208
    // and should not be necessary once we upgrade to JLine 2.13.1
    ///
    // Test by:
    // scala> {" ".char}<LEFT><TAB>
    //
    // And checking we don't get an extra } on the line.
    ///
    val handler = getCompletionHandler
    setCompletionHandler(new CompletionHandler {
      override def complete(consoleReader: ConsoleReader, list: JList[CharSequence], i: Int): Boolean = {
        try {
          handler.complete(consoleReader, list, i)
        } finally if (getCursorBuffer.cursor != getCursorBuffer.length()) {
          print(" ")
          getCursorBuffer.write(' ')
          backspace()
        }
      }
    })
    setAutoprintThreshold(400) // max completion candidates without warning
  }
}

// implements a jline interface
class JLineDelimiter extends ArgumentDelimiter {
  def toJLine(args: List[String], cursor: Int): ArgumentList = args match {
    case Nil => new ArgumentList(new Array[String](0), 0, 0, cursor)
    case xs => new ArgumentList(xs.toArray, xs.size - 1, xs.last.length, cursor)
  }

  def delimit(buffer: CharSequence, cursor: Int) = {
    val p = scala.tools.nsc.interpreter.Parsed(buffer.toString, cursor)
    toJLine(p.args, cursor)
  }

  def isDelimiter(buffer: CharSequence, cursor: Int) = scala.tools.nsc.interpreter.Parsed(buffer.toString, cursor).isDelimiter
}
