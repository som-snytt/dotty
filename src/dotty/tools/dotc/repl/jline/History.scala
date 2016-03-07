/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package dotty.tools.dotc
package repl
package jline

import java.util.{Iterator => JIterator, ListIterator => JListIterator}

import _root_.jline.{console => jconsole}
import jconsole.history.History.{Entry => JEntry}
import jconsole.history.{History => JHistory}

import _root_.jline.console.history.PersistentHistory

import scala.reflect.io.{ File, Path }
import scala.tools.nsc.Properties.{ propOrNone, userHome }


/** A straight scalafication of the jline interface which mixes
 *  in the sparse jline-independent one too.
 */
trait JLineHistory extends JHistory with History {
  def size: Int
  def isEmpty: Boolean
  def index: Int
  def clear(): Unit
  def get(index: Int): CharSequence
  def add(line: CharSequence): Unit
  def replace(item: CharSequence): Unit

  def entries(index: Int): JListIterator[JEntry]
  def entries(): JListIterator[JEntry]
  def iterator: JIterator[JEntry]

  def current(): CharSequence
  def previous(): Boolean
  def next(): Boolean
  def moveToFirst(): Boolean
  def moveToLast(): Boolean
  def moveTo(index: Int): Boolean
  def moveToEnd(): Unit

  override def historicize(text: String): Boolean = {
    text.lines foreach add
    moveToEnd()
    true
  }
}

object JLineHistory {
  class JLineFileHistory extends SimpleHistory with FileBackedHistory {
    override def add(item: CharSequence): Unit = {
      if (!isEmpty && last == item)
        Console.println("Ignoring duplicate entry '" + item + "'")
      else {
        super.add(item)
        addLineToFile(item)
      }
    }
    override def toString = "History(size = " + size + ", index = " + index + ")"

    import scala.collection.JavaConverters._

    override def asStrings(from: Int, to: Int): List[String] =
      entries(from).asScala.take(to - from).map(_.value.toString).toList

    case class Entry(index: Int, value: CharSequence) extends JEntry {
      override def toString = value.toString
    }

    private def toEntries(): Seq[JEntry] = buf.zipWithIndex map { case (x, i) => Entry(i, x)}
    def entries(idx: Int): JListIterator[JEntry] = toEntries().asJava.listIterator(idx)
    def entries(): JListIterator[JEntry] = toEntries().asJava.listIterator()
    def iterator: JIterator[JEntry] = toEntries().iterator.asJava
  }

  def apply(): History = try new JLineFileHistory catch { case x: Exception => new SimpleHistory() }
}

/** TODO: file locking.
  */
trait FileBackedHistory extends JLineHistory with PersistentHistory {
  def maxSize: Int

  protected lazy val historyFile: File = FileBackedHistory.defaultFile
  private var isPersistent = true

  locally {
    load()
  }

  def withoutSaving[T](op: => T): T = {
    val saved = isPersistent
    isPersistent = false
    try op
    finally isPersistent = saved
  }

  def addLineToFile(item: CharSequence): Unit = {
    if (isPersistent)
      append(item + "\n")
  }

  /** Overwrites the history file with the current memory. */
  protected def sync(): Unit = {
    val lines = asStrings map (_ + "\n")
    historyFile.writeAll(lines: _*)
  }

  /** Append one or more lines to the history file. */
  protected def append(lines: String*): Unit = {
    historyFile.appendAll(lines: _*)
  }

  def load(): Unit = {
    if (!historyFile.canRead)
      historyFile.createFile()

    val lines: IndexedSeq[String] = {
      try historyFile.lines().toIndexedSeq
      catch {
        // It seems that control characters in the history file combined
        // with the default codec can lead to nio spewing exceptions.  Rather
        // than abandon hope we'll try to read it as ISO-8859-1
        case _: Exception =>
          try historyFile.lines("ISO-8859-1").toIndexedSeq
          catch {
            case _: Exception => Vector()
          }
      }
    }

    Console.println("Loading " + lines.size + " into history.")

    // avoid writing to the history file
    withoutSaving(lines takeRight maxSize foreach add)
    // truncate the history file if it's too big.
    if (lines.size > maxSize) {
      Console.println("File exceeds maximum size: truncating to " + maxSize + " entries.")
      sync()
    }
    moveToEnd()
  }

  def flush(): Unit = ()

  def purge(): Unit = historyFile.truncate()
}

object FileBackedHistory {
  //   val ContinuationChar = '\003'
  //   val ContinuationNL: String = Array('\003', '\n').mkString

  final val defaultFileName = ".scala_history"

  def defaultFile: File = File(
    propOrNone("scala.shell.histfile") map (Path.apply) getOrElse (Path(userHome) / defaultFileName)
  )
}
