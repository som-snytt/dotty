package dotty.tools
package dotc
package reporting

import scala.language.unsafeNulls

import java.lang.System.{lineSeparator => EOL}

import core.Contexts._
import core.Decorators._
import printing.Highlighting.{Blue, Red, Yellow}
import printing.SyntaxHighlighting
import Diagnostic._
import util.{ SourcePosition, NoSourcePosition }
import util.Chars.{ LF, CR, FF, SU }
import scala.annotation.switch

trait MessageRendering:
  import Highlight.*
  import Offsets.*
  import MessageRendering.*

  /** Remove ANSI coloring from `str`, useful for getting real length of strings.
   *  Used by ReplTest, for example.
   *
   *  @return string stripped of ANSI escape codes
   */
  protected final def stripColor(str: String): String = stripper.replaceAllIn(str, "")
  private val stripper = raw"\u001b\[.*?m".r

  /** List of all the inline calls that surround the position */
  def inlinePosStack(pos: SourcePosition): List[SourcePosition] =
    if pos.outer != null && pos.outer.exists then pos :: inlinePosStack(pos.outer)
    else Nil

  /** Split the source lines contained by the pos at the point of the pos,
   *  such that the last line of the first group contains the point.
   */
  private def sourceLines(pos: SourcePosition)(using Context, Level, Offset): (List[String], List[String], Int) =
    assert(pos.exists && pos.source.file.exists)
    var maxLen = Int.MinValue
    def render(offsetAndLine: (Int, String)): String =
      val (offset1, line) = offsetAndLine
      val lineNbr = (pos.source.offsetToLine(offset1) + 1).toString
      val prefix = String.format(s"%${offset - 2}s |", lineNbr)
      maxLen = math.max(maxLen, prefix.length)
      val lnum = hl(" " * math.max(0, maxLen - prefix.length - 1) + prefix)
      lnum + line.stripLineEnd

    def linesFrom(arr: Array[Char]): List[String] =
      def pred(c: Char) = (c: @switch) match
        case LF | CR | FF | SU => true
        case _ => false
      val (line, rest0) = arr.span(!pred(_))
      val (_, rest1) = rest0.span(pred)
      val rest = if rest1.isEmpty then Nil else linesFrom(rest1)
      new String(line) :: rest

    val syntax =
      if ctx.settings.color.value == "never" then pos.linesSlice
      else SyntaxHighlighting.highlight(new String(pos.linesSlice)).toCharArray
    val lines = linesFrom(syntax)
    val (before, after) = pos.beforeAndAfterPoint

    (
      before.zip(lines).map(render),
      after.zip(lines.drop(before.length)).map(render),
      maxLen
    )
  end sourceLines

  /** Generate box containing the report title
   *
   *  ```
   *  -- Error: source.scala ---------------------
   *  ```
   */
  private def boxTitle(title: String)(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val line = "-" * (pageWidth - title.length - 4)
    hl(s"-- $title $line")

  /** The position markers aligned under the error
   *
   *  ```
   *    |         ^^^^^
   *  ```
   */
  private def positionMarker(pos: SourcePosition)(using Context, Level, Offset): String =
    val padding = pos.startColumnPadding
    val carets =
      if pos.startLine == pos.endLine then
        "^" * math.max(1, pos.endColumn - pos.startColumn)
      else "^"
    hl(s"$offsetBox$padding$carets")

  /** The horizontal line with the given offset
   *
   *  ```
   *    |
   *  ```
   */
  private def offsetBox(using Context, Level, Offset): String =
    val prefix = " " * (offset - 1)
    hl(s"$prefix|")

  /** The end of a box section
   *
   *  ```
   *    |---------------
   *  ```
   *  Or if there `soft` is true,
   *  ```
   *    |- - - - - - - -
   *  ```
   */
  private def newBox(soft: Boolean = false)(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val prefix = " " * (offset - 1)
    val lineWidth = (pageWidth - offset)
    val line = if soft then ("- " * ((lineWidth + 1) / 2)).trim else "-" * lineWidth
    hl(s"$prefix|$line")

  /** The end of a box section
   *
   *  ```
   *     ----------------
   *  ```
   */
  private def endBox(using Context, Level, Offset): String =
    val pageWidth = ctx.settings.pageWidth.value
    val prefix = " " * (offset - 1)
    val line = "-" * (pageWidth - offset)
    hl(s"${prefix} $line")

  /** The error message (`msg`) aligned under `pos`. */
  private def errorMsg(pos: SourcePosition, msg: String)(using Context, Level, Offset): String =
    val padding = msg.linesIterator.foldLeft(pos.startColumnPadding) { (pad, line) =>
      val lineLength = stripColor(line).length
      val maxPad = math.max(0, ctx.settings.pageWidth.value - offset - lineLength) - offset

      if (maxPad < pad.length) " " * maxPad
      else pad
    }
    msg.linesIterator
      .map { line => offsetBox + (if line.isEmpty then "" else padding + line) }
      .toLines

  /** The source file path, line and column numbers from the given SourcePosition */
  protected def posFileStr(pos: SourcePosition): String =
    val path = pos.source.file.path
    if pos.exists then s"$path:${pos.line + 1}:${pos.column}" else path

  /** The separator between errors containing the source file and error type
    *
    * @return separator containing error location and kind
    */
  private def posStr(
    pos: SourcePosition,
    message: Message,
    diagnosticString: String
  )(using Context, Level, Offset): String =
    assert(
      message.errorId.isActive,
      """|Attempting to use an ErrorMessageID that is marked as inactive.
         |The ID either needs to be marked as active or you need to use another.""".stripMargin
    )
    if pos.source != NoSourcePosition.source then
      hl {
        val realPos = pos.nonInlined
        val fileAndPos = posFileStr(realPos)
        val errId =
          if message.errorId != ErrorMessageID.NoExplanationID then
            val errorNumber = message.errorId.errorNumber
            s"[E${"0" * (3 - errorNumber.toString.length) + errorNumber}] "
          else ""
        val kind =
          if message.kind == MessageKind.NoKind then diagnosticString
          else s"${message.kind.message} $diagnosticString"
        val title =
          if fileAndPos.isEmpty then s"$errId$kind:" // this happens in dotty.tools.repl.ScriptedTests // TODO add name of source or remove `:` (and update test files)
          else s"$errId$kind: $fileAndPos"
        boxTitle(title)
      }
    else ""
  end posStr

  /** Explanation rendered under "Explanation" header, used externally by sbt. */
  def explanation(m: Message)(using Context): String = renderString {
    sb"""|
         |${Blue("Explanation").show}
         |${Blue("===========").show}"""
    nl"${m.explanation}"
    if !m.explanation.endsWith(EOL) then nl"$EOL"
  }

  private def appendFilterHelp(dia: Diagnostic)(using StringBuilder): Unit =
    import dia.*
    val hasId = msg.errorId.errorNumber >= 0
    val category = dia match {
      case _: UncheckedWarning => "unchecked"
      case _: DeprecationWarning => "deprecation"
      case _: FeatureWarning => "feature"
      case _ => ""
    }
    if hasId || category.nonEmpty then
      nl"Matching filters for @nowarn or -Wconf:"
      if hasId then
        nl"  - id=E${msg.errorId.errorNumber}"
        nl"  - name=${msg.errorId.productPrefix.stripSuffix("ID")}"
      if category.nonEmpty then
        nl"  - cat=$category"

  /** The whole message rendered from `msg`. */
  def messageAndPos(dia: Diagnostic)(using Context): String =
    import dia.*
    val pos1 = pos.nonInlined
    val inlineStack = inlinePosStack(pos).filter(_ != pos1)
    val maxLineNumber =
      if pos.exists then (pos1 :: inlineStack).map(_.endLine).max + 1
      else 0
    given Level = Level(level)
    given Offset = Offset(maxLineNumber.toString.length + 2)
    renderString {
      val posString = posStr(pos, msg, diagnosticLevel(dia))
      if posString.nonEmpty then sb"$posString$EOL"
      if pos.exists then
        val pos1 = pos.nonInlined
        if pos1.exists && pos1.source.file.exists then
          val (srcBefore, srcAfter, _) = sourceLines(pos1)
          val marker = positionMarker(pos1)
          val err = errorMsg(pos1, msg.message)
          sb"${(srcBefore ::: marker :: err :: srcAfter).toLines}"

          if inlineStack.nonEmpty then
            nl"${newBox()}"
            nl"${offsetBox}${i"Inline stack trace"}"
            for inlinedPos <- inlineStack if inlinedPos != pos1 do
              nl"${newBox(soft = true)}"
              nl"${offsetBox}${i"This location contains code that was inlined from $pos"}"
              if inlinedPos.source.file.exists then
                val (srcBefore, srcAfter, _) = sourceLines(inlinedPos)
                val marker = positionMarker(inlinedPos)
                nl"${(srcBefore ::: marker :: srcAfter).toLines}"
            nl"$endBox"
        else sb"${msg.message}"
      else sb"${msg.message}"
      if dia.isVerbose then appendFilterHelp(dia)

      if Diagnostic.shouldExplain(dia) then
        nl"${newBox()}"
        nl"${offsetBox} Explanation (enabled by `-explain`)"
        nl"${newBox(soft = true)}"
        dia.msg.explanation.split(raw"\R").foreach { line =>
          nl"${offsetBox}${if line.isEmpty then "" else " "}${line}"
        }
        nl"$endBox"
      else if dia.msg.canExplain then
        nl"${offsetBox}"
        nl"${offsetBox} longer explanation available when compiling with `-explain`"
    }
  end messageAndPos

  private  def hl(str: String)(using Context, Level): String =
    summon[Level].value match
      case interfaces.Diagnostic.ERROR   => Red(str).show
      case interfaces.Diagnostic.WARNING => Yellow(str).show
      case interfaces.Diagnostic.INFO    => Blue(str).show

  private def diagnosticLevel(dia: Diagnostic): String =
    dia match
      case dia: FeatureWarning => "Feature Warning"
      case dia: DeprecationWarning => "Deprecation Warning"
      case dia: UncheckedWarning => "Unchecked Warning"
      case dia: MigrationWarning => "Migration Warning"
      case _ => dia.level match // Diagnostic isn't sealed (e.g. created in the REPL) so provide a fallback
        case interfaces.Diagnostic.ERROR   => "Error"
        case interfaces.Diagnostic.WARNING => "Warning"
        case interfaces.Diagnostic.INFO    => "Info"
object MessageRendering:
  extension (sc: StringContext)
    def sb(args: Any*)(using sbr: StringBuilder): Unit =
      def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringOps#isLineBreak
      def stripTrailingPart(s: String) =
        val (pre, post) = s.span(c => !isLineBreak(c))
        pre + post.stripMargin
      val stripped: List[String] = sc.parts.toList match
        case head :: tail => head.stripMargin :: tail.map(stripTrailingPart)
        case Nil => Nil
      def loop(i: Int): Unit =
        if i < args.length then
          sbr.append(args(i)).append(stripped(i + 1))
          loop(i + 1)
      sbr.append(stripped(0))
      loop(0)
    def nl(args: Any*)(using sbr: StringBuilder): Unit =
      sbr.append(EOL)
      sb(args*)
  inline def sbRender(using sbr: StringBuilder): String = sbr.result()
  def renderString(body: StringBuilder ?=> Unit): String =
    given StringBuilder = StringBuilder()
    body
    sbRender

  extension (it: Iterator[String]) def toLines = it.mkString(EOL)
  extension (ss: List[String]) def toLines = ss.mkString(EOL)
end MessageRendering

private object Highlight {
  opaque type Level = Int
  extension (level: Level) def value: Int = level
  object Level:
    def apply(level: Int): Level = level
}

/** Size of the left offset added by the box
 *
 *  ```
 *  -- Error: ... ------------
 *  4 |  foo
 *    |  ^^^
 *  ^^^ // size of this offset
 *  ```
 */
private object Offsets {
  opaque type Offset = Int
  def offset(using o: Offset): Int = o
  object Offset:
    def apply(level: Int): Offset = level
}
