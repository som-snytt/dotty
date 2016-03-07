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
import typer.FrontEnd
import repl.{InteractiveReader, SimpleReader, SplashReader, SplashLoop}

import Flags._
import Names._
import Symbols._
import ast._
import Trees._
import Constants._
import Decorators._
import Reporter.{Error, ConditionalWarning, MigrationWarning, Warning, Info}

import scala.annotation._
import scala.collection.mutable
import scala.reflect.internal.util.ScalaClassLoader
import scala.util.control.NonFatal
import java.io.{BufferedWriter, OutputStreamWriter}

/** The REPL scripter phase is an untyped transform that wraps snippets as required for execution.
 */
class Replater extends Phase {
  override def phaseName = "replater"
  override def run(implicit ctx: Context): Unit = {
    import ReadEvalPrint._
    val unit = ctx.compilationUnit
    val analyzer = new ScriptAnalyzer
    unit.untpdTree = analyzer.replate(unit.untpdTree)
  }
}
/** PrePostTyper that rewrites the print output of REPL with type information.
 */
class Reprinter extends transform.MacroTransform {
  import tpd.Tree
  import ReadEvalPrint._
  override def phaseName = "reprinter"
  private var repl: ReadEvalPrint = _
  val analyzer = new Restringer
  // reanalyze trees and replace toString
  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    for (repl <- unit.untpdTree.getAttachment(ReplAttachment)) {
      Reprinter.this.repl = repl
      unit.tpdTree = analyzer.replate(unit.tpdTree)
      Reprinter.this.repl = unit.tpdTree.attachment(ReplAttachment)
    }
    super.run
  }
  override def newTransformer(implicit ctx: Context): Transformer = new Transformer() {
    def isSnippet(tree: Tree) = ScriptNames.isSnippetPackage(tree.symbol.owner)
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case DefDef(name @ nme.toString_, List(), List(List()), tpt, _) if isSnippet(tree) =>
        Console println s"Noticed def $name"
        // toString for the line result
        def stringer(line: TermName, handlers: List[Handler[_]]): Tree = ctx.typer.typed {
          import untpd._
          //val name = handlers(0).tree.asInstanceOf[ValOrDefDef].name
          //val rhs = Apply(Select(Ident(name), "toString".toTermName), Nil)
          //Literal(Constants.Constant("huh")).withPos(tree.pos)
          val rhs = Literal(Constants.Constant("nope"))
              //handlers.head.print
          DefDef("toString".toTermName, Nil, List(List()), TypeTree(), rhs withPos tree.pos).withMods(Modifiers(Override))
        }
        cpy.DefDef(tree)(name, List(), List(List()), transform(tpt), stringer(repl.line, repl.handlers))
      case _ => super.transform(tree)
    }
  }
}
/** Collect the ReadEvalPrint if compilation completes and invokes its finisher.
 */
class Collector extends Phase {
  override def phaseName = "collector"
  override def run(implicit ctx: Context): Unit = {
    import ReadEvalPrint._
    val unit = ctx.compilationUnit
    unit.tpdTree.getAttachment(ReplAttachment) foreach (_ => Console println s"Has a REPL on tpdTree!")
    if (!ctx.reporter.hasErrors && !unit.tpdTree.isEmpty)
      unit.untpdTree.getAttachment(ReplAttachment) foreach (repl => repl finish repl)
  }
}

// code resides in package line
class ReadEvalPrint(val line: TermName, val handlers: List[Handler[_]])(val finish: ReadEvalPrint => Unit)(implicit ctx: Context) {
  //def defHandlers = handlers collect { case x: MemberDefHandler => x }

  /** list of names used by this expression */
  val referencedNames: List[Name] = handlers flatMap (_.referencedNames)

  /** def and val names */
  def termNames = handlers flatMap (_.termNames)
  def typeNames = handlers flatMap (_.typeNames)

  //def finishing(finish: ReadEvalPrint => Unit): Unit = new ReadEvalPrint(line, handlers)(finish)(ctx)
}
object ReadEvalPrint {
  val ReplAttachment = new Attachment.Key[ReadEvalPrint]

  def apply(line: TermName, handlers: List[Handler[_]])(finish: ReadEvalPrint => Unit)(implicit ctx: Context)
    = new ReadEvalPrint(line, handlers)(finish)(ctx)
}

class ScriptAnalyzer(implicit val ctx: Context) extends ScriptAnalysis with UntypedAnalysis

trait ScriptAnalysis { _: UntypedAnalysis =>
  import untpd._
  import Constants._
  import ReadEvalPrint._
  import Types._

  // tweak the tree and attach a repl
  def replate(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case ModuleDef(nme.SCRIPT_PACKAGE, Template(ctor, parents, self, body)) =>
      val bodily = (body match {
        case lazily: Lazy[_] => lazily.complete
        case other => other
      }).asInstanceOf[List[Tree]]
      val line = scriptName
      val tweaked = wrap(bodily)
      val handlers = tweaked map handlerOf
      val newTemplate = Template(ctor, parents, self, tweaked :+ stringer(line, handlers))
      val replated = ModuleDef(line, newTemplate).withMods(Modifiers(Package))
      val repl = ReadEvalPrint(line, handlers)(r => Main.runScript(r)(ctx))
      replated.pushAttachment(ReplAttachment, repl)
      replated
    case tree => ctx.error(s"scripting unknown tree ${ tree.show }") ; EmptyTree
  }
  // toString for the line result
  def stringer(line: TermName, handlers: List[Handler[Untyped]]): Tree = {
    //val name = handlers(0).tree.asInstanceOf[ValOrDefDef].name
    //val rhs = Apply(Select(Ident(name), "toString".toTermName), Nil)
    val rhs = Literal(Constants.Constant("nope"))
        //handlers.head.print
    DefDef("toString".toTermName, Nil, List(List()), TypeTree(), rhs).withMods(Modifiers(Override))
  }
  def wrap(stmts: List[Tree])(implicit ctx: Context): List[Tree] = {
    val body = captureResultExpr(stmts)
    body
  }
  /** If the last tree is not an assignment, turn it into a valdef. */
  def captureResultExpr(trees: List[Tree])(implicit ctx: Context): List[Tree] = {
    def unrewritten = trees
    // Rewriting    "foo ; bar ; 123"
    // to           "foo ; bar ; val resXX = 123"
    def rewritten(last: Tree) = {
      //import imain.naming._
      //val varName  = TermName(if (synthetic) freshInternalVarName() else freshUserVarName())
      //trees.init :+ q"val $varName = $last"
      val lastval = ValDef(ScriptNames.res.toTermName, TypeTree(), last)
      trees.init :+ lastval
    }
    // If the last tree is a bare expression, rewrite it
    // as a ValDef instead, so we can access the value.
    val last = trees.lastOption getOrElse EmptyTree
    last match {
      case _: Assign => unrewritten            // we don't want to include assignments
      case _: TermTree | _: Ident | _: Select  // ... but do want other unnamed terms.
                    => rewritten(last)
      case _        => unrewritten
    }
  }
  def scriptName: TermName = ScriptNames.line.toTermName
}

/** Fix the printed output with type info. */
class Restringer extends TypedAnalysis {

  import tpd._
  import ReadEvalPrint._

  def replate(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case PackageDef(pid, stmts) =>
      Console println s"OK, pkg $pid, ${pid.getClass}"
      stmts collectFirst {
        case TypeDef(name, Template(ctor, parents, self, bodily)) =>
          val body = (bodily match {
            case lazily: Lazy[_] => lazily.complete
            case other => other
          }).asInstanceOf[List[Tree]].init
          Console println s"OK, template $name with body $body"
          val handlers = body.init map handlerOf
          val repl = ReadEvalPrint("".toTermName, handlers)(r => Main.runScript(r)(ctx))
          tree.pushAttachment(ReplAttachment, repl)
          ()
      }
      tree
    /*
    case ModuleDef(line, Template(ctor, parents, self, body)) =>
      val bodily = (body match {
        case lazily: Lazy[_] => lazily.complete
        case other => other
      }).asInstanceOf[List[Tree]].init
      val handlers = bodily map handlerOf
      val newTemplate = Template(ctor, parents, self, tweaked :+ stringer(line, handlers))
      val replated = ModuleDef(line, newTemplate).withMods(Modifiers(Package))
      val repl = ReadEvalPrint(line, handlers)(r => Main.runScript(r)(ctx))
      replated.pushAttachment(ReplAttachment, repl)
      replated
    */
    //case tree => ctx.error(s"scripting unknown tree ${ tree.show }") ; EmptyTree
    case tree => Console println s"Nope: ${tree.getClass}: ${tree.show}" ; tree
  }
  /*
  // toString for the line result
  def stringer(line: TermName, handlers: List[Handler[Type]])(implicit ctx: Context): Tree = ctx.typer.typed {
    import untpd._
    //val name = handlers(0).tree.asInstanceOf[ValOrDefDef].name
    //val rhs = Apply(Select(Ident(name), "toString".toTermName), Nil)
    val rhs = untpd.Literal(Constants.Constant("nope"))
        //handlers.head.print
    untpd.DefDef("toString".toTermName, Nil, List(List()), TypeTree(), rhs).withMods(Modifiers(Override))
  }
  */
}

/** Handlers for untyped trees. */
trait UntypedAnalysis {
  import untpd._

  //implicit def ctx: Context

  def handlerOf(tree: Tree): Handler[Untyped] = tree match {
    case valdef: ValDef => ValHandler(valdef)
    case _ => TreeHandler(tree)
  }

  abstract class BaseHandler(tree: Tree) extends Handler[Untyped] with UntypedStringly {
    def referencedNames(implicit ctx: Context) = {
      val usedNames: List[Name] = {
        val ivt = new UntypedTreeAccumulator[mutable.Set[Name]] {
          override def apply(ns: mutable.Set[Name], tree: Tree)(implicit ctx: Context) =
            tree match {
              case Ident(name) => ns += name
              case _ => foldOver(ns, tree)
            }
        }
        ivt.foldOver(mutable.HashSet(), tree).toList
      }
      usedNames
    }
    def termNames(implicit ctx: Context) = Nil
    def typeNames(implicit ctx: Context) = Nil

    def print(implicit ctx: Context) = ctx.typer.typed(printed)
    def printed(implicit ctx: Context): untpd.Tree
  }
  sealed abstract class MemberHandler(val tree: Tree) extends BaseHandler(tree) {
  }
  abstract class MemberDefHandler(val member: MemberDef) extends MemberHandler(member) {
    def name: Name = member.name
  }
  case class ValHandler(tree: ValDef) extends BaseHandler(tree) {
    def printed(implicit ctx: Context) =
      joining(text(tree.name.toString + " = "), stringOf(Ident(tree.name)))
  }
  case class TreeHandler(tree: Tree) extends BaseHandler(tree) {
    def printed(implicit ctx: Context) = Literal(Constant("any tree"))
  }
}
/** Handlers for typed trees. */
trait TypedAnalysis {
  import tpd._
  import Types.Type

  def handlerOf(tree: Tree): Handler[Type] = tree match {
    case valdef: ValDef => ValHandler(valdef)
    case _ => TreeHandler(tree)
  }

  abstract class BaseHandler(tree: Tree) extends Handler[Type] with TypedStringly {
    def referencedNames(implicit ctx: Context) = Nil
    def termNames(implicit ctx: Context) = Nil
    def typeNames(implicit ctx: Context) = Nil

    def print(implicit ctx: Context) = ctx.typer.typed(printed)
    def printed(implicit ctx: Context): untpd.Tree
  }
  sealed abstract class MemberHandler(val tree: Tree) extends BaseHandler(tree) {
  }
  abstract class MemberDefHandler(val member: MemberDef) extends MemberHandler(member) {
    def name: Name = member.name
  }
  case class ValHandler(tree: ValDef) extends BaseHandler(tree) {
    def printed(implicit ctx: Context) =
      joining(text(s"{tree.name.toString}: SOMETHING = "), stringOf(untpd.Ident(tree.name)))
  }
  case class TreeHandler(tree: Tree) extends BaseHandler(tree) {
    def printed(implicit ctx: Context) = Literal(Constant("any tree"))
  }
}
/** A handler for one tree corresponding to user input.
 *  It reports what names are defined, and what names must
 *  be in scope for compilation.
 *
 *  The print method supplies a tree suitable as the RHS of
 *  a `toString` method. The result string is a user-friendly
 *  representation of the tree, as output by the REPL, and
 *  generally includes a name, type and value.
 *
 *  If the underlying tree is untyped, then the string
 *  representation may omit type information or attempt to
 *  report the runtime class of the result value.
 */
trait Handler[T >: Untyped] {
  /** what was analyzed */
  //def tree: Tree[T]

  /** list of names used by this expression */
  def referencedNames(implicit ctx: Context): List[Name]

  /** def and val names */
  def termNames(implicit ctx: Context): List[Name]
  def typeNames(implicit ctx: Context): List[Name]

  // => String, what the REPL should print
  def print(implicit ctx: Context): Tree[T]
}
trait TypedStringly {
  import untpd._

  private val uts = new UntypedStringly {}

  def text(s: String)(implicit ctx: Context) = ctx.typer.typed(uts.text(s))
  def stringOf(tree: Tree)(implicit ctx: Context) = ctx.typer.typed(uts.stringOf(tree))
  def toStringOf(tree: Tree)(implicit ctx: Context) = ctx.typer.typed(uts.toStringOf(tree))
  def joining(trees: Tree*)(implicit ctx: Context) = ctx.typer.typed(uts.joining(trees: _*))
}
trait UntypedStringly {
  import untpd._

  def text(s: String) = Literal(Constant(s))
  def stringOf(tree: Tree): Tree = {
    Apply(Select(Select(Select(Ident("scala".toTermName), "runtime".toTermName), "ScalaRunTime".toTermName),
      "replStringOf".toTermName), List(tree, Literal(Constant(1000))))
  }
  def toStringOf(tree: Tree): Tree = {
    Apply(Select(tree, "toString".toTermName), List())
  }
  def joining(trees: Tree*): Tree = {
    val sb: Tree = Apply(Select(New(Ident(typeName("StringBuilder"))), nme.CONSTRUCTOR), List())
    def append(a: Tree, b: Tree) = Apply(Select(a, termName("append")), List(b))
    toStringOf(trees.foldLeft(sb)((acc, t) => append(acc, t)))
  }
}
/*
trait Stringly[T >: Untyped] {
  val treeInstance: Trees.Instance[T]
  import treeInstance._

  def text(s: String) = Literal(Constant(s))
  def stringOf(tree: Tree): treeInstance.Tree = {
    Apply(Select(Select(Select(Ident("scala".toTermName), "runtime".toTermName), "ScalaRunTime".toTermName),
      "replStringOf".toTermName), List(tree, Literal(Constant(1000))))
  }
  def toStringOf(tree: Tree): Tree = {
    Apply(Select(tree, "toString".toTermName), List())
  }
  def joining(trees: Tree*): Tree = {
    val sb: Tree = Apply(Select(New(Ident(typeName("StringBuilder"))), nme.CONSTRUCTOR), List())
    def append(a: Tree, b: Tree): treeInstance.Tree = Apply(Select(a, termName("append")), List(b))
    toStringOf(trees.foldLeft(sb)((acc, t) => append(acc, t)))
  }
}
*/

object ScriptNames {
  import java.util.concurrent.atomic.AtomicInteger
  private val lineCounter = new AtomicInteger
  private val resCounter  = new AtomicInteger
  def line = s"line${lineCounter.incrementAndGet}"
  def res  = s"res${lineCounter.incrementAndGet}"
  def reset(): Unit = { lineCounter.set(0) ; resCounter.set(0) }

  val liner = raw"line\d+".r
  def isSnippetPackage(sym: Symbol)(implicit ctx: Context) = sym.isPackageObject && (sym.owner.showName match {
    case liner() => true ; case _ => false
  })
  //def isSnippetPackage(sym: Symbol)(implicit ctx: Context) = sym.isPackageObject && (sym.owner.showName startsWith "line")
}





  /*
  def cleanTypeAfterTyper(sym: => Symbol): Type = {
    exitingTyper(
      dealiasNonPublic(
        dropNullaryMethod(
          sym.tpe_*
        )
      )
    )
  }
        val resObjSym =
          (accessPath.split("\\.")).foldLeft(outerResObjSym) { (sym,str) =>
            if (str.isEmpty) sym
            else
              ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
                sym.info.member(str.toTermName).symbol
              }
          }

        names.foldLeft(Map.empty[Name,String]) { (map, name) =>
          val rawType =
            ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
              resObjSym.info.member(name).info
            }

          // the types are all =>T; remove the =>
          val cleanedType = rawType.widenExpr

          map + (name ->
            ctx.atPhase(ctx.typerPhase.next) { implicit ctx =>
              cleanedType.show
            })

class Reprinter extends transform.MacroTransform with TypedAnalysis {
  import tpd.Tree
  import ReadEvalPrint._
  override def phaseName = "reprinter"
  private var repl: ReadEvalPrint = _
  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    for (repl <- unit.untpdTree.getAttachment(ReplAttachment)) {
      Reprinter.this.repl = repl
      super.run(ctx)
      unit.tpdTree.pushAttachment(ReplAttachment, repl)
    }
  }
  override def newTransformer(implicit ctx: Context): Transformer = new Transformer() {
    def isSnippet(tree: Tree) = ScriptNames.isSnippetPackage(tree.symbol.owner)
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case DefDef(name @ nme.toString_, List(), List(List()), tpt, _) if isSnippet(tree) =>
        Console println s"Noticed def $name"
        def printable = ctx.typer.typed {
          import untpd._
          Literal(Constants.Constant("huh")).withPos(tree.pos)
        }
        cpy.DefDef(tree)(name, List(), List(List()), transform(tpt), printable)
      case _ => super.transform(tree)
    }
  }
}

  */
