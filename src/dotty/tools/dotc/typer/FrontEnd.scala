package dotty.tools.dotc
package typer

import core._
import Phases._
import Contexts._
import Symbols._
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import parsing.Parsers.parser
import parsing.ScriptParsers.Scripter
import config.Printers._
import util.Stats._
import scala.util.control.NonFatal
import ast.Trees._
import util.FreshNameCreator

class FrontEnd extends Phase {

  override def phaseName = "frontend"
  override def isTyper = true
  import ast.tpd

  def monitor(doing: String)(body: => Unit)(implicit ctx: Context) =
    try body
    catch {
      case NonFatal(ex) =>
        ctx.echo(s"exception occurred while $doing ${ctx.compilationUnit}")
        throw ex
    }

  def parse(implicit ctx: Context) = monitor("parsing") {
    val unit = ctx.compilationUnit
    unit.untpdTree =
      if (unit.isJava) new JavaParser(unit.source).parse()
      else parser(unit.source).parse()
    val printer = if (ctx.settings.Xprint.value.contains("parser")) default else typr
    printer.println("parsed:\n" + unit.untpdTree.show)
  }

  def postparse(implicit ctx: Context) = monitor("scripting") {
    val unit = ctx.compilationUnit
    postparser.run
    val printer = if (ctx.settings.Xprint.value.contains("scripter")) default else typr
    printer.println("scripted:\n" + unit.untpdTree.show)
  }

  def enterSyms(implicit ctx: Context) = monitor("indexing") {
    val unit = ctx.compilationUnit
    ctx.typer.index(unit.untpdTree)
    typr.println("entered: " + unit.source)
  }

  def typeCheck(implicit ctx: Context) = monitor("typechecking") {
    val unit = ctx.compilationUnit
    unit.tpdTree = ctx.typer.typedExpr(unit.untpdTree)
    typr.println("typed: " + unit.source)
    record("retainedUntypedTrees", unit.untpdTree.treeSize)
    record("retainedTypedTrees", unit.tpdTree.treeSize)
  }

  private def firstTopLevelDef(trees: List[tpd.Tree])(implicit ctx: Context): Symbol = trees match {
    case PackageDef(_, defs) :: _    => firstTopLevelDef(defs)
    case Import(_, _) :: defs        => firstTopLevelDef(defs)
    case (tree @ TypeDef(_, _)) :: _ => tree.symbol
    case _ => NoSymbol
  }

  private def discardAfterTyper(unit: CompilationUnit)(implicit ctx: Context) =
    unit.isJava || firstTopLevelDef(unit.tpdTree :: Nil).isPrimitiveValueClass

  // post-parser hook for scripting and other untyped transforms, in lieu of plugin facility
  protected def postparser: Phase = new Scripter()

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val unitContexts = for (unit <- units) yield
      ctx.fresh.setCompilationUnit(unit).setFreshNames(new FreshNameCreator.Default)
    unitContexts foreach (parse(_))
    record("parsedTrees", ast.Trees.ntrees)
    unitContexts foreach (postparse(_))
    unitContexts foreach (enterSyms(_))
    unitContexts foreach (typeCheck(_))
    record("totalTrees", ast.Trees.ntrees)
    unitContexts.map(_.compilationUnit).filterNot(discardAfterTyper)
  }

  override def run(implicit ctx: Context): Unit = {
    parse
    enterSyms
    typeCheck
  }
}
