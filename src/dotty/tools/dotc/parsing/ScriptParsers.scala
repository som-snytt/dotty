package dotty.tools
package dotc
package parsing

import util.SourceFile
import core._
import Contexts._
import Decorators._
import Parsers._
import Tokens._
import Names._
import StdNames._
import Phases._
import ast.Trees._
import Flags._
import Names._

object ScriptParsers {

  import ast.untpd._

  class ScriptParser(source: SourceFile)(implicit ctx: Context) extends Parser(source) {

    /** This is the parse entry point for code which is not self-contained, i.e.,
     *  a script which is a series of template statements.
     *
     *  The statements are wrapped in a special package object named `<script>`.
     *  The scripter phase will normally turn this tree into something more useful.
     */
    override def parse(): Tree = {
      val (_, stmts) = templateStatSeq()
      accept(EOF)
      makeScriptPackage(0, stmts)
    }

    // wrap in package object, so it could compile without further ado. (Except <script> is unfriendly package name.)
    private def makeScriptPackage(start: Int, stats: List[Tree]): Tree =
      makePackageObject(nme.SCRIPT_PACKAGE, start, stats)

    // package module doesn't need to be wrapped in package: `package object script extends App { ...$stmts }`
    private def makePackageObject(name: TermName, start: Int, stats: List[Tree]): Tree = {
      val ctor = atPos(start, start, start) { ast.untpd.emptyConstructor }
      val template = Template(ctor, List(Ident("App".toTypeName)), EmptyValDef, stats).withPos(ctor.pos.toSynthetic)
      ModuleDef(name, template).withMods(Modifiers(Package)).withPos(ctor.pos.toSynthetic)
    }
  }

  /** The default scripter phase is an untyped transform that wraps snippets as required for execution.
   */
  class Scripter extends Phase {
    override def phaseName = "scripter"
    override def run(implicit ctx: Context): Unit = {
      val unit = ctx.compilationUnit
      unit.untpdTree = unit.untpdTree match {
        case tree @ ModuleDef(nme.SCRIPT_PACKAGE, template) => ModuleDef(scriptName, template).withMods(Modifiers(Package))
        //case tree => ctx.error(s"scripting unknown tree ${ tree.show }") ; EmptyTree
        case tree => ctx.echo(tree.show) ; tree
        //case tree => tree
      }
    }

    // TODO: settable or inferrable
    def scriptName: TermName = "script".toTermName

    /* TODO: reinstantiate
    /** This is the parse entry point for code which is not self-contained, e.g.
     *  a script which is a series of template statements.  They will be
     *  swaddled in Trees until the AST is equivalent to the one returned
     *  by compilationUnit().
     */
    override def parse(): Tree = unsupported("parse")
      val stmts = templateStatSeq(false)._2
      accept(EOF)

      def mainModuleName = ctx.settings.script.value

      /** If there is only a single object template in the file and it has a
       *  suitable main method, we will use it rather than building another object
       *  around it.  Since objects are loaded lazily the whole script would have
       *  been a no-op, so we're not taking much liberty.
       */
      def searchForMain(): Option[Tree] = {
        /** Have to be fairly liberal about what constitutes a main method since
         *  nothing has been typed yet - for instance we can't assume the parameter
         *  type will look exactly like "Array[String]" as it could have been renamed
         *  via import, etc.
         */
        def isMainMethod(t: Tree) = t match {
          case DefDef(_, nme.main, Nil, List(_), _, _)  => true
          case _                                        => false
        }
        /** For now we require there only be one top level object. */
        var seenModule = false
        val newStmts = stmts collect {
          case t @ Import(_, _) => t
          case md @ ModuleDef(mods, name, template)
            if !seenModule && (template.body exists isMainMethod) =>
            seenModule = true
            /** This slightly hacky situation arises because we have no way to communicate
             *  back to the scriptrunner what the name of the program is.  Even if we were
             *  willing to take the sketchy route of settings.script.value = progName, that
             *  does not work when using fsc.  And to find out in advance would impose a
             *  whole additional parse.  So instead, if the actual object's name differs from
             *  what the script is expecting, we transform it to match.
             */
            md.derivedModuleDef(mods, mainModuleName.toTermName, template)
          case _ =>
            /** If we see anything but the above, fail. */
            return None
        }
        Some(makePackaging(0, emptyPkg, newStmts))
      }

      if (mainModuleName == ScriptRunner.defaultScriptMain)
        searchForMain() foreach { return _ }

      /** Here we are building an AST representing the following source fiction,
       *  where <moduleName> is from -Xscript (defaults to "Main") and <stmts> are
       *  the result of parsing the script file.
       *
       *  object <moduleName> {
       *    def main(argv: Array[String]): Unit = {
       *      val args = argv
       *      new AnyRef {
       *        <stmts>
       *      }
       *    }
       *  }
       */
      import definitions._

      def emptyPkg    = atPos(0, 0, 0) { Ident(nme.EMPTY_PACKAGE_NAME) }
      def emptyInit   = DefDef(
        Modifiers(),
        nme.CONSTRUCTOR,
        Nil,
        List(Nil),
        TypeTree(),
        Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)), Literal(Constant(())))
      )

      // def main
      def mainParamType = AppliedTypeTree(Ident(tpnme.Array), List(Ident(tpnme.String)))
      def mainParameter = List(ValDef(Modifiers(Param), "argv", mainParamType, EmptyTree))
      def mainSetArgv   = List(ValDef(Modifiers(), "args", TypeTree(), Ident("argv")))
      def mainNew       = makeNew(Nil, emptyValDef, stmts, List(Nil), NoPosition, NoPosition)
      def mainDef       = DefDef(Modifiers(), nme.main, Nil, List(mainParameter), scalaDot(tpnme.Unit), Block(mainSetArgv, mainNew))

      // object Main
      def moduleName  = ScriptRunner scriptMain settings
      def moduleBody  = Template(List(scalaScalaObjectConstr), emptyValDef, List(emptyInit, mainDef))
      def moduleDef   = ModuleDef(Modifiers(), moduleName, moduleBody)

      // package <empty> { ... }
      makePackaging(0, emptyPkg, List(moduleDef))
    }*/
  }
}
