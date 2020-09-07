package dotty.tools.dotc
package config

import Settings._
import core.Contexts._
import Properties._

import scala.PartialFunction.cond
import scala.jdk.CollectionConverters._
import scala.util.chaining._

import java.nio.file.{Files, Paths}

object CompilerCommand {

  /** The name of the command */
  def cmdName: String = "scalac"

  private def explainAdvanced = """
    |-- Notes on option parsing --
    |Boolean settings are always false unless set.
    |Where multiple values are accepted, they should be comma-separated.
    |  example: -Xplugin:plugin1,plugin2
    |<phases> means one or a comma-separated list of:
    |  - (partial) phase names with an optional "+" suffix to include the next phase
    |  - the string "all"
    |  example: -Xprint:all prints all phases.
    |  example: -Xprint:typer,mixin prints the typer and mixin phases.
    |  example: -Ylog:erasure+ logs the erasure phase and the phase after the erasure phase.
    |           This is useful because during the tree transform of phase X, we often
    |           already are in phase X + 1.
  """

  def shortUsage: String = s"Usage: $cmdName <options> <source files>"

  def versionMsg: String = s"Scala compiler $versionString -- $copyrightString"

  /** Distill arguments into summary detailing settings, errors and files to compiler */
  def distill(args: Array[String])(using Context): ArgsSummary = {
    /**
     * Expands all arguments starting with @ to the contents of the
     * file named like each argument.
     */
    def expandArg(arg: String): List[String] = {
      def stripComment(s: String) = s takeWhile (_ != '#')
      val path = Paths.get(arg stripPrefix "@")
      if (!Files.exists(path))
        throw new java.io.FileNotFoundException("argument file %s could not be found" format path.getFileName)

      val lines = Files.readAllLines(path) // default to UTF-8 encoding

      val params = lines.asScala map stripComment mkString " "
      CommandLineParser.tokenize(params)
    }

    // expand out @filename to the contents of that filename
    def expandedArguments = args.toList flatMap {
      case x if x startsWith "@"  => expandArg(x)
      case x                      => List(x)
    }

    ctx.settings.processArguments(expandedArguments, processAll = true)
  }

  /** Provide usage feedback on argument summary, assuming that all settings
   *  are already applied in context.
   *  @return  The list of files to compile.
   */
  def checkUsage(summary: ArgsSummary, sourcesRequired: Boolean)(using Context): List[String] = {
    val settings = ctx.settings
    extension [T](setting: Setting[T]):
      def isStandard: Boolean = !isVerbose && !isWarning && !isAdvanced && !isPrivate || setting.name == "-Werror" || setting.name == "-Wconf"
      def isVerbose: Boolean  = setting.name.startsWith("-V") && setting.name != "-V"
      def isWarning: Boolean  = setting.name.startsWith("-W") && setting.name != "-W" || setting.name == "-Xlint"
      def isAdvanced: Boolean = setting.name.startsWith("-X") && setting.name != "-X"
      def isPrivate: Boolean  = setting.name.startsWith("-Y") && setting.name != "-Y"
      def shortHelp: String   = setting.description.linesIterator.next()
      def isHelping(using Context): Boolean =
        cond(setting.value) {
          case ss: List[?] if setting.isMultivalue => ss.contains("help")
          case s: String                           => "help" == s
        }

    /** Creates a help message for a subset of options based on cond */
    def availableOptionsMsg(cond: Setting[?] => Boolean): String = {
      val ss                  = settings.allSettings.filter(cond).toList.sortBy(_.name)
      val width               = ss.map(_.name.length).max
      def format(s: String)   = s"%-${width}s".format(s)
      def helpStr(s: Setting[?]) = {
        def defaultValue = s.default match {
          case _: Int | _: String => s.default.toString
          case _ =>
            // For now, skip the default values that do not make sense for the end user.
            // For example 'false' for the version command.
            ""
        }
        def formatSetting(name: String, value: String) =
          if (value.nonEmpty)
          // the format here is helping to make empty padding and put the additional information exactly under the description.
            s"\n${format("")} $name: $value."
          else
            ""
        s"${format(s.name)} ${s.shortHelp}${formatSetting("Default", defaultValue)}${formatSetting("Choices", s.legalChoices)}"
      }
      ss.map(helpStr).mkString("\n")
    }

    def createUsageMsg(label: String, explain: Boolean = true)(cond: Setting[?] => Boolean): String = {
      val prefix = List(
        Some(shortUsage),
        Some(explainAdvanced).filter(_ => explain),
        Some(label + " options include:")
      ).flatten mkString "\n"

      prefix + "\n" + availableOptionsMsg(cond)
    }

    /** Messages explaining usage and options. */
    def usageMessage  = createUsageMsg("Standard", explain = false)(_.isStandard)
    def vusageMessage = createUsageMsg("Verbose")(_.isVerbose)
    def wusageMessage = createUsageMsg("Warnings")(_.isWarning)
    def xusageMessage = createUsageMsg("Available advanced")(_.isAdvanced)
    def yusageMessage = createUsageMsg("Available private")(_.isPrivate)

    def shouldStopWithInfo = {
      import settings._
      val infoSettings = List(help, Vhelp, Whelp, Xhelp, Yhelp, showPlugins, XshowPhases)
      infoSettings.exists(_.value) || allSettings.exists(_.isHelping)
    }

    def phasesMessage: String = {
      (new Compiler()).phases.map {
        case List(single) => single.phaseName
        case more => more.map(_.phaseName).mkString("{", ", ", "}")
      }.mkString("\n")
    }

    def infoMessage: String = {
      import settings._
      if (help.value) usageMessage
      else if (Vhelp.value) vusageMessage
      else if (Whelp.value) wusageMessage
      else if (Xhelp.value) xusageMessage
      else if (Yhelp.value) yusageMessage
      else if (showPlugins.value) ctx.base.pluginDescriptions
      else if (XshowPhases.value) phasesMessage
      else ""
    }

    // Print all warnings encountered during arguments parsing
    summary.warnings.foreach(report.warning(_))

    if (summary.errors.nonEmpty) {
      summary.errors foreach (report.error(_))
      report.echo("  scalac -help  gives more information")
      Nil
    }
    else if (settings.version.value) {
      report.echo(versionMsg)
      Nil
    }
    else if (shouldStopWithInfo) {
      report.echo(infoMessage)
      Nil
    }
    else
      summary.arguments tap (args => if sourcesRequired && args.isEmpty then report.echo(usageMessage))
  }
}
