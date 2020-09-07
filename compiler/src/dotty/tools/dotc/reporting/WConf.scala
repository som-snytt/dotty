package dotty.tools
package dotc
package reporting

import core.Contexts._

import scala.collection.mutable.ListBuffer

sealed trait MessageFilter:
  def matches(message: Diagnostic): Boolean

object MessageFilter:
  object Any extends MessageFilter:
    def matches(message: Diagnostic): Boolean = true
  object Deprecated extends MessageFilter:
    def matches(message: Diagnostic): Boolean = message.isInstanceOf[Diagnostic.DeprecationWarning]
  object Feature extends MessageFilter:
    def matches(message: Diagnostic): Boolean = message.isInstanceOf[Diagnostic.FeatureWarning]
  object Empty extends MessageFilter:
    def matches(message: Diagnostic): Boolean = false

enum Action:
  case Error, Warning, WarningSummary, WarningVerbose, Info, InfoSummary, InfoVerbose, Silent

final case class WConf(confs: List[(List[MessageFilter], Action)]):
  def action(message: Diagnostic): Action = confs.collectFirst {
    case (filters, action) if filters.forall(_.matches(message)) => action
  }.getOrElse(Action.Warning)

// "cat=deprecation:ws", "cat=feature:ws", "cat=optimizer:ws"
object WConf:
  private type Conf = (List[MessageFilter], Action)
  object ActionOf:
    import Action._
    def unapply(x: String): Option[Action] =
      x match {
        case "error" | "e"            => Some(Error)
        case "warning" | "w"          => Some(Warning)
        case "warning-summary" | "ws" => Some(WarningSummary)
        case "warning-verbose" | "wv" => Some(WarningVerbose)
        case "info" | "i"             => Some(Info)
        case "info-summary" | "is"    => Some(InfoSummary)
        case "info-verbose" | "iv"    => Some(InfoVerbose)
        case "silent" | "s"           => Some(Silent)
        case _                        => None
      }
  object FilterOf:
    import MessageFilter._
    val splitter = raw"([^=]+)=(.+)".r
    def unapply(x: String): Option[MessageFilter] =
      x match {
        case "any"                  => Some(Any)
        case splitter(filter, conf) =>
          filter match {
            case "cat" =>
              conf match {
                case "deprecation"  => Some(Deprecated)
                case "feature"      => Some(Feature)
                case "optimizer"    => Some(Empty)          // support as empty category
              }
            case "msg" => ???
          }
        case _                      => None
      }
  def parsed(using Context): WConf = fromSettings(ctx.settings.Wconf.value)
  def fromSettings(settings: List[String]): WConf =
    val conf = ListBuffer.empty[Conf]
    def loop(more: List[String]): Unit =
      more match {
        case s :: rest =>
          val parts   = s.split(':').toList
          var filters = ListBuffer.empty[MessageFilter]
          var action  = null.asInstanceOf[Action]
          def eachPart(moreParts: List[String]): Unit =
            moreParts match {
              case ActionOf(a) :: ps => if (action ne null) then ??? else action = a ; eachPart(ps)
              case FilterOf(f) :: ps => filters += f ; eachPart(ps)
              case _ =>
            }
          eachPart(parts)
          if action eq null then action = Action.Silent
          if filters.isEmpty then filters += MessageFilter.Any
          conf.addOne((filters.toList, action))
          loop(rest)
        case _ =>
      }
    loop(settings)
    WConf(conf.toList)
  end fromSettings
end WConf
