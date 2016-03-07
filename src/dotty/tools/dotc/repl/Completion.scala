/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package dotty.tools.dotc
package repl

import Completion._
 
/** A component that supplies a `Completer`.
 */
trait Completion {
  def resetVerbosity(): Unit
  def complete(buffer: String, cursor: Int): Candidates
} 

/** A component that supplies a `Completer` that yields no results.
 */
object NoCompletion extends Completion {
  /** Empty completion results. */
  private val NoCandidates = Candidates(-1, Nil)
  def resetVerbosity() = ()
  def complete(buffer: String, cursor: Int): Candidates = NoCandidates
}  
  
object Completion { 
  /** Completion results. */
  case class Candidates(cursor: Int, candidates: List[String])
/*
  def looksLikeInvocation(code: String) = (
        (code != null)
    &&  (code startsWith ".")
    && !(code == ".")
    && !(code startsWith "./")
    && !(code startsWith "..")
  ) 
  object Forwarder {
    def apply(forwardTo: () => Option[CompletionAware]): CompletionAware = new CompletionAware {
      def completions(verbosity: Int) = forwardTo() map (_ completions verbosity) getOrElse Nil
      override def follow(s: String) = forwardTo() flatMap (_ follow s)
    } 
  } 
*/
} 
