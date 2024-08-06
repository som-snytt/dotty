package scala.annotation
package internal

/** An annotation to record a Scala2 pickled alias.
 *  @param aliased  A TermRef pointing to the aliased field.
 */
class SourceFile(@unused path: String) extends Annotation
