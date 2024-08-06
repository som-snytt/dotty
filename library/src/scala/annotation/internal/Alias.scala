package scala.annotation
package internal

/** An annotation to record a Scala2 pickled alias.
 *  @param aliased  A TermRef pointing to the aliased field.
 *  TODO: Drop once the new param alias scheme is in the bootstrap compiler
 */
class Alias(@unused aliased: Any) extends Annotation
