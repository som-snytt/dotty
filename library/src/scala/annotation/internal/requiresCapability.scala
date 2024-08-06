package scala.annotation
package internal

/** An annotation to record a required capaility in the type of a throws
 */
class requiresCapability(@unused capability: Any) extends StaticAnnotation

