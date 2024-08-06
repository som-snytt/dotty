package scala.annotation

/** An annotation that indicates capture of an enclosing by-name type
 */
@experimental
class retainsByName(@unused xs: (Any@retainsArg)*) extends StaticAnnotation

