
type AliasedAnyVal = AnyVal
class C(c: Int) extends AliasedAnyVal
object C

@main def Test =
  val x, y = C(42)
  assert(x == y)
