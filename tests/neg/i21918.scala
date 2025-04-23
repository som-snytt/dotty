
type AliasedAnyVal = AnyVal
class C(c: Int) extends AliasedAnyVal // error

class D(d: Int) extends AliasedAnyVal // no error
object D

class E(d: Int) extends AnyVal
