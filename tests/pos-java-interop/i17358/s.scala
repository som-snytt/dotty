
class Nullifier extends J:
  override def j = null  // inferred Null

class Denullifier extends Nullifier:
  override def j = "was error"

class X {
  def f: String = "x"
  //override def toString = "x"
}

class Y extends X {
  override def f = null
  override def toString = null
}

class Z extends Y {
  override def f = "z"
  override def toString = "z"
}

object A extends App {
  println {
    new Z()
  }
}
