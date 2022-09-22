
case class C(c: Int) {
  def +(i: Int, j: Int): C = new C(c + i*j)
}

object Test extends App {
  println {
    var x = new C(42)
    x = x + (1, 9)
    x
  }
  println {
    var x = new C(42)
    x.+=(2, 9)
    x
  }
  println {
    var x = new C(42)
    x += (3, 9)  // was: value += is not a member of C - did you mean C.!=?
    x
  }
}
