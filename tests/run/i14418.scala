
case class C(c: Int):
  def +(i: Int, j: Int): C = new C(c + i*j)

@main def test() =
  locally {
    var x = C(42)
    x = x + (3, 9)
    assert(x == C(69))
  }
  locally {
    var x = C(42)
    x.+=(3, 9)
    assert(x == C(69))
  }
  locally {
    var x = new C(42)
    x += (3, 9)
    assert(x == C(69))
  }
}
