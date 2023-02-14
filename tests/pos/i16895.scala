
type Authorization = Unit

class Foo():
  def apply(i: Int)(using Authorization): Int = i

  def update(i: Int, f: Authorization ?=> Int): Unit = println(s"The new value of #$i is now ${f(using ())}")


@main def test() =
  val foo = new Foo()
  foo(42) = foo(42) + 1 //Ouputs: "The new value of #42 is now 43"
  foo(42) += 1
