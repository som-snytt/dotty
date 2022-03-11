
object NewSyntax {
  def test1(x: Int, y: Int) = if x == y          then "equal" else "different"
  def test2(x: Int, y: Int) = if (x == y)        then "equal" else "different"
  def test3(x: Int, y: Int) = if ((x - y) == 0)  then "equal" else "different"
  def test4(x: Int, y: Int) = if (x - y) == 0    then "equal" else "different"
}

println(NewSyntax.test4(1,2))
