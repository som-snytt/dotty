// scalac: -Vprint:all
object X {
  def f[A](as: List[A]) = as headOption // error
  42
}
