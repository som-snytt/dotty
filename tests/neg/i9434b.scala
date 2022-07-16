
object X:
  def f[A](as: List[A]) = as headOption // error
    42

object X2:
  def f[A](as: List[A]) = as headOption // error
  42

object Y:
  def g = 42
  def f = g // error
    (42)

object Z:
  def g = 42
  def f = g
  (42)
