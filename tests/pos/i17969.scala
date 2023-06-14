
// scalac: -deprecation -Werror

import scala.annotation.nowarn
object Foo {
  private def bar[T](body: => T): T = body

  @deprecated("unwise", since="0.5")
  def f = 42

  def foo(): String = bar {
    @nowarn("cat=deprecation")
    val m = implicitly[Manifest[String]]
    m.runtimeClass.getName
  }

  def g = f
}
