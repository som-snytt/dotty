
// scalac: -Werror

import scala.annotation.nowarn
object Foo {
  private def bar[T](body: => T): T = body

  def foo(): String = bar {
    @nowarn("cat=deprecation")
    val m = implicitly[Manifest[String]]
    m.runtimeClass.getName
  }
}
