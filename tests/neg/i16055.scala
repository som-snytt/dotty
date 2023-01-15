
package a:
  class Foo()
  private object Foo

import a.Foo

@main def run() =
  val f = Foo() // error
  println(f)
