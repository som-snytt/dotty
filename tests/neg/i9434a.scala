
object Foo {
  def bar1: (Int, Int) = {
    val foo = 1.0
    val bar = foo      // error
      (1, 1)           // scastie error: value foo does not take parameters
  }                    // error: Found: Unit Required: (Int, Int)

  def bar2: Vector[Int] = {
    val foo =
      Vector(1) ++     // error
    Vector(2) ++       // scastie error: postfix operator `++` needs to be enabled
      Vector(3)
    foo
  }                    // scastie error: Found: (foo : IterableOnce[Int] => Vector[Int]) Required: Vector[Int]
}
/*
was:
-- [E018] Syntax Error: i9434.scala:11:18 ------------------------------------------------------------------------------
11 |      Vector(1) ++
   |                  ^
   |                  expression expected but unindent found
   |
   | longer explanation available when compiling with `-explain`
-- [E050] Type Error: i9434.scala:5:14 ---------------------------------------------------------------------------------
5 |    val bar = foo
  |              ^^^
  |              value foo does not take parameters
  |
  | longer explanation available when compiling with `-explain`
-- [E007] Type Mismatch Error: i9434.scala:7:3 -------------------------------------------------------------------------
7 |  }                    // scastie error: Found: Unit Required: (Int, Int)
  |   ^
  |   Found:    Unit
  |   Required: (Int, Int)
  |
  | longer explanation available when compiling with `-explain`
3 errors found
*/
