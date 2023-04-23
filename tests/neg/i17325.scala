
// scalac: -Werror

import language.implicitConversions

@main
def main() = {
  class DoubleOps(d: Double) {
    def unary_~(): Double = scala.math.ceil(d) // error
  }
  implicit def doubleOps(d: Double): DoubleOps = new DoubleOps(d)

  println {
    ~(0.5) // error
    //"hi" // allowed the warning
  }
}
