//> using options -Wunused:all

object PowerUser:
  import scala.concurrent.* // warn [taps mic]
  import Power.*
  def square(x: Double): Double = powerMacro(x, 2)
