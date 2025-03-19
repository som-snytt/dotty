// sample options -coverage-out:coverage -Vprint:instrumentCoverage -Yexplicit-nulls

@main def Test = println:
  val m = new java.util.HashMap[String, Double]()
  def f: Option[Double] = Option(m.get(""))
  f

class More:
  def pi: Double = 3.14
  def answer: Int = 42

  def f[A, B](a: A)(b: B): String = a.toString + b.toString

  def g = f[Double, Int](pi)(answer)
