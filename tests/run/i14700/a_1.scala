
object l1:
  def g(args: Int*)(block: => Unit): Unit = println(s"Well, ${args.sum} and $block")
