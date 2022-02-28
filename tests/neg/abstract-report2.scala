import java.util.Collection

class Foo extends Collection[Int] // error

class Bar extends Collection[List[_ <: String]] // error

class Baz[T] extends Collection[T]  // error

trait Xyz[T] {
  def foo(x: T): Boolean
}

trait Symbolic {
  def --? : Int
  def --!(i: Int): Unit
  def unary_~ : Long
}

trait Bippy[T1, T2, T3] extends collection.IterableOps[(T2, String), List, List[(T2, String)]] with Xyz[T3]

class Dingus extends Bippy[String, Set[Int], List[Int]] // error

class JustOne extends Collection[Int] { // error
  def add(x$1: Int): Boolean = ???
  def addAll(x$1: java.util.Collection[_ <: Int]): Boolean = ???
  def clear(): Unit = ???
  def contains(x$1: Object): Boolean = ???
  def containsAll(x$1: java.util.Collection[_]): Boolean = ???
  def isEmpty(): Boolean = ???
  def iterator(): java.util.Iterator[Int] = ???
  def remove(x$1: Object): Boolean = ???
  def removeAll(x$1: java.util.Collection[_]): Boolean = ???
  def retainAll(x$1: java.util.Collection[_]): Boolean = ???
  def size(): Int = ???
  //def toArray[T](x$1: Array[T with Object]): Array[T with Object] = ???
  def toArray(): Array[Object] = ???
}
