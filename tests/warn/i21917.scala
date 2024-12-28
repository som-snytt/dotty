//> using scala 3.6.2
//> using options -Wunused:imports
//
//> abusing scala 2.13.15
//> abusing options -Wunused:imports -Xsource:3

import Pet.Owner

class Dog(owner: Owner) extends Pet(owner) {
  import Pet.* // warn although unambiguous
  //import Car.* // ambiguous

  def bark(): String = "bite"

  def this(owner: Owner, goodDog: Boolean) = {
    this(owner)
    if (goodDog) println(s"$owner's dog is a good boy")
  }
  
  val getOwner: Owner = owner
}

class Pet(val owner: Owner)

object Pet {
  class Owner
}

object Car {
  class Owner
}
