val m1 = Map("1" -> Set("a"))
val m2 = Map("1" -> Set("a"))

val result = Set("1").collect { id =>
  m2.get(id).flatMap { x =>
    m1
      .get(id)
      .filter(_ == x)
  } match {
    case Some(template) => template
    case None           => ???
  }
}
