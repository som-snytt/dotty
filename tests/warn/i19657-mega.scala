//> abusing options -Wshadow:type-parameter-shadow
//> using options -Wshadow:type-parameter-shadow -Wunused:all

class F[X, M[N[X]]]:
  private def x[X] = toString // warn // warn
