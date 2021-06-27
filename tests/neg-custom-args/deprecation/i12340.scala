
// empty colonized template body is deprecated

class C: // error
end C

trait T: // error
end T

// sample trivial non-empty body

class D:
  this: D =>
end D

trait U:
  this: U =>
end U
