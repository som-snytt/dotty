
// something with nested regions expecting commas.
// let's go vertical!

class f[
     A,
     X <:
     (Int,
      Int,
     ),
     Y[
      A,
      X,
      ],
     B,
     ,
    ]
class g[
     A,
     ,
     ]
class C {
  def f(i: Int*) = i.sum
  def g = f(1,
    2,
    ,
    ,
    )
}
