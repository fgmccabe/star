star.d{
  xx : index[logical].
  xx = xc{
    get(false) => 0.
    get(true) => 1.

    ee ~> integer.
  }

  all t ~~ index[t] ::= exists e ~~ xc{ get:(t)=>e. type ee : e. } | none.

  logical ::= false | true.

  yy : xx.ee.
  yy = xx.get(false).
}
