cafe.d{
  xx : index[logical].
  xx = xc{
    get(false) => 0.
    get(true) => 1.

    e ~> integer.
  }

  all t ~~ index[t] ::= exists e ~~ xc{ get:(t)=>e. type e : e. } | none.

  logical ::= false | true.
}
