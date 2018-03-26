star.d{
  xx : index[boolean].
  xx = xc{
    get(false) => 0.
    get(true) => 1.

    test = true.

    ee ~> integer.
  }

  all t ~~ index[t] ::= exists e ~~ xc{ get:(t)=>e. test:t. type ee : e. } | none.

  boolean ::= false | true.

  yy : xx.ee.
  yy = xx.get(false).

  assert xx.test.
}
