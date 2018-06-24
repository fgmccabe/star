test.d{
  import star.core.

  xx : index[boolean].
  xx = xc{
    get(false) => 0.
    get(_) default => 1.

    test = true.

    ee ~> integer.
  }

  all t ~~ index[t] ::= exists e ~~ xc{ get:(t)=>e. test:t. type ee : e. } | none.

  yy : xx.ee.
  yy = xx.get(false).

  assert xx.test.
}
