test.e{
  import star.
  import star.script.

  xx = open xc{
    get(.false) => 0.
    get(_) default => 1.

    test = .true.

    ee ~> integer.
  }

  all t ~~ index[t] ::= exists e ~~ xc{ get:(t)=>e. test:t. type ee : e. }.

  yy : xx.ee.
  yy = xx.get(.false).

  main:()=>().
  main()=>valof{
    assert xx.test;
    valis ()
  }
}
