test.e{
  import star.
  import star.assert.

  xx = xc{
    get(.false) => 0.
    get(_) default => 1.

    test = .true.

    check(0)=>.false.
    check(_) default => .true.

    ee ~> integer.
  }

  all t ~~ index[t] ::= exists e ~~ xc{ get:(t)=>e. test:t. check:(e)=>t. ee ~> e. }.

  yy : xx.ee.
  yy = xx.get(.false).

  main:()=>().
  main()=>valof{
    assert xx.test;

    -- assert yy==0; -- should report a syntax error

    assert ~ xx.check(yy);
    
    valis ()
  }
}
