test.do11{
  import star.
  import star.assert.

  foo ::= .foo(integer,string).

  lcOf(.foo(L,_)) => L.

  multiWhr:(foo)=>integer.
  multiWhr(X where .foo(_,_).=X) where V .= lcOf(X) => valof{
    logMsg("$(V)");
    valis V
  }

  main:()=>().
  main()=>valof{
    assert multiWhr(.foo(23,""))==23;
      
    valis ()
  }
}

  
