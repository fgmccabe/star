test.do11{
  import star.
  import star.script.

  foo ::= .foo(integer,string).

  locOf(.foo(L,_)) => L.

  multiWhr:(foo)=>integer.
  multiWhr(X where .foo(_,_).=X) where V .= locOf(X) => valof{
    logMsg("$(V)");
    valis V
  }

  main:()=>().
  main()=>valof{
    assert multiWhr(.foo(23,""))==23;
      
    valis ()
  }
}

  
