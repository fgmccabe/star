test.do11{
  import star.
  import star.script.

  foo ::= foo(integer,string).

  locOf(foo(L,_)) => L.

  multiWhr:(foo)=>action[(),integer].
  multiWhr(X where foo(_,_).=X) where V .= locOf(X) => do{
    logMsg("$(V)");
    valis V
  }

  main:()=>action[(),()].
  main()=>action{
    assert valof multiWhr(foo(23,""))==23
  }
}

  
