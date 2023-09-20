test.dy2{
  import star.
  import star.assert.

  -- Commented out due to bug in boot compiler
  dyntype[e] ::= .dyn(/*(foo:integer)|:*/(e)=>integer).

  undyn(.dyn(F),A) => F(A).

  foo = 10.

  DD = .dyn((_)=>foo).

  EE = undyn(DD,"out").

  main:()=>().
  main() => let{
    foo = 3
  } in valof{
    show undyn(.dyn((_)=>foo),"test");
    show EE;

    assert undyn(.dyn((_)=>foo),"test")==3;
    assert EE == 10;
    valis ()
  }
}
    

    
