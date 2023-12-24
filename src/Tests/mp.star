test.mp{
  import star.
  import star.assert.
  
  contract all m/1,e,f ~~ mapp[m->>e,f] ::= {
    map:(m[e],(e)=>f) => m[f].
  }

  implementation all e,f ~~ mapp[cons->>e,f] => {.
    map(L,F) => mapOverList(L,F).

    mapOverList(.nil,_) => .nil.
    mapOverList(.cons(H,T),F) => .cons(F(H),mapOverList(T,F)).
  .}

  double:(integer)=>integer.
  double(X) => X+X.

  main:()=>().
  main()=>valof{
    assert map(.cons("a",.cons("b",.nil)),(X)=>X++"*")==["a*","b*"];
    assert map(.cons(1,.cons(2,.nil)),double)==[2,4];
    valis ()
  }
}
