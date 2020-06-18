test.lt2a{
  import star.
  import star.script.

  js ::=
    .jN | jS(js).

  implementation equality[js] => {.
    X == Y => equalJ(eqr,X,Y)
  .}

  eqR ::= eqR((eqR,js,js)=>boolean).

  eqr = eqR(equalJ).
  
  equalJ:(eqR,js,js)=>boolean.
  equalJ(_,.jN,.jN) => .true.
  equalJ(E,jS(L1),jS(L2)) => execE(E,L1,L2).
  equalJ(_,_,_) default => .false.

  execE(E,L1,L2) => equalJ(E,L1,L2).

  test()=>runner(()=>jS(.jN)==jS(.jN)).

  runner(F) => F().

  main:()=>action[(),()].
  main() => do{
    assert test()
  }
}
