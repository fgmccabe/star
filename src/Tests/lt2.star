test.lt2{
  import star.
  import star.script.

  js ::=
    .jT | .jF  | jS(js).

  eqr = {.
    eq = equalJs
  .}.

  equalJs(.jT,.jT) => .true.
  equalJs(.jF,.jF) => .true.
  equalJs(jS(L1),jS(L2)) => execQ(L1,L2).
  equalJs(_,_) default => .false.

  execQ(L1,L2) => eqr.eq(L1,L2).

  test() => equalJs(jS(.jT),jS(.jT)).

  main:()=>action[(),()].
  main() => do{
    assert test();
    assert ~execQ(.jF,.jT)
  }
}

  
