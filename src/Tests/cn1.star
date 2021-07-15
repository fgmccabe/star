test.cn1{
  import test.cn0.

  _perform:all a,e ~~ (action[a,e]) => a.
  _perform(action(F)) => case F() in {
    either(R) => R
  }

  public dblInt:(integer)=>integer.
  dblInt(X) => plus(X,X).

  dblCons:(cons[integer])=>cons[integer].
  dblCons(X) => plus(X,X).

  dbl:all x ~~ ar[x] |: (x)=>x.
  dbl(X) => plus(X,X).

  ddbl:((integer,cons[integer]))=>(integer,cons[integer]).
  ddbl(T1) => plus(T1,T1).

  dd = ddbl((23,.nil)).

  consLen(.nil) => 0.
  consLen(cons(_,T)) => plus(1,consLen(T)).

  logM:(string)=>action[(),()].
  logM(M) => action{
    _ .= _logmsg(M);
    return ()
  }

  _main:(cons[string])=>().
  _main(cons(A,.nil)) =>
    valof main(A).

  main:(string)=>action[(),()].
  main(T) => action{
    logM(T)
  }
}
