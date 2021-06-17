test.cn0{
  public boolean ::= .true | .false.

  public contract all x ~~ ar[x] ::= {
    plus:(x,x)=>x.
  }

  public implementation ar[integer] => {.
    plus(x,y) => _int_plus(x,y).
  .}

  public cons[e] ::= .nil | cons(e,cons[e]).

  implementation all e ~~ ar[e] |: ar[cons[e]] => let{
    pl(.nil,.nil) => .nil.
    pl(cons(H1,T1),cons(H2,T2)) => cons(plus(H1,H2),pl(T1,T2)).
  } in {.
    plus(L1,L2) => pl(L1,L2)
  .}

  implementation all e,f ~~ ar[e],ar[f] |: ar[(e,f)] => {.
    plus((L1,R1),(L2,R2)) => (plus(L1,L2),plus(R1,R2)).
  .}

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

  logM:(string)=>action[()].
  logM(M) => do{
    _ = _logmsg(M);
    return ()
  }
}  
