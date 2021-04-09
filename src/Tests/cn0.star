test.cn0{
  public boolean ::= .true | .false.

  contract all x ~~ ar[x] ::= {
    plus:(x,x)=>x.
  }

  implementation ar[integer] => {.
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

  dblInt:(integer)=>integer.
  dblInt(X) => plus(X,X).

  dblCons:(cons[integer])=>cons[integer].
  dblCons(X) => plus(X,X).

  dbl:all x ~~ ar[x] |: (x)=>x.
  dbl(X) => plus(X,X).

  ddbl:((integer,cons[integer]))=>(integer,cons[integer]).
  ddbl(T1) => plus(T1,T1).
}  
