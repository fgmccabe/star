test.cn0{
  public boolean ::= .true | .false.

  public contract all x ~~ ar[x] ::= {
    plus:(x,x)=>x.
  }

  public implementation ar[integer] => {.
    plus(x,y) => _int_plus(x,y).
  .}

  public cons[e] ::= .nil | cons(e,cons[e]).

  public either[e,o] ::= either(e) | other(o).

  public implementation all e ~~ ar[e] |: ar[cons[e]] => let{
    pl(.nil,.nil) => .nil.
    pl(cons(H1,T1),cons(H2,T2)) => cons(plus(H1,H2),pl(T1,T2)).
  } in {.
    plus(L1,L2) => pl(L1,L2)
  .}

  public implementation all e,f ~~ ar[e],ar[f] |: ar[(e,f)] => {.
    plus((L1,R1),(L2,R2)) => (plus(L1,L2),plus(R1,R2)).
  .}

  ee:all alpha,beta ~~ (alpha) => either[alpha,beta].
  ee(E) => either(E).

}  
