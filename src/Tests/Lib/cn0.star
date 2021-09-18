test.cn0{
  import star.core.

  public contract all x ~~ ar[x] ::= {
    plus:(x,x)=>x.
    minus:(x,x)=>x.
    times:(x,x)=>x.
    div:(x,x)=>x.
  }

  public implementation ar[integer] => {.
    plus(x,y) => _int_plus(x,y).
    minus(x,y) => _int_minus(x,y).
    times(x,y) => _int_times(x,y).
    div(x,y) => _int_div(x,y).
  .}

  public implementation all e ~~ ar[e] |: ar[cons[e]] => let{
    pl(.nil,.nil) => .nil.
    pl(cons(H1,T1),cons(H2,T2)) => cons(plus(H1,H2),pl(T1,T2)).

    mn(.nil,.nil) => .nil.
    mn(cons(H1,T1),cons(H2,T2)) => cons(minus(H1,H2),mn(T1,T2)).

    tm(.nil,.nil) => .nil.
    tm(cons(H1,T1),cons(H2,T2)) => cons(times(H1,H2),tm(T1,T2)).

    dv(.nil,.nil) => .nil.
    dv(cons(H1,T1),cons(H2,T2)) => cons(div(H1,H2),dv(T1,T2)).
  } in {.
    plus(L1,L2) => pl(L1,L2).
    minus(L1,L2) => mn(L1,L2).
    times(L1,L2) => tm(L1,L2).
    div(L1,L2) => dv(L1,L2).
  .}

  public implementation all e,f ~~ ar[e],ar[f] |: ar[(e,f)] => {.
    plus((L1,R1),(L2,R2)) => (plus(L1,L2),plus(R1,R2)).
    minus((L1,R1),(L2,R2)) => (minus(L1,L2),minus(R1,R2)).
    times((L1,R1),(L2,R2)) => (times(L1,L2),times(R1,R2)).
    div((L1,R1),(L2,R2)) => (div(L1,L2),div(R1,R2)).
  .}

  public contract all x ~~ cmp[x] ::= {
    (<) : (x,x)=>boolean.
  }
}  
