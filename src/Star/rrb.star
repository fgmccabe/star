star.rrb{
  import star.core.
  import star.arith.

  -- Relaxed Radix Balanced Trees - for vectors

  public all e ~~ rrb[e] ::= rrbE
        | lf(e)
        | node4(integer,integer,integer,integer,integer,rrb[e],rrb[e],rrb[e],rrb[e]).

  rrbGet:all e ~~ (rrb[e],integer)=>option[e].
  rrbGet(rrbE,_) => none.
  rrbGet(lf(E),0) => some(E).
  rrbGet(lf(_),_) => none.
  rrbGet(node4(_,L1,L2,L3,L4,T1,T2,T3,T4),Ix) => select4(Ix,L1,L2,L3,L4,T1,T2,T3,T4).

  select4:all e ~~ (integer,integer,integer,integer,integer,rrb[e],rrb[e],rrb[e],rrb[e]) => option[e].
  select4(Ix,L1,L2,L3,L4,T1,T2,T3,T4) =>
    (Ix<L1 ? rrbGet(T1,Ix)
     || Ix<L2 ? rrbGet(T2,Ix-L1)
     || Ix<L3 ? rrbGet(T3,Ix-L2)
     || Ix<L4 ? rrbGet(T4,Ix-L3)
     || none).

  conc_ph1:all e ~~ (rrb[e],rrb[e])=>rrb[e].
  conc_ph1(node4(Dp,L1,L2,L3,L4,T1,T2,T3,T4),RT) where Dp>0 =>
    conc_ph1a(L1,L2,L3,L4,T1,T2,T3,T4,RT).


  conc_ph1a:all e ~~ (integer,integer,integer,integer,rrb[e],rrb[e],rrb[e],rrb[e],rrb[e]) => rrb[e].
  conc_ph1a(L1,L2,L3,L4,T1,T2,T3,T4,RT)
}
