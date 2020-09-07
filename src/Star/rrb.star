star.rrb{
  import star.core.
  import star.arith.
  import star.bits.

  -- Relaxed Radix Balanced Trees - for vectors

  public all e ~~ rrb[e] ::= .rrbE
    | lf1(e)
    | lf2(e,e)
    | lf3(e,e,e)
    | lf4(e,e,e,e)
    | node1(integer,integer,rrb[e])
    | node2(integer,integer,integer,rrb[e],rrb[e])
    | node3(integer,integer,integer,integer,rrb[e],rrb[e],rrb[e])
    | node4(integer,integer,integer,integer,integer,rrb[e],rrb[e],rrb[e],rrb[e]).

  rrbGet:all e ~~ (rrb[e],integer)=>option[e].
  rrbGet(.rrbE,_) => .none.
  rrbGet(lf1(X),0) => some(X).
  rrbGet(lf2(X,_),0) => some(X).
  rrbGet(lf2(_,X),1) => some(X).
  rrbGet(lf3(X,_,_),0) => some(X).
  rrbGet(lf3(_,X,_),1) => some(X).
  rrbGet(lf3(_,_,X),2) => some(X).
  rrbGet(lf4(X,_,_,_),0) => some(X).
  rrbGet(lf4(_,X,_,_),1) => some(X).
  rrbGet(lf4(_,_,X,_),2) => some(X).
  rrbGet(lf4(_,_,_,X),3) => some(X).
  rrbGet(node1(H,L1,T1),Ix) =>
    select1((Ix.>>.H).&.0x3,Ix,L1,T1).
  rrbGet(node2(H,L1,L2,T1,T2),Ix) =>
    select2((Ix.>>.H).&.0x3,Ix,L1,L2,T1,T2).
  rrbGet(node3(H,L1,L2,L3,T1,T2,T3),Ix) =>
    select3((Ix.>>.H).&.0x3,Ix,L1,L2,L3,T1,T2,T3).
  rrbGet(node4(H,L1,L2,L3,L4,T1,T2,T3,T4),Ix) =>
    select4((Ix.>>.H).&.0x3,Ix,L1,L2,L3,L4,T1,T2,T3,T4).
  rrbGet(_,_) default => .none.

  select1:all e ~~ (integer,integer,integer,rrb[e]) => option[e].
  select1(0,Ix,L1,T1) where Ix<L1 => rrbGet(T1,Ix).
  select1(0,Ix,L1,T1) where Ix>=L1 => .none.

  select2:all e ~~ (integer,integer,integer,integer,rrb[e],rrb[e]) => option[e].
  select2(0,Ix,L1,L2,T1,T2) where Ix<L1 => rrbGet(T1,Ix).
  select2(0,Ix,L1,L2,T1,T2) where Ix>=L1 => select2(1,Ix,L1,L2,T1,T2).
  select2(1,Ix,L1,L2,T1,T2) where Ix<L2 => rrbGet(T2,Ix-L1).
  select2(1,Ix,L1,L2,T1,T2) where Ix>=L2 => .none.


  select3:all e ~~ (integer,integer,integer,integer,integer,rrb[e],rrb[e],rrb[e]) => option[e].
  select3(0,Ix,L1,L2,L3,T1,T2,T3) where Ix<L1 => rrbGet(T1,Ix).
  select3(0,Ix,L1,L2,L3,T1,T2,T3) where Ix>=L1 => select3(1,Ix,L1,L2,L3,T1,T2,T3).
  select3(1,Ix,L1,L2,L3,T1,T2,T3) where Ix<L2 => rrbGet(T2,Ix-L1).
  select3(1,Ix,L1,L2,L3,T1,T2,T3) where Ix>=L2 => select3(2,Ix,L1,L2,L3,T1,T2,T3).
  select3(2,Ix,L1,L2,L3,T1,T2,T3) where Ix<L3 => rrbGet(T3,Ix-L2).
  select3(2,Ix,L1,L2,L3,T1,T2,T3) where Ix>=L3 => .none.

  select4:all e ~~ (integer,integer,integer,integer,integer,integer,rrb[e],rrb[e],rrb[e],rrb[e]) => option[e].
  select4(0,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix<L1 => rrbGet(T1,Ix).
  select4(0,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix>=L1 => select4(1,Ix,L1,L2,L3,L4,T1,T2,T3,T4).
  select4(1,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix<L2 => rrbGet(T2,Ix-L1).
  select4(1,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix>=L2 => select4(2,Ix,L1,L2,L3,L4,T1,T2,T3,T4).
  select4(2,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix<L3 => rrbGet(T3,Ix-L2).
  select4(2,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix>=L3 => select4(3,Ix,L1,L2,L3,L4,T1,T2,T3,T4).
  select4(3,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix<L4 => rrbGet(T4,Ix-L3).
  select4(3,Ix,L1,L2,L3,L4,T1,T2,T3,T4) where Ix>=L4 => .none.

  conc:all e ~~ (rrb[e],rrb[e])=>(rrb[e],rrb[e]).
  conc(.rrbE,R) => (R,.rrbE).
  conc(L,.rrbE) => (L,.rrbE).
  conc(lf1(E1),lf1(O1)) => (lf2(E1,O1),.rrbE).
  conc(lf1(E1),lf2(O1,O2)) => (lf3(E1,O1,O2),.rrbE).
  conc(lf1(E1),lf3(O1,O2,O3)) => (lf4(E1,O1,O2,O3),.rrbE).
  conc(lf1(E1),lf4(O1,O2,O3,O4)) => (lf4(E1,O1,O2,O3),lf1(O4)).
  conc(lf2(E1,E2),lf1(O1)) => (lf3(E1,E2,O1),.rrbE).
  conc(lf2(E1,E2),lf2(O1,O2)) => (lf4(E1,E2,O1,O2),.rrbE).
  conc(lf2(E1,E2),lf3(O1,O2,O3)) => (lf4(E1,E2,O1,O2),lf1(O3)).
  conc(lf2(E1,E2),lf4(O1,O2,O3,O4)) => (lf4(E1,E2,O1,O2),lf2(O3,O4)).
  conc(lf3(E1,E2,E3),lf1(O1)) => (lf4(E1,E2,E3,O1),.rrbE).
  conc(lf3(E1,E2,E3),lf2(O1,O2)) => (lf4(E1,E2,E3,O1),lf1(O2)).
  conc(lf3(E1,E2,E3),lf3(O1,O2,O3)) => (lf4(E1,E2,E3,O1),lf2(O2,O3)).
  conc(lf3(E1,E2,E3),lf4(O1,O2,O3,O4)) => (lf4(E1,E2,E3,O1),lf3(O2,O3,O4)).
  conc(lf4(E1,E2,E3,E4),R) => (lf4(E1,E2,E3,E4),R).
  conc(node1(H,L1,N1),node1(H,L2,N2)) where (L,R) .= conc(N1,N2) =>
    mkNode(H,L,R,.rrbE,.rrbE).

  mkNode:all e ~~ (integer,rrb[e],rrb[e],rrb[e],rrb[e])=>(rrb[e],rrb[e]).
  mkNode(H,N1,.rrbE,_,_) => (node1(H,size(N1),N1),.rrbE).
  

  height:all e ~~ (rrb[e])=>integer.
  height(.rrbE) => 0.
  height(lf1(_)) => 0.
  height(lf2(_,_)) => 0.
  height(lf3(_,_,_)) => 0.
  height(lf4(_,_,_,_)) => 0.
  height(node1(H,_,_)) => H.
  height(node2(H,_,_,_,_)) => H.
  height(node3(H,_,_,_,_,_,_)) => H.
  height(node4(H,_,_,_,_,_,_,_,_)) => H.

  count:all e ~~ (rrb[e])=>integer.
  count(.rrbE) => 0.
  count(lf1(_)) => 1.
  count(lf2(_,_)) => 2.
  count(lf3(_,_,_)) => 3.
  count(lf4(_,_,_,_)) => 4.
  count(node1(_,C,_)) => C.
  count(node2(_,_,C,_,_)) => C.
  count(node3(_,_,_,C,_,_,_)) => C.
  count(node4(_,_,_,_,C,_,_,_,_)) => C.
  
  private adjustHeight:all e ~~ (integer,integer,rrb[e])=>rrb[e].
  adjustHeight(H,H,T) => T.
  adjustHeight(N,H,T) => adjustHeight(N,H+2,node1(H,count(T),T)).

  public implementation all e ~~ sizeable[rrb[e]] => let{
    sz(.rrbE)=>0.
    sz(lf1(_))=>1.
    sz(lf2(_,_))=>2.
    sz(lf3(_,_,_))=>3.
    sz(lf4(_,_,_,_))=>4.
    sz(node1(_,C,_))=>C.
    sz(node2(_,_,C,_,_))=>C.
    sz(node3(_,_,_,C,_,_,_))=>C.
    sz(node4(_,_,_,_,C,_,_,_,_))=>C.
  } in {.
    isEmpty(.rrbE)=>.true.
    isEmpty(_) default => .false.

    size(N) => sz(N)
  .}
    

  -- public implementation all e ~~ sequence[rrb[e]->>e] => {.
  --   _nil = .rrbE.
  --   _cons(H,T) => conc(adjustHeight(0,height(T),lf1(H)),T,0)
  -- .}
}
