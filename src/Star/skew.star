star.skew{
  import star.

  tree[a] ::= leaf(a) | node(a,tree[a],tree[a]).

  all a ~~ rlist[a] ~> cons[(integer,tree[a])].

  public rl[a] ::= private rl(rlist[a]).

  cns:all a ~~ (a,rlist[a]) => rlist[a].
  cns(x,ts where cons((w1,t1),cons((w2,t2),rest)).=ts) =>
    (w1==w2 ?
	cons((w1+w2+1,node(x,t1,t2)),rest) ||
	cons((1,leaf(x)),ts)).
  cns(x,ts) => cons((1,leaf(x)),ts).

  hed:all a ~~ (rlist[a]) => a.
  hed(cons((1,leaf(x)),_)) => x.
  hed(cons((w,node(x,_,_)),_)) => x.

  tl:all a ~~ (rlist[a]) => rlist[a].
  tl(cons((1,_),ts)) => ts.
  tl(cons((w,node(_,t1,t2)),ts)) => cons((w/2,t1),cons((w/2,t2),ts)).

  lookup:all a ~~ (integer,rlist[a]) => option[a].
  lookup(_,nil) => none.
  lookup(Ix,cons((w,t),ts)) where Ix<w => lookupTree(Ix,w,t).
  lookup(Ix,cons((w,_),ts)) => lookup(Ix-w,ts).

  lookupTree:all a ~~ (integer,integer,tree[a]) => option[a].
  lookupTree(0,1,leaf(x)) => some(x).
  lookupTree(0,w,node(x,_,_)) => some(x).
  lookupTree(Ix,w,node(_,t1,t2)) where w2 .= w/2 =>
    (Ix=<w2 ?
	lookupTree(Ix-1,w2,t1) ||
	lookupTree(Ix-1-w2,w2,t2)).
  lookupTree(_,_,_) default => none.

  update:all a ~~ (integer,rlist[a],a) => rlist[a].
  update(_,nil,V) => cons((1,leaf(V)),nil). -- be slightly forgiving here
  update(Ix,cons((w,T),rs),V) =>
    (Ix<w ?
	cons((w,updateTree(Ix,w,V,T)),rs) ||
	cons((w,T),update(Ix-w,rs,V))).

  updateTree:all a ~~ (integer,integer,a,tree[a]) => tree[a].
  updateTree(0,1,v,leaf(_)) => leaf(v).
  updateTree(0,w,v,node(_,t1,t2)) => node(v,t1,t2).
  updateTree(Ix,w,v,node(x,t1,t2)) where w2 .= w/2 =>
    (Ix=<w2 ?
	node(x,updateTree(Ix-1,w2,v,t1),t2) ||
	node(x,t1,updateTree(Ix-1-w2,w2,v,t2))).

  public implementation all e ~~ head[rl[e]->>e] => {
    head(rl(nil)) => none.
    head(rl(ts)) => some(hed(ts)).

    tail(rl(nil)) => none.
    tail(rl(ts)) => some(rl(tl(ts))).
  }

  public implementation all e ~~ sequence[rl[e]->>e] => {
    _nil = rl(nil).
    _cons(E,rl(L)) => rl(cns(E,L)).
  }

  public implementation all e ~~ indexed[rl[e]->>integer,e] => {
    _index(rl(ts),Ix) => lookup(Ix,ts).

    _put(rl(ts),Ix,V) => rl(update(Ix,ts,V)).

    _empty = rl(nil).
  }

  foldDLeft:all e,x ~~ ((x,e)=>x,x,rlist[e]) => x.
  foldDLeft(_,X,nil) => X.
  foldDLeft(F,X,cons((_,T),R)) => foldDLeft(F,foldTLeft(F,X,T),R).

  foldTLeft:all e,x ~~ ((x,e)=>x,x,tree[e]) => x.
  foldTLeft(F,X,leaf(E)) => F(X,E).
  foldTLeft(F,X,node(W,L,R)) => foldTLeft(F,foldTLeft(F,F(X,W),L),R).

  foldDRight:all e,x ~~ ((e,x)=>x,x,rlist[e])=>x.
  foldDRight(_,X,nil) => X.
  foldDRight(F,X,cons((_,T),R)) => foldTRight(F,foldDRight(F,X,R),T).

  foldTRight:all e,x ~~ ((e,x)=>x,x,tree[e])=>x.
  foldTRight(F,A,leaf(E)) => F(E,A).
  foldTRight(F,A,node(X,L,R)) => F(X,foldTRight(F,foldTRight(F,A,R),L)).

  public implementation all e ~~ folding[rl[e]->>e] => {
    foldLeft(F,U,rl(Ds)) => foldDLeft(F,U,Ds).
    foldRight(F,U,rl(Ds)) => foldDRight(F,U,Ds).
  }

  public implementation all e ~~ reversible[rl[e]] => {
    reverse(rl(L)) => rl(foldDLeft((So,E)=>cns(E,So),nil,L)).
  }

  public implementation all e ~~ concat[rl[e]] => {
    rl(L)++rl(R) => rl(foldDRight((E,So)=>cns(E,So),R,L)).
  }

  public implementation all e ~~ filter[rl[e]->>e] => {
    rl(L)^/F => let{
      ff(E,So) where F(E) => [E,..So].
      ff(_,So) => So.
    } in foldDRight(ff,[],L)
  }

  public implementation all e ~~ display[e] |: dump[rl[e]] => let{
    dumpList:all a ~~ display[a] |: (rlist[a]) => ss.
    dumpList(nil) => ss("").
    dumpList(cons((w,x),ts)) =>
      ssPr(ss("($(w)"),
	ssPr(dumpTree(x),
	  ssPr(dumpList(ts),
	    ss(")")))).

    dumpTree:all a ~~ display[a] |: (tree[a]) => ss.
    dumpTree(leaf(x)) => ssPr(ss("ƒ("), ssPr(disp(x),ss(")"))).
    dumpTree(node(w,t1,t2)) =>
      ssPr(ss("{"),
	ssPr(disp(w),
	  ssPr(dumpTree(t1),
	    ssPr(ss("-"),
	      ssPr(dumpTree(t2),
		ss("}")))))).
  } in {
    dump(rl(ts)) => ssPr(ss("rl["),ssPr(dumpList(ts),ss("]"))).
  }

  public implementation all e ~~ display[e] |: display[rl[e]] => let{
    dispList:all a ~~ display[a] |: (rlist[a],cons[ss]) => cons[ss].
    dispList(nil,L) => L.
    dispList(cons((_,T),rs),L) => dispTree(T,dispList(rs,L)).

    dispTree:all a ~~ display[a] |: (tree[a],cons[ss]) => cons[ss].
    dispTree(leaf(X),L) => cons(disp(X),L).
    dispTree(node(X,t1,t2),L) => cons(disp(X),dispTree(t1,dispTree(t2,L))).

    rollup:(cons[ss]) => ss.
    rollup(nil) => ss("").
    rollup(cons(S,nil)) => S.
    rollup(cons(S,R)) => ssPr(S,ssPr(ss(","),rollup(R))).
  } in {
    disp(rl(L)) => ssPr(ss("["),ssPr(rollup(dispList(L,nil)),ss("]"))).
  }

  public implementation all e ~~ equality[e] |: equality[rl[e]] => let{
    equalList(nil,nil) => true.
    equalList(cons((W1,T1),L1),cons((W2,T2),L2)) =>
      W1==W2 && equalTree(T1,T2) && equalList(L1,L2).
    equalList(_,_) default => false.

    equalTree(leaf(L1),leaf(L2)) => L1==L2.
    equalTree(node(X1,L1,R1),node(X2,L2,R2)) =>
      X1==X2 && equalTree(L1,L2) && equalTree(R1,R2).
    equalTree(_,_) default => false.
  } in {
    rl(L1) == rl(L2) => equalList(L1,L2).
  }

  public implementation all e ~~ sizeable[rl[e]] => {
    isEmpty(rl(nil)) => true.
    isEmpty(rl(_)) => false.

    size(rl(L)) => countSizes(L,0).

    countSizes(nil,Sz) => Sz.
    countSizes(cons((W,_),L),Sz) => countSizes(L,Sz+W).
  }
}
