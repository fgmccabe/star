star.skew{
  import star.
  import star.iterable.
  import star.monad.

  tree[a] ::= .eTree | .node(a,tree[a],tree[a]).

  all a ~~ rlist[a] ~> cons[(integer,tree[a])].

  public sk[a] ::= .rl(rlist[a]).

  cns:all a ~~ (a,rlist[a]) => rlist[a].
  cns(x,ts where .cons((w1,t1),.cons((w2,t2),rest)).=ts) =>
    (w1==w2 ??
	.cons((w1+w2+1,.node(x,t1,t2)),rest) ||
	.cons((1,.node(x,.eTree,.eTree)),ts)).
  cns(x,ts) => .cons((1,.node(x,.eTree,.eTree)),ts).

  hed:all a ~~ (rlist[a]) => a.
  hed(.cons((w,.node(x,_,_)),_)) => x.

  tl:all a ~~ (rlist[a]) => rlist[a].
  tl(.cons((1,_),ts)) => ts.
  tl(.cons((w,.node(_,t1,t2)),ts)) => .cons((w.>>.1,t1),.cons((w.>>.1,t2),ts)).

  lookup:all a ~~ (integer,rlist[a]) => option[a].
  lookup(_,.nil) => .none.
  lookup(Ix,.cons((w,t),ts)) where Ix<w => lookupTree(Ix,w,t).
  lookup(Ix,.cons((w,_),ts)) => lookup(Ix-w,ts).

  lookupTree:all a ~~ (integer,integer,tree[a]) => option[a].
  lookupTree(0,w,.node(x,_,_)) => .some(x).
  lookupTree(Ix,w,.node(_,t1,t2)) where w2 .= w.>>.1 =>
    (Ix=<w2 ??
	lookupTree(Ix-1,w2,t1) ||
	lookupTree(Ix-1-w2,w2,t2)).
  lookupTree(_,_,_) default => .none.

  update:all a ~~ (integer,rlist[a],a) => rlist[a].
  update(_,.nil,V) => .cons((1,.node(V,.eTree,.eTree)),.nil). -- be slightly forgiving here
  update(Ix,.cons((w,T),rs),V) =>
    (Ix<w ??
	.cons((w,updateTree(Ix,w,V,T)),rs) ||
	.cons((w,T),update(Ix-w,rs,V))).

  updateTree:all a ~~ (integer,integer,a,tree[a]) => tree[a].
  updateTree(0,w,v,.node(_,t1,t2)) => .node(v,t1,t2).
  updateTree(Ix,w,v,.node(x,t1,t2)) where w2 .= w.>>.1 =>
    (Ix=<w2 ??
	.node(x,updateTree(Ix-1,w2,v,t1),t2) ||
	.node(x,t1,updateTree(Ix-1-w2,w2,v,t2))).

  remove:all a ~~ (integer,rlist[a]) => rlist[a].
  remove(_,.nil) => .nil. -- be slightly forgiving here
  remove(Ix,.cons((w,T),rs)) =>
    (Ix<w ??
	.cons(removeTree(Ix,w,T),rs) ||
	.cons((w,T),remove(Ix-w,rs))).

  removeTree:all a ~~ (integer,integer,tree[a]) => (integer,tree[a]).
  removeTree(0,w,.node(_,t1,t2)) => (w-1,mergeTree(t1,t2)).
  removeTree(Ix,w,.node(x,t1,t2)) where w2 .= w.>>.1 =>
    (Ix=<w2 ?? valof{
	(_,NL) = removeTree(Ix-1,w2,t1);
	valis (w-1,.node(x,NL,t2))
      } ||
      valof{
	(_,NR) = removeTree(Ix-1-w2,w2,t2);
	valis (w-1,.node(x,t1,NR))
      }).

  mergeTree(.eTree,T) => T.
  mergeTree(T,.eTree) => T.
  mergeTree(.node(v1,l1,r1),T2) =>
    .node(v1,l1,mergeTree(r1,T2)).

  public implementation all e ~~ head[sk[e]->>e] => {
    head(.rl(.nil)) => .none.
    head(.rl(ts)) => .some(hed(ts)).

    tail(.rl(.nil)) => .none.
    tail(.rl(ts)) => .some(.rl(tl(ts))).
  }

  public implementation all e ~~ build[sk[e]->>e] => {
    _null = .rl(.nil).
    _push(E,.rl(L)) => .rl(cns(E,L)).
  }

  public implementation all e ~~ sequence[sk[e]->>e] => {
    _nil = .rl(.nil).
    _cons(E,.rl(L)) => .rl(cns(E,L)).
  }

  public implementation all e ~~ indexed[sk[e]->>integer,e] => {
    _index(.rl(ts),Ix) => lookup(Ix,ts).

    _put(.rl(ts),Ix,V) => .rl(update(Ix,ts,V)).

    _remove(.rl(ts),Ix) => .rl(remove(Ix,ts)).

    _empty = .rl(.nil).
  }

  foldDLeft:all e,x ~~ ((e,x)=>x,x,rlist[e]) => x.
  foldDLeft(_,X,.nil) => X.
  foldDLeft(F,X,.cons((_,T),R)) => foldDLeft(F,foldTLeft(F,X,T),R).

  foldTLeft:all e,x ~~ ((e,x)=>x,x,tree[e]) => x.
  foldTLeft(F,X,.eTree) => X.
  foldTLeft(F,X,.node(W,L,R)) => foldTLeft(F,foldTLeft(F,F(W,X),L),R).

  foldDRight:all e,x ~~ ((e,x)=>x,x,rlist[e])=>x.
  foldDRight(_,X,.nil) => X.
  foldDRight(F,X,.cons((_,T),R)) => foldTRight(F,foldDRight(F,X,R),T).

  foldTRight:all e,x ~~ ((e,x)=>x,x,tree[e])=>x.
  foldTRight(F,A,.eTree) => A.
  foldTRight(F,A,.node(X,L,R)) => F(X,foldTRight(F,foldTRight(F,A,R),L)).

  public implementation all e ~~ folding[sk[e]->>e] => {
    foldLeft(F,U,.rl(Ds)) => foldDLeft(F,U,Ds).
    foldRight(F,U,.rl(Ds)) => foldDRight(F,U,Ds).
  }

  public implementation all e ~~ reversible[sk[e]] => {
    reverse(.rl(L)) => .rl(foldDLeft((E,So)=>cns(E,So),.nil,L)).
  }

  public implementation all e ~~ concat[sk[e]] => {
    .rl(L)++.rl(R) => .rl(foldDRight((E,So)=>cns(E,So),R,L)).
    _multicat([]) => .rl(.nil).
    _multicat([S1,..Ss]) => S1++_multicat(Ss).
  }

  public implementation all e ~~ filter[sk[e]->>e] => let {
    filter(.rl(L),F) => let{
      ff(E,So) where F(E) => [E,..So].
      ff(_,So) => So.
    } in foldDRight(ff,[],L)
  } in {
    (^/)=filter
  }

  public implementation functor[sk] => let{.
    fm:all a,b ~~ ((a)=>b,tree[a])=>tree[b].
    fm(F,.eTree) => .eTree.
    fm(F,.node(X,L,R)) => .node(F(X),fm(F,L),fm(F,R)).

    fmp:all a,b ~~ ((a)=>b,sk[a])=>sk[b].
    fmp(F,.rl(L)) => .rl(L//(((Ix,T))=>(Ix,fm(F,T)))).
  .} in {
    fmap = fmp.
  }
  
  public implementation all e ~~ display[e] |: display[sk[e]] => let{.
    dispList:all a ~~ display[a] |: (rlist[a],cons[string]) => cons[string].
    dispList(.nil,L) => L.
    dispList(.cons((_,T),rs),L) => dispTree(T,dispList(rs,L)).

    dispTree:all a ~~ display[a] |: (tree[a],cons[string]) => cons[string].
    dispTree(.eTree,L) => L.
    dispTree(.node(X,t1,t2),L) => [disp(X),..dispTree(t1,dispTree(t2,L))].

    rollup:(cons[string]) => string.
    rollup(.nil) => "".
    rollup(.cons(S,.nil)) => S.
    rollup(.cons(S,R)) => "#(S),#(rollup(R))".
 .} in {
    disp(.rl(L)) => "[#(rollup(dispList(L,.nil)))]".
  }

  public implementation all e ~~ equality[e] |: equality[sk[e]] => let{.
    equalList(.nil,.nil) => .true.
    equalList(.cons((W1,T1),L1),.cons((W2,T2),L2)) =>
      W1==W2 && equalTree(T1,T2) && equalList(L1,L2).
    equalList(_,_) default => .false.

    equalTree(.eTree,.eTree) => .true.
    equalTree(.node(X1,L1,R1),.node(X2,L2,R2)) =>
      X1==X2 && equalTree(L1,L2) && equalTree(R1,R2).
    equalTree(_,_) default => .false.
 .} in {
    .rl(L1) == .rl(L2) => equalList(L1,L2).
  }

  public implementation all e ~~ sizeable[sk[e]] => {.
    isEmpty(.rl(.nil)) => .true.
    isEmpty(.rl(_)) => .false.

    size(.rl(L)) => countSizes(L,0).

    private countSizes(.nil,Sz) => Sz.
    countSizes(.cons((W,_),L),Sz) => countSizes(L,Sz+W).
 .}

  public implementation all t ~~ iter[sk[t]->>t] => let{.
    iterList:all x ~~ (rlist[t],x,(t,x)=>x)=>x.
    iterList(.nil,St,_) => St.
    iterList(.cons((_,T),R),St,Fn) =>
      iterList(R,iterTree(T,St,Fn),Fn).

    iterTree:all x ~~ (tree[t],x,(t,x)=>x)=>x.
    iterTree(.eTree,St,Fn) => St.
    iterTree(.node(X,T1,T2),St,Fn) =>
      iterTree(T2,
	iterTree(T1,
	  Fn(X,St),Fn),Fn).
  .} in {
    _iter(.rl(L),St,Fn) => iterList(L,St,Fn).
  }

  public implementation all t ~~ generate[sk[t]->>t] => {
    _generate(T) => iterGenerator(T)
  }

  public implementation all t ~~ coercion[sk[t],cons[t]] => {
    _coerce(S) => .some(foldLeft((e,L) => .cons(e,L),.nil,S)).
  }
    
}
