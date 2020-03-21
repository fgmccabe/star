test.ralist{
  import star.

  tree[e] ::= leaf(e) | node(integer,tree[e],tree[e]).
  digit[e] ::= .zer | one(tree[e]).

  public all e ~~ ra[e] ::= ra(cons[digit[e]]).

  link:all e ~~ (tree[e],tree[e])=>tree[e].
  link(t1,t2) => node(size(t1)+size(t2),t1,t2).

  consTree:all e ~~ (tree[e],cons[digit[e]])=>cons[digit[e]].
  consTree(e,.nil) => cons(one(e),.nil).
  consTree(e,cons(.zer,ts)) => cons(one(e),ts).
  consTree(e,cons(one(t1),ts)) => cons(.zer,consTree(link(e,t1),ts)).

  unconsTree:all e ~~ (cons[digit[e]]) => option[(tree[e],cons[digit[e]])].
  unconsTree(.nil) => .none.
  unconsTree(cons(one(t),.nil)) => some((t,.nil)).
  unconsTree(cons(one(t),ts)) => some((t,cons(.zer,ts))).
  unconsTree(cons(.zer,ts)) where (node(_,t1,t2),ts1) ^= unconsTree(ts) =>
    some((t1,cons(one(t2),ts1))).

  lookupTree:all e ~~ (integer,tree[e]) => option[e].
  lookupTree(0,leaf(X)) => some(X).
  lookupTree(Ix,leaf(_)) => .none.
  lookupTree(Ix,node(W,t1,t2)) where Ix<W/2 => lookupTree(Ix,t1).
  lookupTree(Ix,node(W,t1,t2)) => lookupTree(Ix-W/2,t2).

  lookup:all e ~~ (integer,cons[digit[e]]) => option[e].
  lookup(_,.nil) => .none.
  lookup(Ix,cons(.zer,ts)) => lookup(Ix,ts).
  lookup(Ix,cons(one(t),ts)) where sz.=size(t) =>
    (Ix<sz ?
	lookupTree(Ix,t) ||
	lookup(Ix-sz,ts)).

  updateTree:all e ~~ (integer,e,tree[e])=>tree[e].
  updateTree(0,x,leaf(_)) => leaf(x).
  updateTree(Ix,x,node(w,t1,t2)) where w2.=w/2 =>
    (Ix<w2 ?
	node(w,updateTree(Ix,x,t1),t2) ||
	node(w,t1,updateTree(Ix-w2,x,t2))).

  update:all e ~~ (integer,e,cons[digit[e]]) => cons[digit[e]].
  update(0,x,.nil) => cons(one(leaf(x)),.nil).
  update(Ix,x,cons(.zer,ts)) => cons(.zer,update(Ix,x,ts)).
  update(Ix,x,cons(one(t1),ts)) where sz.=size(t1) =>
    (Ix<sz ?
	cons(one(updateTree(Ix,x,t1)),ts) ||
	cons(one(t1),update(Ix-sz,x,ts))).

  removeFromTree:all e ~~ (integer,tree[e])=>digit[e].
  removeFromTree(0,leaf(_)) => .zer.
  removeFromTree(Ix,node(w,t1,t2)) where w2.=w/2 =>
    (Ix<w2 ?
	unwrapDigit(w,removeFromTree(Ix,t1),one(t2)) ||
	unwrapDigit(w,one(t1),removeFromTree(Ix-w2,t2))).

  unwrapDigit(w,.zer,t) => t.
  unwrapDigit(w,t,.zer) => t.
  unwrapDigit(w,one(l),one(r)) => one(node(w,l,r)).

  remove:all e ~~ (integer,cons[digit[e]]) => cons[digit[e]].
  remove(0,.nil) => .nil.
  remove(Ix,cons(.zer,ts)) => cons(.zer,remove(Ix,ts)).
  remove(Ix,cons(one(t1),ts)) where sz.=size(t1) =>
    (Ix<sz ?
	cons(removeFromTree(Ix,t1),ts) ||
	cons(one(t1),remove(Ix-sz,ts))).

  

  public implementation all e ~~ folding[ra[e]->>e] => {.
    foldLeft(F,U,ra(Ds)) => foldDLeft(F,U,Ds).
    foldRight(F,U,ra(Ds)) => foldDRight(F,U,Ds).
  .}

  foldDLeft:all e,x ~~ ((x,e)=>x,x,cons[digit[e]]) => x.
  foldDLeft(_,X,.nil) => X.
  foldDLeft(F,X,cons(one(T),R)) => foldDLeft(F,foldTLeft(F,X,T),R).
  foldDLeft(F,X,cons(.zer,R)) => foldDLeft(F,X,R).

  foldTLeft:all e,x ~~ ((x,e)=>x,x,tree[e]) => x.
  foldTLeft(F,X,leaf(E)) => F(X,E).
  foldTLeft(F,X,node(W,L,R)) => foldTLeft(F,foldTLeft(F,X,L),R).

  foldDRight:all e,x ~~ ((e,x)=>x,x,cons[digit[e]])=>x.
  foldDRight(_,X,.nil) => X.
  foldDRight(F,X,cons(.zer,R)) => foldDRight(F,X,R).
  foldDRight(F,X,cons(one(T),R)) => foldTRight(F,foldDRight(F,X,R),T).

  foldTRight:all e,x ~~ ((e,x)=>x,x,tree[e])=>x.
  foldTRight(F,X,leaf(E)) => F(E,X).
  foldTRight(F,X,node(_,L,R)) => foldTRight(F,foldTRight(F,X,R),L).

  implementation all e ~~ sizeable[tree[e]] => {.
    size(leaf(_)) => 1.
    size(node(w,_,_)) => w.

    isEmpty(leaf(_))=>.true.
    isEmpty(node(_,_,_))=>.false.
  .}

  public implementation all e ~~ sequence[ra[e]->>e] => {.
    _nil = ra(.nil).
    _cons(E,ra(L)) => ra(consTree(leaf(E),L)).
    _apnd(ra(L),E) => ra(consTree(leaf(E),L)).
  .}

  public implementation all e ~~ head[ra[e]->>e] => {.
    head(ra(.nil)) => .none.
    head(ra(ts)) where (leaf(h),_) ^= unconsTree(ts) => some(h).

    tail(ra(.nil)) => .none.
    tail(ra(ts)) where (_,tl) ^= unconsTree(ts) => some(ra(tl)).
  .}

  public implementation all e ~~ indexed[ra[e]->>integer,e] => {.
    _index(ra(ts),Ix) => lookup(Ix,ts).

    _put(ra(ts),Ix,V) => ra(update(Ix,V,ts)).

    _remove(ra(ts),Ix) => ra(remove(Ix,ts)).

    _empty = ra(.nil).
  .}

  public implementation all e ~~ reversible[ra[e]] => {.
    reverse(ra(L)) => foldDLeft((So,E)=>[E,..So],[],L).
  .}

  public implementation all e ~~ concat[ra[e]] => {.
    ra(L)++R => foldDRight((E,So)=>[E,..So],R,L).
  .}

  public implementation all e ~~ filter[ra[e]->>e] => {
    (ra(L)^/F) => let{
      ff(E,So) where F(E) => [E,..So].
      ff(_,So) => So.
    } in foldDRight(ff,[],L)
  }
  
  public implementation all e ~~ display[e] |: dump[ra[e]] => {
    dump(ra(ts)) => ssSeq([ss("ra["),dumpList(ts),ss("]")]).

    dumpList(.nil) => ss("").
    dumpList(cons(.zer,ts)) => ssSeq([ss("."),dumpList(ts)]).
    dumpList(cons(one(t),ts)) => ssSeq([dumpTree(t),dumpList(ts)]).

    dumpTree(leaf(x)) => disp(x).
    dumpTree(node(w,t1,t2)) => ssSeq([ss("{"), disp(w), dumpTree(t1), dumpTree(t2),ss("}")]).
  }

  public implementation all e ~~ display[e] |: display[ra[e]] => let{
    dispList:(cons[digit[e]])=>ss.
    dispList(.nil) => ss("").
    dispList(cons(.zer,ts))=>dispList(ts).
    dispList(cons(one(t),ts)) =>
      ssSeq([dispTree(t), dispList(ts)]).

    dispTree:(tree[e]) => ss.
    dispTree(leaf(x)) => ssSeq([disp(x),ss(",")]).
    dispTree(node(_,t1,t2)) => ssSeq([dispTree(t1),dispTree(t2)]).
  } in {
    disp(ra(ts)) => ssSeq([ss("["), dispList(ts), ss("]")]).
  }
}
