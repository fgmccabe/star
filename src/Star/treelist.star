star.treelist{
  import star.

  /*
  A tree structure used to represent arrays.
  Balanced, supports addition on either end only.
  */

  -- Constructors are not exposed directly

  -- the integer in the nodes denotes the size of the tree. Corresponds to the search key

  public tl[e] ::=        -- only expose the type
    private .tlEmpty |
      private tlRed(tl[e],integer,e,tl[e]) |
      private tlBlack(tl[e],integer,e,tl[e]).

  private (tlColor ::= .red | .black).

  colorOf:all e ~~ (tl[e])=>tlColor.
  colorOf(.tlEmpty)=>.black.
  colorOf(tlRed(_,_,_,_)) => .red.
  colorOf(tlBlack(_,_,_,_)) => black.

  tlSize:all e ~~ (tl[e])=>integer.
  tlSize(tlEmpty) => 0.
  tlSize(tlRed(_,Sz,_,_)) => Sz.
  tlSize(tlBlack(_,Sz,_,_)) => Sz.

  find_by_index:all e ~~ (tl[e],integer) => option[e].
  find_by_index(tlEmpty,_) => .none.
  find_by_index(tlRed(_,Ix,V,_),Ix) => some(V).
  find_by_index(tlBlack(_,Ix,V,_),Ix) => some(V).
  find_by_index(tlRed(L,_,_,R),Ix) where Ix<tlSize(L) => find_by_index(L,Ix).
  find_by_index(tlBlack(L,_,_,R),Ix) where Ix<tlSize(L) => find_by_index(L,Ix).

  find_by_index(tlRed(L,_,_,R),Ix) where Lx.=tlSize(L) && Ix>Lx => find_by_index(R,Ix-Lx-1).
  find_by_index(tlBlack(L,_,_,R),Ix) where Lx.=tlSize(L) && Ix>Lx => find_by_index(R,Ix-Lx-1).
  

  public implementation all e ~~ indexed[tl[e] ->> integer,e] => {
    _index(R,ix) where ix>=0 => find_by_index(R,ix).
    _put(R,ix,E) => put_by_index(R,ix,E).
    _remove(R,ix) => remove_by_index(R,ix).
    _empty = tlEmpty.
  }

  public implementation all e ~~ display[e] |: display[tl[e]] => let{
    dispTl(tlEmpty) => ss("").
    dispTl(tlLeaf(E)) => disp(E).
    dispTl(tlRed(L,_,R)) => ssSeq([dispTl(L),ss(","),dispTl(R)]).
    dispTl(tlBlack(L,_,R)) => ssSeq([dispTl(L),ss(","),dispTl(R)]).
  } in {.
    disp(L) => ssSeq([ss("["),dispTl(L),ss("]")]).
  .}

  public implementation all e ~~ sequence[tl[e]->>e] => {
    _cons(E,S) => rebalance(tlLeaf(E),S).
    _nil = tlEmpty
  }

  public implementation all e ~~ stream[tl[e] ->> e] => let{
    eof:(tl[e]) => boolean.
    eof(tlEmpty) => .true.
    eof(_) => .false.

    hdlt:(tl[e])=>option[(e,tl[e])].
    hdtl(tlLeaf(E)) => some((E,tlEmpty)).
    hdtl(tlRed(L,_,R)) where (E,Lr)^=hdtl(L) => some((E,rebalance(Lr,R))).
    hdtl(tlEmpty) => .none.
  } in {.
    _eof = eof.
    _hdtl = hdtl.
  .}


  put_by_index(tlEmpty,_,E) => tlLeaf(E).
  put_by_index(tlLeaf(V1),Ix,V2) where Ix>0 => tlRed(tlLeaf(V1),2,tlLeaf(V2)).
  put_by_index(tlLeaf(V1),0,V2) => tlRed(tlLeaf(V2),2,tlLeaf(V1)).
  put_by_index(tlRed(L,W,R),Ix,V) where Ix<tlSize(L) =>
    rebalance(put_by_index(L,Ix,V),R).
  put_by_index(tlRed(L,W,R),Ix,V) where Ix>=tlSize(L) =>
    rebalance(L,put_by_index(R,Ix-W-1,V)).

  remove_by_index:all e ~~ (tl[e],integer) => tl[e].
  remove_by_index(tlEmpty,_) => tlEmpty.
  remove_by_index(tlRed(L,Lw,R),Ix) where Ix<Lw =>
    rebalance(remove_by_index(L,Ix),R).
  remove_by_index(tlRed(L,Lw,R),Ix) where Ix>=Lw =>
    rebalance(L,remove_by_index(R,Ix-Lw)).

}
    
  
  
