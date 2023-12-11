star.index{
  import star.core.
  import star.arith.
  import star.cons.

  -- Implement indexed access
  public implementation all t ~~ indexed[cons[t]->>integer,t] => let{.
    indexCons:(cons[t],integer)=>option[t].
    indexCons(.nil,_) => .none.
    indexCons(.cons(E,_),0) => .some(E).
    indexCons(.cons(_,T),Ix) where Ix>0 => indexCons(T,Ix-1).

    putCons:(cons[t],integer,t) => cons[t].
    putCons(.nil,_,V) => .cons(V,.nil).
    putCons(.cons(_,L),0,V) => .cons(V,L).
    putCons(.cons(X,L),Ix,V) where Ix>0 => .cons(X,putCons(L,Ix-1,V)).

    remCons:(cons[t],integer)=>cons[t].
    remCons(.nil,_) => .nil.
    remCons(.cons(_,L),0) => L.
    remCons(.cons(E,L),Ix) where Ix>0 => .cons(E,remCons(L,Ix-1)).
    
 .} in {
    _index(C,Ix) => indexCons(C,Ix).

    _put(C,Ix,V) => putCons(C,Ix,V).

    _remove(C,Ix) => remCons(C,Ix).

    _empty = .nil.
  }

}
