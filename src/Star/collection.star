star.collection{
  import star.core.
  import star.option.
  import star.cons.
  import star.arith.

  public contract all c,e ~~ folding[c->>e] ::= {
    foldRight:all x ~~ (((e,x)=>x),x,c) => x.
    foldLeft:all x ~~ (((e,x)=>x),x,c) => x.
  }

  public contract all t,e ~~ reduce[t->>e] ::= {
    reducer:all a ~~ ((e,a)=>a) => (t,a) => a.
    reducel:all a ~~ ((e,a)=>a) => (t,a) => a.
  }

  public contract all c,e ~~ filter[c->>e] ::= {
    (^/):(c,(e)=>boolean) => c.
  }

  public contract all c,e ~~ search[c->>e] ::= {
    search:(c,(e)=>boolean) => option[e].
  }

  public contract all m/1 ~~ mapping[m] ::= {
    (//):all e,f ~~ (m[e],(e)=>f) => m[f].
  }

  public contract all m/2 ~~ ixmap[m] ::= {
    (///):all k,e,f ~~ (m[k,e],(k,e)=>f)=>m[k,f].
  }

  public contract all k,v,m ~~ ixfilter[m->>k,v] ::= {
    (^//):(m,(k,v)=>boolean) => m.
  }

  public contract all c,k,v ~~ ixfold[c->>k,v] ::= {
    ixRight:all x ~~ (((k,v,x)=>x),x,c) => x.
    ixLeft:all x ~~ (((k,v,x)=>x),x,c) => x.
  }

  public contract all k,e ~~ membership[k->>e] ::= {
    (\+):(k,e)=>k.
    (\-):(k,e)=>k.
    (.<.):(e,k)=>boolean.
  }

  public contract all k ~~ setops[k] ::= {
    (\/) : (k,k)=>k.
    (/\) : (k,k)=>k.
    (\) : (k,k)=>k.
  }

  public contract all r,t ~~ updateable[r->>t] ::= {
    _extend:(r,t)=>r.
    _merge:(r,r) => r.
    _delete:(r,(t)=>boolean) => r.
    _update:(r,(t)=>option[t]) => r.
  }

  public interleave: all c,t ~~ stream[c->>t],sequence[c->>t] |: (c,t) => c.
  interleave([],_) => [].
  interleave([F,..R],I) => let{
    inter([]) => [].
    inter([E,..L]) => [I,E,..inter(L)].
  } in [F,..inter(R)].

  public implementation mapping[cons] => {
    (L//F) => mapOverList(L,F).

    private mapOverList:all e,f ~~ (cons[e],(e)=>f)=>cons[f].
    mapOverList(.nil,_) => .nil.
    mapOverList(cons(H,T),F) => cons(F(H),mapOverList(T,F)).
  }

  public implementation all e ~~ folding[cons[e]->>e] => {
    foldRight(F,U,.nil) => U.
    foldRight(F,U,cons(H,T)) => F(H,foldRight(F,U,T)).

    foldLeft(F,U,.nil) => U.
    foldLeft(F,U,cons(H,T)) => foldLeft(F,F(H,U),T).
  }

  public implementation all e ~~ reduce[cons[e]->>e] => {
    reducer(F) => (L,U) => foldRight(F,U,L).
    reducel(F) => (L,U) => foldLeft(F,U,L).
  }

  public implementation all e ~~ ixfold[cons[e] ->> integer,e] => {.
    ixRight(F,Z,L) => let{
      fdr(.nil,_) => Z.
      fdr(cons(H,T),Ix) => F(Ix,H,fdr(T,Ix+1)).
    } in fdr(L,0).

    ixLeft(F,Z,L) => let{
      fdl(.nil,Ix,Ac) => Ac.
      fdl(cons(H,T),Ix,Ac) => fdl(T,Ix+1,F(Ix,H,Ac)).
    } in fdl(L,0,Z).
  .}

  public implementation all e ~~ search[cons[e]->>e] => {
    search(L,F) => searchList(L,F).

    private searchList([],_) => .none.
    searchList([E,..L],F) where F(E) => some(E).
    searchList([_,..L],F) => searchList(L,F).
  }

  public implementation all t ~~ filter[cons[t]->>t] => let{
    filter([],F) => [].
    filter([E,..Es],F) where F(E) => [E,..filter(Es,F)].
    filter([_,..Es],F) => filter(Es,F).
  } in {
    (LL ^/ F) => filter(LL,F)
  }

  public implementation all e ~~ equality[e] |: membership[cons[e]->>e] => let{
    _mem(K,cons(K,_)) => .true.
    _mem(K,cons(_,L)) => _mem(K,L).
    _mem(_,.nil) => .false.

    _rem(_,.nil) => .nil.
    _rem(K,cons(K,L)) => L.
    _rem(K,cons(E,L)) => cons(E,_rem(K,L)).
  } in {.
    L\+E where _mem(E,L) => L.
    L\+E => cons(E,L).
    L\-E => _rem(E,L).
    E .<. L => _mem(E,L)
  .}

  public implementation all e ~~ equality[e] |: setops[cons[e]] => let{
    merge(.nil,C) => C.
    merge(cons(H,T),C) where H.<.C => merge(T,C).
    merge(cons(H,T),C) default => [H,..merge(T,C)].

    intersect(.nil,C) => .nil.
    intersect(cons(H,T),C) where H.<.C => [H,..intersect(T,C)].
    intersect(cons(H,T),C) default => intersect(T,C).

    diff(.nil,C) => .nil.
    diff(cons(H,T),C) where H.<.C => diff(T,C).
    diff(cons(H,T),C) default => [H,..diff(T,C)].
  } in {.
    L1 \/ L2 => merge(L1,L2).
    L1 /\ L2 => intersect(L1,L2).
    L1 \ L2 => diff(L1,L2)
  .}

  public iota: all c ~~ sequence[c->>integer] |: (integer,integer)=>c.
  iota(Mx,Mx) => [].
  iota(Ix,Mx) where Ix<Mx => [Ix,..iota(Ix+1,Mx)].

}
