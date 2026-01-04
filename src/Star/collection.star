star.collection{
  import star.core.
  import star.option.
  import star.cons.
  import star.arith.

  public contract all c,e ~~ folding[c->>e] ::= {
    foldRight:all x,xx ~~ (((e,x)=>x throws xx),x,c) => x throws xx.
    foldLeft:all x,xx ~~ (((e,x)=>x throws xx),x,c) => x throws xx.
  }

  public contract all c,e ~~ filter[c->>e] ::= {
    (^/):(c,(e)=>boolean) => c.
  }

  public contract all c,e ~~ searchable[c->>e] ::= {
    search:(c,(e)=>boolean) => option[e].
    replace:(c,(e)=>boolean,e) => c.
  }

  public contract all m/1,e,f ~~ mapping[m->>e,f] ::= {
    (//):all xx ~~ (m[e],(e)=>f throws xx) => m[f] throws xx.
  }

  public contract all m/2 ~~ ixmap[m] ::= {
    (///):all k,e,f,xx ~~ (m[k,e],(k,e)=>f throws xx)=>m[k,f] throws xx.
  }

  public contract all k,v,m ~~ ixfilter[m->>k,v] ::= {
    (^//):(m,(k,v)=>boolean) => m.
  }

  public contract all c,k,v ~~ ixfold[c->>k,v] ::= {
    ixRight:all x,xx ~~ (((k,v,x)=>x throws xx),x,c) => x throws xx.
    ixLeft:all x,xx ~~ (((k,v,x)=>x throws xx),x,c) => x throws xx.
  }

  public contract all c,t ~~ visitor[c->>t] ::= {
    visit:(c,(t){}){}
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

  public interleave: all c,t ~~ stream[c->>t],sequence[c->>t] |= (c,t) => c.
  interleave([],_) => [].
  interleave([F,..R],I) => let{.
    inter([]) => [].
    inter([E,..L]) => [I,E,..inter(L)].
 .} in [F,..inter(R)].

  public implementation all e,f ~~ mapping[cons->>e,f] => let{.
    mapOverList:all x ~~ (cons[e],(e)=>f throws x)=>cons[f] throws x.
    mapOverList(.nil,_) => .nil.
    mapOverList(.cons(H,T),F) => .cons(F(H),mapOverList(T,F)).
  .} in {
    (//) = mapOverList
  }

  public implementation all e ~~ folding[cons[e]->>e] => {.
    foldRight(F,U,.nil) => U.
    foldRight(F,U,.cons(H,T)) => F(H,foldRight(F,U,T)).

    foldLeft(F,U,.nil) => U.
    foldLeft(F,U,.cons(H,T)) => foldLeft(F,F(H,U),T).
  .}

  public implementation all e ~~ visitor[cons[e]->>e] => {.
    visit(.nil,_) do {}.
    visit(.cons(E,L),P) do{
      P(E);
      visit(L,P)
    }
  .}

  public implementation all e,f ~~ mapping[option->>e,f] => {
    (.none // _) => .none.
    (.some(X) // F) => .some(F(X))
  }

  public implementation all x ~~ folding[option[x]->>x] => let{
    fold:all a,xx ~~ ((x,a)=>a throws xx,a,option[x])=>a throws xx.
    fold(F,A,.some(X)) => F(X,A).
    fold(F,A,.none) => A.
  } in {
    foldRight = fold.
    foldLeft = fold
  }

  public implementation all e ~~ ixfold[cons[e] ->> integer,e] => {
    ixRight(F,Z,L) => let{.
      fdr:(cons[e], integer) => x throws xx.
      fdr(.nil,_) => Z.
      fdr(.cons(H,T),Ix) => F(Ix,H,fdr(T,Ix+1)).
   .} in fdr(L,0).

    ixLeft(F,Z,L) => let{.
      fdl:(cons[e], integer, x) => x throws xx.
      fdl(.nil,Ix,Ac) => Ac.
      fdl(.cons(H,T),Ix,Ac) => fdl(T,Ix+1,F(Ix,H,Ac)).
   .} in fdl(L,0,Z).
  }

  public implementation all e ~~ searchable[cons[e]->>e] => {.
    search(L,F) => searchList(L,F).
    replace(L,F,R) => let{.
      repl([])=>[].
      repl([E,..Es]) where F(E) => [R,..repl(Es)].
      repl([E,..Es]) => [E,..repl(Es)].
    .} in repl(L).

    private searchList([],_) => .none.
    searchList([E,..L],F) where F(E) => .some(E).
    searchList([_,..L],F) => searchList(L,F).
 .}

  public implementation all t ~~ filter[cons[t]->>t] => let{.
    filter(L,F) => case L in {
      | [] => []
      | [E,..Es] where F(E) => [E,..filter(Es,F)]
      | [_,..Es] => filter(Es,F)
    }
 .} in {
    (LL ^/ F) => filter(LL,F)
  }

  public implementation all e ~~ equality[e] |= membership[cons[e]->>e] => let{.
    _mem(K,Ls) => case Ls in {
      | .cons(K,_) => .true
      | .cons(_,L) => _mem(K,L)
      | .nil => .false
    }

    _rem(K,Ls) => case Ls in {
      | .nil => .nil
      | .cons(K,L) => L
      | .cons(E,L) => .cons(E,_rem(K,L))
    }
  .} in {
    L\+E where _mem(E,L) => L.
    L\+E => .cons(E,L).
    L\-E => _rem(E,L).
    E .<. L => _mem(E,L)
  }

  public implementation all e ~~ equality[e] |= setops[cons[e]] => let{.
    merge(L,R) => case L in {
      | .nil => R
      | .cons(H,T) where H.<.R => merge(T,R)
      | .cons(H,T) default => [H,..merge(T,R)]
    }

    intersect(L,C) => case L in {
      | .nil => .nil
      | .cons(H,T) where H.<.C => [H,..intersect(T,C)]
      | .cons(H,T) default => intersect(T,C)
    }

    diff(L,C) => case L in {
      | .nil => .nil
      | .cons(H,T) where H.<.C => diff(T,C)
      | .cons(H,T) default => [H,..diff(T,C)]
    }
  .} in {
    L1 \/ L2 => merge(L1,L2).
    L1 /\ L2 => intersect(L1,L2).
    L1 \ L2 => diff(L1,L2)
  }

  public iota: all c ~~ sequence[c->>integer] |= (integer,integer)=>c.
  iota(Ix,Mx) => Ix==Mx ?? [] || [Ix,..iota(Ix+1,Mx)].

}
