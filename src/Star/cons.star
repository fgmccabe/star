star.cons{
  import star.core.
  import star.arith.
  import star.iterable.
  import star.monad.
  import star.coerce.

  public implementation all x ~~ equality[x] |: equality[cons[x]] => {.
    L1 == L2 => smList(L1,L2).
  .}

  smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
  smList(.nil,.nil) => .true.
  smList(cons(x,xr),cons(y,yr)) => x==y && smList(xr,yr).
  smList(_,_) default => .false.

  public implementation all x ~~ hash[x] |: hash[cons[x]] => {
    hash(L) => cHash(L,0).
  }

  cHash:all x ~~ hash[x] |: (cons[x],integer) => integer.
  cHash(.nil,X) => X.
  cHash(cons(x,xr),H) => cHash(xr,(H+hash(x))*37).

  public implementation all x ~~ comp[x],equality[x] |: comp[cons[x]] => let{
    consLess(.nil,cons(_,_)) => .true.
    consLess(cons(H1,T1),cons(H2,T2)) where H1<H2 => .true.
    consLess(cons(H,T1),cons(H,T2)) => consLess(T1,T2).
    consLess(_,_) default => .false.

    consGe(L1,L2) => !consLess(L2,L1).
  } in {. (<) = consLess. (>=) = consGe .}

  -- stream & sequence contracts
  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof(.nil) => .true.
    _eof(cons(_,_)) => .false.
    
    _hdtl(cons(H,T)) => some((H,T)).
    _hdtl(.nil) => .none.
  }

  public implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => cons(E,S).
    _nil = .nil.
  }

  public implementation all e ~~ sizeable[cons[e]] => {
    size(L) => consLength(L,0).
    isEmpty(.nil) => .true.
    isEmpty(_) default => .false.
  }

  consLength:all e ~~ (cons[e],integer) => integer.
  consLength(.nil,Ln) => Ln.
  consLength(cons(_,T),Ln) => consLength(T,Ln+1).

  last:all e ~~ (cons[e]) => (cons[e],e).
  last(cons(X,.nil)) => (.nil,X).
  last(cons(X,Y)) where (L,E) .= last(Y) => (cons(X,L),E).

  public implementation all x ~~ concat[cons[x]] => {
    X++Y => concat(X,Y).
  }

  concat: all e ~~ (cons[e],cons[e])=>cons[e].
  concat(.nil,Y) => Y.
  concat(cons(E,X),Y) => cons(E,concat(X,Y)).

  public multicat : all e ~~ (cons[cons[e]]) => cons[e].
  multicat(.nil) => .nil.
  multicat(cons(H,T)) => concat(H,multicat(T)).

  public implementation all x ~~ reversible[cons[x]] => {
    reverse(L) => rev(L,.nil).

    private rev:(cons[x],cons[x])=>cons[x].
    rev(.nil,R) => R.
    rev(cons(E,L),R) => rev(L,cons(E,R)).
  }

  public implementation all x ~~ head[cons[x]->>x] => {
    head(cons(E,_)) => some(E).
    head(.nil) => .none.

    tail(cons(_,T)) => some(T).
    tail(.nil) => .none.
  }

  -- Implement iteration of executions over a cons list
  public implementation all t ~~ iter[cons[t]->>t] => {
    _iter(.nil,St,_) => St.
    _iter(cons(H,T),St,Fn) => _sequence(St,(SS)=>_iter(T,Fn(H,SS),Fn)).
  }

/*  -- Implement indexed access
  public implementation all t ~~ indexed[cons[t]->>integer,t] => let{
    indexCons:(cons[t],integer)=>option[t].
    indexCons(.nil,_) => .none.
    indexCons(cons(E,_),0) => some(E).
    indexCons(cons(_,T),Ix) where Ix>0 => indexCons(T,Ix-1).

    putCons:(cons[t],integer,t) => cons[t].
    putCons(.nil,_,V) => cons(V,.nil).
    putCons(cons(_,L),0,V) => cons(V,L).
    putCons(cons(X,L),Ix,V) where Ix>0 => cons(X,putCons(L,Ix-1,V)).

    remCons:(cons[t],integer)=>cons[t].
    remCons(.nil,_) => .nil.
    remCons(cons(_,L),0) => L.
    remCons(cons(E,L),Ix) where Ix>0 => cons(E,remCons(L,Ix-1)).
    
  } in {
    _index(C,Ix) => indexCons(C,Ix).

    _put(C,Ix,V) => putCons(C,Ix,V).

    _remove(C,Ix) => remCons(C,Ix).

    _empty = .nil.
  }
*/
  public implementation all e ~~ display[e] |: display[cons[e]] => let{
    consDisp(.nil) => ss("").
    consDisp(cons(X,.nil)) => disp(X).
    consDisp(cons(X,R)) => ssSeq([disp(X), ss(","), consDisp(R)]).
  } in {
    disp(L) => ssSeq([ss("["), consDisp(L),ss("]")]).
  }

  public implementation functor[cons] => let{
    fm:all a,b ~~ ((a)=>b,cons[a])=>cons[b].
    fm(_,.nil) => .nil.
    fm(f,cons(H,T)) => cons(f(H),fm(f,T))
  } in {.
    fmap = fm.
    C <$ L => fm((_)=>C,L).
  .}

  public implementation monad[cons] => {
    (return X) => cons(X,.nil).
    (XS >>= F) => multicat(fmap(F,XS)).
  }  
}
