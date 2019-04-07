star.cons{
  import star.core.
  import star.arith.
  import star.collection.
  import star.iterable.
  import star.monad.
  import star.lists.
  import star.coerce.

  public all t ~~ cons[t] ::= nil | cons(t,cons[t]).

  public implementation all x ~~ equality[x] |: equality[cons[x]] => {.
    L1 == L2 => smList(L1,L2).
  .}

  smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
  smList(nil,nil) => true.
  smList(cons(x,xr),cons(y,yr)) => x==y && smList(xr,yr).

  public implementation all x ~~ hash[x] |: hash[cons[x]] => {
    hash(L) => cHash(L,0).
  }

  cHash:all x ~~ hash[x] |: (cons[x],integer) => integer.
  cHash(nil,X) => X.
  cHash(cons(x,xr),H) => cHash(xr,(H+hash(x))*37).

  public implementation all x ~~ comp[x],equality[x] |: comp[cons[x]] => let{
    consLess(nil,cons(_,_)) => true.
    consLess(cons(H1,T1),cons(H2,T2)) where H1<H2 => true.
    consLess(cons(H,T1),cons(H,T2)) => consLess(T1,T2).
    consLess(_,_) default => false.

    consGe(L1,L2) => \+ consLess(L2,L1).
  } in {. (<) = consLess. (>=) = consGe .}

  -- stream & sequence contracts
  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof(nil) => true.
    _eof(cons(_,_)) => false.

    _hdtl(cons(H,T)) => some((H,T)).
    _hdtl(nil) => none.

    _back(nil) => none.
    _back(X) => some(last(X)).
  }

  public implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => cons(E,S).
    _apnd(S,E) => concat(S,cons(E,nil)).

    _nil = nil.
  }

  public implementation all e ~~ sizeable[cons[e]] => {
    size(L) => consLength(L,0).
    isEmpty(nil) => true.
    isEMpty(_) default => false.
  }

  consLength:all e ~~ (cons[e],integer) => integer.
  consLength(nil,Ln) => Ln.
  consLength(cons(_,T),Ln) => consLength(T,Ln+1).

  last:all e ~~ (cons[e]) => (cons[e],e).
  last(cons(X,nil)) => (nil,X).
  last(cons(X,Y)) where last(Y) =. (L,E)  => (cons(X,L),E).

  public implementation all x ~~ concat[cons[x]] => {
    X++Y => concat(X,Y).
  }

  concat: all e ~~ (cons[e],cons[e])=>cons[e].
  concat(nil,Y) => Y.
  concat(cons(E,X),Y) => cons(E,concat(X,Y)).

  public implementation all x ~~ reversible[cons[x]] => {
    reverse(L) => rev(L,nil).

    rev(nil,R) => R.
    rev(cons(E,L),R) => rev(L,cons(E,R)).
  }

  public implementation all x ~~ head[cons[x]->>x] => {
    head(cons(E,_)) => some(E).
    head(nil) => none.

    tail(cons(_,T)) => some(T).
    tail(nil) => none.
  }

  public implementation mapping[cons] => {
    L//F => mapOverList(L,F).

    mapOverList:all e,f ~~ (cons[e],(e)=>f)=>cons[f].
    mapOverList(nil,_) => nil.
    mapOverList(cons(H,T),F) => cons(F(H),mapOverList(T,F)).
  }

  public implementation all e ~~ folding[cons[e]->>e] => {
    foldRight(F,U,nil) => U.
    foldRight(F,U,cons(H,T)) => F(H,foldRight(F,U,T)).

    foldLeft(F,U,nil) => U.
    foldLeft(F,U,cons(H,T)) => foldLeft(F,F(U,H),T).
  }

  public implementation all e ~~ reduce[cons[e]->>e] => {
    reducer(F) => (L,U) => foldRight(F,U,L).
    reducel(F) => (U,L) => foldLeft(F,U,L).
  }

  -- Implement iteration of executions over a cons list
  public implementation all t ~~ iter[cons[t]->>t] => {
    _iter(nil,St,_) => St.
    _iter(cons(H,T),St,Fn) => _sequence(St,(SS)=>_iter(T,Fn(H,SS),Fn)).
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => {.
    disp(L) => consDisp(L).
  .}

  consDisp:all e ~~ display[e] |: (cons[e]) => ss.
  consDisp(nil) => ss(".").
  consDisp(cons(E,T)) => ssSeq([disp(E),ss(": "),consDisp(T)]).

  public multicat : all e ~~ (cons[cons[e]]) => cons[e].
  multicat(nil) => nil.
  multicat(cons(H,T)) => concat(H,multicat(T)).

  public implementation functor[cons] => let{
    fm(_,nil) => nil.
    fm(f,cons(H,T)) => cons(f(H),fm(f,T))
  } in {.
    fmap = fm
  .}

  public implementation monad[cons] => {
    (return X) => cons(X,nil).
    (XS >>= F) => multicat(fmap(F,XS)).
  }

  public implementation all e ~~ coercion[cons[e],list[e]] => let{
    mkList(nil) => [].
    mkList(cons(H,T)) => [H,..mkList(T)].
  } in {.
    _coerce = mkList
  .}
}
