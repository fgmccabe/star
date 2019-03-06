star.cons{
  import star.core.
  import star.arith.
  import star.collection.
  import star.iterable.
  import star.monad.
  import star.lists.

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

  public implementation all e ~~ iterable[cons[e]->>e] => {
    _iterate(Lst,Fn,Init) => iterateOverCons(Lst,Fn,Init).

    iterateOverCons(nil,_,St) => St.
    iterateOverCons(_,_,abortIter(E)) => abortIter(E).
    iterateOverCons(_,_,noMore(E)) => noMore(E).
    iterateOverCons(cons(H,T),Fn,St) => iterateOverCons(T,Fn,Fn(H,St)).
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => {.
    disp(L) => consDisp(L).
  .}

  consDisp:all e ~~ display[e] |: (cons[e]) => ss.
  consDisp(nil) => ss(".").
  consDisp(cons(E,T)) => ssSeq([disp(E),ss(": "),consDisp(T)]).
}
