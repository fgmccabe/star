star.cons{
  import star.core.
  import star.arith.
  -- import star.collection.

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

  -- stream contract
  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof() <= (nil).
    _hdtl(H,T) <= (cons(H,T)).

    _cons(E,S) => cons(E,S).
    _apnd(S,E) => S++[E].

    _back(T,E) <= (X) where last(X) =. (T,E).
    _nil = nil.
  }

  last:all e ~~ (cons[e]) => (cons[e],e).
  last(cons(X,nil)) => (nil,X).
  last(cons(X,Y)) where last(Y) =. (L,E)  => (cons(X,L),E).

  public implementation all x ~~ concat[cons[x]] => {
    X++Y => concat(X,Y).

    concat: all e ~~ (cons[e],cons[e])=>cons[e].
    concat(nil,Y) => Y.
    concat(cons(E,X),Y) => cons(E,concat(X,Y)).
  }
}
