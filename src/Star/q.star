star.q{
  import star.core.
  import star.cons.
  import star.arith.
  import star.collection.

  public all e ~~ qc[e] ::= qc(cons[e],cons[e]).

  public implementation all x ~~ equality[x] |: equality[qc[x]] => {.
    qc(F1,B1) == qc(F2,B2) => smQ(F1,B1,F2,B2).
  .}

  smQ: all x ~~ equality[x] |: (cons[x],cons[x],cons[x],cons[x]) => boolean.
  smQ(nil,B1,nil,B2) => smList(B1,B2).
  smQ(nil,B1,F2,B2) => smQ(reverse(B1),nil,F2,B2).
  smQ(F1,B1,nil,B2) => smQ(F1,B1,reverse(B2),nil).
  smQ(cons(H1,T1),B1,cons(H2,T2),B2) => H1==H2 && smQ(T1,B1,T2,B2).

  smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
  smList(nil,nil) => true.
  smList(cons(x,xr),cons(y,yr)) => x==y && smList(xr,yr).

  -- stream & sequence contracts
  public implementation all x ~~ stream[qc[x] ->> x] => {
    _eof(qc(nil,nil)) => true.
    _eof(_) default => false.

    _hdtl(qc(cons(H,T),B)) => some((H,qc(T,B))).
    _hdtl(qc(nil,nil)) => none.
    _hdtl(qc(nil,B)) => _hdtl(qc(reverse(B),nil)).

    _back(qc(F,cons(H,T))) => some((qc(F,T),H)).
    _back(qc(nil,nil)) => none.
    _back(qc(F,nil)) => _back(qc(nil,reverse(F))).
  }

  public implementation all x ~~ sequence[qc[x] ->> x] => {
    _cons(E,qc(F,B)) => qc(cons(E,F),B).

    _apnd(qc(F,B),E) => qc(F,cons(E,B)).

    _nil = qc(nil,nil).
  }

  public implementation all x ~~ concat[qc[x]] => {.
    qc(F1,B1)++qc(F2,B2) => qc(F1++reverse(B1),B2++reverse(F2)).
  .}

  public implementation all x ~~ reversible[qc[x]] => {
    reverse(qc(F,B)) => qc(B,F).
  }

  public implementation all e ~~ folding[qc[e]->>e] => {
    foldRight(F,U,qc(nil,nil)) => U.
    foldRight(F,U,qc(nil,B)) => foldLeftR(F,U,B).
    foldRight(F,U,qc(cons(H,T),B)) => F(H,foldRight(F,U,qc(T,B))).

    foldLeftR(F,U,nil) => U.
    foldLeftR(F,U,cons(H,T)) => foldLeftR(F,F(H,U),T).

    foldLeft(F,U,qc(nil,nil)) => U.
    foldLeft(F,U,qc(nil,B)) => foldRightR(F,U,B).
    foldLeft(F,U,qc(cons(H,T),B)) => foldLeft(F,F(U,H),qc(T,B)).

    foldRightR(F,U,nil) => U.
    foldRightR(F,U,cons(H,T)) => F(foldRightR(F,U,T),H).
  }

}
