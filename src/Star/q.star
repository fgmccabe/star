star.q{
  import star.core.
  import star.cons.
  import star.arith.
  import star.collection.
  import star.coerce.

  public all e ~~ qc[e] ::= .qc(cons[e],cons[e]).

  public implementation all x ~~ equality[x] |: equality[qc[x]] => let{.
    smQ: all x ~~ equality[x] |: (cons[x],cons[x],cons[x],cons[x]) => boolean.
    smQ(.nil,B1,.nil,B2) => smList(B1,B2).
    smQ(.nil,B1,F2,B2) => smQ(reverse(B1),.nil,F2,B2).
    smQ(F1,B1,.nil,B2) => smQ(F1,B1,reverse(B2),.nil).
    smQ(.cons(H1,T1),B1,.cons(H2,T2),B2) => H1==H2 && smQ(T1,B1,T2,B2).

    smList:all x ~~ equality[x] |: (cons[x],cons[x]) => boolean.
    smList(.nil,.nil) => .true.
    smList(.cons(x,xr),.cons(y,yr)) => x==y && smList(xr,yr).
  .} in {
    .qc(F1,B1) == .qc(F2,B2) => smQ(F1,B1,F2,B2).
  }

  -- stream & sequence contracts
  public implementation all x ~~ stream[qc[x] ->> x] => {
    _eof(.qc(.nil,.nil)) => .true.
    _eof(_) default => .false.

    _hdtl(.qc(.cons(H,T),B)) => .some((H,.qc(T,B))).
    _hdtl(.qc(.nil,.nil)) => .none.
    _hdtl(.qc(.nil,B)) => _hdtl(.qc(reverse(B),.nil)).
  }

  public implementation all x ~~ sequence[qc[x] ->> x] => {
    _cons(E,.qc(F,B)) => .qc(.cons(E,F),B).
    _nil = .qc(.nil,.nil).
  }

  public implementation all x ~~ concat[qc[x]] => {
    .qc(F1,B1)++.qc(F2,B2) => .qc(F1++reverse(B1),B2++reverse(F2)).
    _multicat(Qs) => multiq(Qs).
  }

  multiq([]) => .qc(.nil,.nil).
  multiq([Q,..Qs]) => Q++multiq(Qs).

  public implementation all x ~~ reversible[qc[x]] => {
    reverse(.qc(F,B)) => .qc(B,F).
  }

  public implementation all x ~~ sizeable[qc[x]] => {
    size(.qc(L,R)) => size(L)+size(R).
    isEmpty(.qc(.nil,.nil)) => .true.
    isEmpty(_) default => .false.
  }

  public implementation all x ~~ glue[qc[x]->>x] => {
    prepend(X,.qc(F,T)) => .qc(.cons(X,F),T).
    append(.qc(F,T),X) => .qc(F,.cons(X,T)).
  }

  public implementation all e ~~ folding[qc[e]->>e] => {.
    foldRight(F,U,.qc(.nil,.nil)) => U.
    foldRight(F,U,.qc(.nil,B)) => foldLeftR(F,U,B).
    foldRight(F,U,.qc(.cons(H,T),B)) => F(H,foldRight(F,U,.qc(T,B))).

    private foldLeftR(F,U,.nil) => U.
    foldLeftR(F,U,.cons(H,T)) => foldLeftR(F,F(H,U),T).

    foldLeft(F,U,.qc(.nil,.nil)) => U.
    foldLeft(F,U,.qc(.nil,B)) => foldRightR(F,U,B).
    foldLeft(F,U,.qc(.cons(H,T),B)) => foldLeft(F,F(H,U),.qc(T,B)).

    private foldRightR(F,U,.nil) => U.
    foldRightR(F,U,.cons(H,T)) => F(H,foldRightR(F,U,T)).
 .}

  public implementation all e ~~ coercion[cons[e],qc[e]] => {
    _coerce(L) => ?.qc(L,.nil).
  }

  public implementation all e ~~ coercion[qc[e],cons[e]] => {
    _coerce(.qc(F,B)) => ?(F++reverse(B)).
  }

}
