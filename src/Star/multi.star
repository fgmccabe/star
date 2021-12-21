star.multi{
  import star.

  -- Similar to the ss structure.
  
  public multi[e] ::= single(e) | multi(cons[multi[e]]) | .null.

  public implementation all e ~~ sequence[multi[e]->>e] => {
    _cons(E,.null) => single(E).
    _cons(E,single(X)) => multi(cons(single(E),cons(single(X),.nil))).
    _cons(E,multi(L)) => multi(cons(single(E),L)).

    _nil = .null
  }

  public implementation all e ~~ stream[multi[e]->>e] => {
    _eof(.null) => .true.
    _eof(multi(.nil)) => .true.
    _eof(_) default => .false.

    _hdtl(single(X)) => some((X,.null)).
    _hdtl(multi(cons(H,T))) where (F,R) ^= _hdtl(H) =>
      some((F,push(R,multi(T)))).

    private push(.null,X) => X.
    push(M,X) => multi(cons(M,cons(X,.nil))).
  }

  public implementation all e ~~ concat[multi[e]] => let{
    conc(.null,M) => M.
    conc(M,.null) => M.
    conc(A,multi(L)) => multi([A,..L]).
    conc(A,single(X)) => multi([A,single(X)]).
  } in {
    L++R => conc(L,R).
    _multicat(L) => multi(L).
  }

  public implementation all e ~~ glue[multi[e]->>e] => {
    prepend(A,M) => single(A)++M.
    append(M,E) => M++single(E)
  }

  public implementation all e ~~ coercion[multi[e],cons[e]] => let{.
    flatten(.null,S) => S.
    flatten(single(X),S) => [X,..S].
    flatten(multi(L),S) => multiFlatten(L,S).

    multiFlatten(.nil,S) => S.
    multiFlatten(cons(H,T),S) => flatten(H,multiFlatten(T,S)).
  .} in {
    _coerce(M) => some(flatten(M,.nil))
  }

  public implementation all e ~~ display[e] |: display[multi[e]] => {
    disp(M) => disp(M::cons[e])
  }

  public implementation mapping[multi] => let{.
    mapOver(.null,F) => .null.
    mapOver(single(X),F) => single(F(X)).
    mapOver(multi(L),F) => multi(multiMapOver(L,F)).

    multiMapOver(.nil,F) => .nil.
    multiMapOver(cons(H,T),F) => cons(mapOver(H,F),multiMapOver(T,F)).
  .} in {
    (M//F) => mapOver(M,F)
  }

}
