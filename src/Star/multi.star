star.multi{
  import star.

  -- Similar to the ss structure.
  
  public multi[e] ::= .single(e) | .multi(cons[multi[e]]) | .null.

  public implementation all e ~~ build[multi[e]->>e] => {
    _push(E,.null) => .single(E).
    _push(E,.single(X)) => .multi(.cons(.single(E),.cons(.single(X),.nil))).
    _push(E,.multi(L)) => .multi(.cons(.single(E),L)).

    _null = .null
  }

  public implementation all e ~~ sequence[multi[e]->>e] => {
    _cons(E,.null) => .single(E).
    _cons(E,.single(X)) => .multi(.cons(.single(E),.cons(.single(X),.nil))).
    _cons(E,.multi(L)) => .multi(.cons(.single(E),L)).

    _nil = .null
  }

  private mform:all e ~~ (cons[multi[e]])=>multi[e].
  mform(.nil) => .null.
  mform(.cons(M,.nil)) => M.
  mform(C) => .multi(C).

  public implementation all e ~~ stream[multi[e]->>e] => {.
    _eof(.null) => .true.
    _eof(.multi(.nil)) => .true.
    _eof(_) default => .false.

    _hdtl(.single(X)) => .some((X,.null)).
    _hdtl(.multi(.cons(H,T))) where (F,R) ?= _hdtl(H) =>
      .some((F,push(R,mform(T)))).

    private push(.null,X) => X.
    push(M,X) => .multi(.cons(M,.cons(X,.nil))).
  .}

  public implementation all e ~~ concat[multi[e]] => let{
    conc(.null,M) => M.
    conc(M,.null) => M.
    conc(A,.multi(L)) => .multi([A,..L]).
    conc(A,.single(X)) => .multi([A,.single(X)]).
  } in {
    L++R => conc(L,R).
    _multicat(.nil) => .null.
    _multicat(L) => .multi(L).
  }

  public implementation all e ~~ glue[multi[e]->>e] => {
    prepend(A,M) => .single(A)++M.
    append(M,E) => M++.single(E)
  }

  public implementation all e ~~ coercion[multi[e],cons[e]] => let{.
    flatten(.null,S) => S.
    flatten(.single(X),S) => [X,..S].
    flatten(.multi(L),S) => multiFlatten(L,S).

    multiFlatten(.nil,S) => S.
    multiFlatten(.cons(H,T),S) => flatten(H,multiFlatten(T,S)).
  .} in {
    _coerce(M) => .some(flatten(M,.nil))
  }

  public implementation all e ~~ display[e] |: display[multi[e]] => {
    disp(M) => disp(M::cons[e])
  }

  public implementation all e,f ~~ mapping[multi->>e,f] => let{.
    mapOver(.null,F) => .null.
    mapOver(.single(X),F) => .single(F(X)).
    mapOver(.multi(L),F) => mform(multiMapOver(L,F)).

    multiMapOver(.nil,F) => .nil.
    multiMapOver(.cons(H,T),F) => .cons(mapOver(H,F),multiMapOver(T,F)).
  .} in {
    (M//F) => mapOver(M,F)
  }

  public implementation all e ~~ sizeable[multi[e]] => {
    size(M) => multiSize(M).
    isEmpty(M) => multiIsEmpty(M)
  }

  multiSize:all e ~~ (multi[e]) => integer.
  multiSize(.null) => 0.
  multiSize(.single(_)) => 1.
  multiSize(.multi(L)) => multiListSize(L).

  multiListSize(.nil) => 0.
  multiListSize(.cons(H,T)) => multiSize(H)+multiListSize(T).

  multiIsEmpty:all e ~~ (multi[e]) => boolean.
  multiIsEmpty(.null) => .true.
  multiIsEmpty(_) default => .false.

}
