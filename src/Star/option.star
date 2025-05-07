star.option{
  import star.core.
  import star.coerce.
  import star.arith.
  import star.monad.
  import star.cons.
  import star.strings.

  -- display optional values
  public implementation all x ~~ display[x] |: display[option[x]] => {
    disp(O) => dispOptional(O).
  }

  private dispOptional:all x ~~ display[x] |: (option[x]) => string.
  dispOptional(.none) => ".none".
  dispOptional(.some(X)) => ".some($(X))".

  public implementation all x ~~ equality[x] |: equality[option[x]] => {
    X == Y => optionEqual(X,Y).
  }

  private optionEqual:all x ~~ equality[x] |: (option[x],option[x]) => boolean.
  optionEqual(.some(A),.some(B)) => A==B.
  optionEqual(.none,.none) => .true.
  optionEqual(_,_) => .false.

  public implementation all x ~~ hashable[x] |: hashable[option[x]] => {
    hash(.some(X)) => hash("?")*37+hash(X).
    hash(.none) => hash("none").
  }

  public implementation all e,f ~~ coercion[e,f] |: coercion[option[e],option[f]] => {
    _coerce(.some(X)) => .some(_coerce(X)).
    _coerce(.none) => .none.
  }

  public deflt:all a~~(option[a],()=>a)=>a.
  deflt(.some(X),_) => X.
  deflt(.none,F) => F().

  public implementation monad[option] => {
    return X => .some(X).
    (.none >>= _) => .none.
    (.some(X) >>= F) => F(X)
  }

  public implementation functor[option] => {.
    fmap(_,.none) => .none.
    fmap(F,.some(A)) => .some(F(A)).
  .}

  public _optval:all a~~(option[a])=>a.
  _optval(.some(X)) => X.

  public implementation all e ~~ pull[option[e]->>e,exception] => {
      ? .some(X) => X.
      ? .none => throw .exception("cannot pull from none")
  }

  public implementation all a,b ~~ measured[a->>b] |: measured[option[a]->>option[b]] => {
    [|.none|] => .none.
    [|.some(A)|] => .some([|A|]).
  }
}
