star.action{
  import star.core.
  import star.iterable.
  import star.monad.
  import star.option.
  import star.coerce.
  import star.cons.
  import star.display.

  public all a,e ~~ action[e,a] ::= action(()=>result[e,a]).

  public all e,a ~~ result[e,a] ::= private ok(a) | private err(e).

  public implementation all e ~~ monad[result[e]] => {
    (ok(X) >>= F) => F(X).
    (err(E) >>= _) => err(E).

    return X => ok(X)
  }

  public implementation all e ~~ monad[action[e]] => {
    (action(A) >>= F) => test(A(),F).

    test(ok(A),F) => F(A).
    test(err(E),_) => action(()=>err(E)).

    return X => action(()=>ok(X)).
  }

  public implementation execution[result] => {
    _valof(ok(X)) => X.

    _valis(X) => ok(X).
    _raise(E) => err(E).

    _sequence(ok(X),F) => F(X).
    _sequence(err(E),_) => err(E).

    _handle(ok(X),_) => ok(X).
    _handle(err(E),H) => H(E).
  }

  public implementation execution[action] => let{.
    test(ok(X),F) => F(X).
    test(err(E),_) => action(()=>err(E)).
    
    testE(ok(X),_) => action(()=>ok(X)).
    testE(err(E),H) => H(E).
  .} in {.
    _valof(action(A)) => _valof(A()).

    _valis(X) => action(()=>ok(X)).
    _raise(E) => action(()=>err(E)).

    _sequence(action(A),F) => test(A(),F).
    _handle(action(A),H) => testE(A(),H).
  .}

  public logMsg:all e ~~ (string)=>action[e,()].
  logMsg(Msg) => action{
    _ .= _logmsg(Msg);
    valis ()
  }
}
