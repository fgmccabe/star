star.action{
  import star.core.
  import star.monad.

  public all a,e ~~ action[e,a] ::= action(()=>result[e,a]).

  public all e,a ~~ result[e,a] ::= ok(a) | err(e).

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

  public implementation all e,a ~~ display[e],display[a] |: display[result[e,a]] => {.
    disp(ok(O)) => "ok: $(O)".
    disp(err(E)) => "err: $(E)".
  .}

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

  public logMsg:all e ~~ (string)=>result[e,()].
  logMsg(Msg) => do{
    _ .= _logmsg(_str_fltn(Msg));
    valis ()
  }

  public showMsg:all e ~~ (string)=>result[e,()].
  showMsg(Msg) => do{
    _ .= _show(_str_fltn(Msg));
    valis ()
  }
}
