star.action{
  import star.core.
  import star.iterable.
  import star.monad.
  import star.coerce.

  public all a,e ~~ action[e,a] ::= done(a) | delay(()=>action[e,a]) | err(e).

  public implementation all e ~~ monad[action[e]] => {.
    (err(E) >>= _) => err(E).
    (done(A) >>= F) => delay(()=>F(A)).
    (delay(G) >>= F) => delay(()=>G()>>=F).

    return X => delay(()=>done(X)).
  .}

  public implementation all e ~~ execution[action[e]->>e] => {
    _perform(done(X)) => X.
    _perform(delay(F)) => _perform(F()).

    _return(X) => delay(()=>done(X)).

    _sequence(err(E),_) => err(E).
    _sequence(done(A),F) => delay(()=>F(A)).
    _sequence(delay(G),F) => delay(()=>_sequence(G(),F)).

    _handle(done(X),_) => done(X).
    _handle(delay(A),E) => _handle(A(),E).
    _handle(err(X),E) => E(X).

    _raise(S) => err(S).
  }

  public implementation all e ~~ coercion[option[e],action[(),e]] => {
    _coerce(none) => err(()).
    _coerce(some(X)) => done(X).
  }

  public implementation all e,t ~~ coercion[action[e,t],iterState[e,t]] => {
    _coerce(done(X)) => continueWith(X).
    _coerce(err(E)) => abortIter(E).
    _coerce(delay(F)) => _coerce(F()).
  }

  public implementation all e,t ~~ coercion[iterState[e,t],action[e,t]] => {
    _coerce(abortIter(E)) => err(E).
    _coerce(noMore(X)) => done(X).
    _coerce(continueWith(X)) => done(X).
  }

  public (:=):all a ~~ (ref a,a) => action[(),()].
  (:=)(L,V) => delay(() where _ .= _assign(L,V) => done(())).

  public (!):all a ~~ (ref a)=>a.
  (!)(V) => _get(V).

  public (!!):all a ~~ (a)=>ref a.
  !! E => _cell(E).
}
