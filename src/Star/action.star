star.action{
  import star.core.
  import star.monad.

  public all a,e ~~ action[e,a] ::= done(a) | delay(()=>action[e,a]) | err(e).

  public implementation all e ~~ monad[action[e]] => {.
    (err(E) >>= _) => err(E).
    (done(A) >>= F) => delay(()=>F(A)).
    (delay(G) >>= F) => delay(()=>G()>>=F).

    return X => delay(()=>done(X)).
  .}

  public implementation execution[action[string]->>string] => {
    _perform(done(X)) => X.
    _perform(delay(F)) => _perform(F()).

    _handle(done(X),_) => done(X).
    _handle(delay(A),E) => _handle(A(),E).
    _handle(err(X),E) => E(X).

    _raise(S) => err(S).
  }

  public (:=):all a ~~ (ref a,a) => action[(),()].
  (:=)(L,V) => delay(() where _ .= _assign(L,V) => done(())).

  public (!):all a ~~ (ref a)=>a.
  (!)(V) => _get(V).
}
