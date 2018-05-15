star.action{
  import star.core.
  import star.monad.

  public all a ~~ action[a] ::= done(a) | delay(()=>action[a]) | err(string).

  public implementation monad[action] => {
    (err(E) >>= _) => err(E).
    (done(A) >>= F) => delay(()=>F(A)).
    (delay(G) >>= F) => delay(()=>G()>>=F).

    return X => delay(()=>done(X)).
  }

  public implementation execution[action->>string] => {
    _perform(done(X)) => X.
    _perform(delay(F)) => _perform(F()).

    _handle(done(X),_) => done(X).
    _handle(delay(A),E) => _handle(A(),E).
    _handle(err(X),E) => E(X).

    _raise(S) => err(S).
  }
}
