  public implementation functor[option] => {.
    fmap(_,.none) => .none.
    fmap(F,some(A)) => some(F(A)).
 .}

  public implementation monad[option] => {
    return x => some(x).
    (some(x) >>= f) => f(x).
    (.none >>= _) => .none.
  }
  public implementation all a ~~ functor[either[a]] => {
    fmap(F,either(X)) => either(F(X)).
    fmap(_,other(E)) => other(E).
  }

  public implementation all e ~~ injection[option,either[e]] => {
    _inject(some(X)) => either(X).
  }
  public implementation all a ~~ monad[either[a]] => {
    (either(X) >>= F) => F(X).
    (other(E) >>= _) => other(E).

    return X => either(X).
}


  public implementation execution[either] => {
    _isOk(either(_)) => .true.
    _isOk(other(_)) => .false.
    _getval(either(X)) => X.
    _errval(other(X)) => X.
    _valis(X) => either(X).
    _raise(S) => other(S).
  }

  public implementation all e ~~ monad[result[e]] => {
    (bad(E) >>= _) => bad(E).
    (ok(A) >>= F) => F(A).

    return X => ok(X).
  }

