star.either{
  import star.core.
  import star.arith.
  import star.cons.
  import star.monad.

  -- Either or values
  public all a,b ~~ either[b,a] ::= either(a) | other(b).

  -- Display either-or values
  public implementation all x,y ~~ display[x], display[y] |: display[either[x,y]] => {
    disp(either(a)) => "either($(a))".
    disp(other(b)) => "or($(b))".
  }

  public implementation all x,y ~~ equality[x], equality[y] |: equality[either[x,y]] => {
    either(A)==either(B) => A==B.
    other(A)==other(B) => A==B.
    _==_ => .false.
  }

  public implementation all x,y ~~ hashable[x],hashable[y] |: hashable[either[x,y]] => {
    hash(either(A)) => hash(A)*37.
    hash(other(B)) => hash(B)*41.
  }

  public implementation execution[either] => {
    _isOk(either(_)) => .true.
    _isOk(other(_)) => .false.
    _getval(either(X)) => X.
    _errval(other(X)) => X.
    _valis(X) => either(X).
    _raise(S) => other(S).
  }

  public implementation all a ~~ monad[either[a]] => {
    (either(X) >>= F) => F(X).
    (other(E) >>= _) => other(E).

    return X => either(X).
  }

  public implementation all a ~~ functor[either[a]] => {
    fmap(F,either(X)) => either(F(X)).
    fmap(_,other(E)) => other(E).
    C <$ either(_) => either(C).
    C <$ other(E) => other(E).
  }

  public implementation all a ~~ applicative[either[a]] => {
    pure X => either(X).
    (either(F) <*> either(X)) => either(F(X)).
    (_ <*> other(E)) => other(E).
  }

  public implementation all e ~~ injection[option,either[e]] => {
    _inject(some(X)) => either(X).
  }
}
