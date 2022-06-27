  public implementation functor[option] => {.
    fmap(_,.none) => .none.
    fmap(F,some(A)) => some(F(A)).
    C <$ L => fmap((_)=>C,L).
 .}

  public implementation applicative[option] => {
    pure X => some(X).
    (.none <*> _) => .none.
    (_ <*> .none) => .none.
    (some(F) <*> some(A)) => some(F(A)).
  }

  public implementation monad[option] => {
    return x => some(x).
    (some(x) >>= f) => f(x).
    (.none >>= _) => .none.
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
/*  public strmap:all x,y,m/1,s/1 ~~ monad[m],stream[s[x]->>x],
  sequence[s[y]->>y],reversible[s[y]] |:
    ((x)=>m[y],s[x]) => m[s[y]].
  strmap(F,S) => let{.
    mmm:(s[x],s[y]) => m[s[y]].
    mmm([],So) => return reverse(So).
    mmm([E,..Es],So) =>
      F(E) >>= (X)=>mmm(Es,[X,..So]).
  .} in mmm(S,[]).

  public seqmap:all x,y,e,m/2,s/1 ~~ execution[m],stream[s[x]->>x],sequence[s[y]->>y],reversible[s[y]] |:
    ((x)=>m[e,y],s[x]) => m[e,s[y]].
  seqmap(F,S) => let{.
    mmm:(s[x],s[y]) => m[e,s[y]].
    mmm([],So) => _valis(reverse(So)).
    mmm([E,..Es],So) =>
      _sequence(F(E),(X)=>mmm(Es,[X,..So])).
  .} in mmm(S,[]).

  */

  public implementation all e ~~ monad[result[e]] => {
    (bad(E) >>= _) => bad(E).
    (ok(A) >>= F) => F(A).

    return X => ok(X).
  }

