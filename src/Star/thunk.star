star.thunk{
  import star.

  public all a,e ~~ thunk[e,a] ::=
    private dne(a) |
      private delai(()=>thunk[e,a]) |
      private bad(e).

  public implementation all e ~~ execution[thunk[e]->>e] => {
    _perform(Th) => perf(Th,Th).
    
    _perf(dne(X),_) => X.
    _perf(delay(F),Th) => _perform(F()).

    _valis(X) => delay(()=>done(X)).

    _sequence(err(E),_) => err(E).
    _sequence(done(A),F) => delay(()=>F(A)).
    _sequence(delay(G),F) => delay(()=>_sequence(G(),F)).

    _handle(done(X),_) => done(X).
    _handle(delay(A),E) => _handle(A(),E).
    _handle(err(X),E) => E(X).

    _raise(S) => err(S).
  }
  
  

  public thunk:all x ~~ (()=>x) => ()=>x.
  thunk(F) => let{
    X := none.
    ff() where V^=X! => V.
    ff() => valof action{
      V = F();
      X := some(V);
      valis V
    }
  } in ff.
}

  
