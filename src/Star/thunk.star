star.thunk{
  import star.

  public all a,e ~~ thunk[e,a] ::=
    private dne(a) |
      private delai(()=>thunk[e,a]) |
      private bad(e).

  public implementation execution[thunk] => {
    _perform(Th) => _perf(Th,Th).

    _perf(dne(X),_) => X.
    _perf(delai(F),Th) => _perf(_overwrite(Th,F()),Th).

    _valis(X) => delai(()=>dne(X)).

    _sequence(bad(E),_) => bad(E).
    _sequence(dne(A),F) => delai(()=>F(A)).
    _sequence(delai(G),F) => delai(()=>_sequence(G(),F)).

    _handle(dne(X),_) => dne(X).
    _handle(delai(A),E) => _handle(A(),E).
    _handle(bad(X),E) => E(X).

    _raise(S) => bad(S).
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

  
