star.result{
  import star.core.
  import star.iterable.
  import star.monad.
  import star.coerce.

  public all a,e ~~ result[e,a] ::=
      _has_value(a)
    | _not_yet(()=>result[e,a])
    | _no_result
    | _failed(e).


  public implementation all e ~~ execution[result[e]->>e] => {
    _perform(_has_value(X)) => X.
    _perform(_not_yet(F)) => _perform(F()).

    _lift(X) => _not_yet(()=>_has_value(X)).

    _sequence(_failed(E),_) => _failed(E).
    _sequence(_has_value(A),F) => _not_yet(()=>F(A)).
    _sequence(_not_yet(G),F) => _not_yet(()=>_sequence(G(),F)).

    _handle(_has_value(X),_) => _has_value(X).
    _handle(_not_yet(A),E) => _handle(A(),E).
    _handle(_failed(X),E) => E(X).

    _raise(S) => _failed(S).
  }

  public implementation all e ~~ monadZero[result[e]] => {
    zed = _no_result
  }
}
