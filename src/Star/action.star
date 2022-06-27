star.action{
  import star.core.
  import star.coerce.

  public implementation all e,a ~~ display[e],display[a] |:
    display[result[e,a]] => {
      disp(ok(O)) => "ok: $(O)".
      disp(bad(E)) => "bad: $(E)".
    }.

  public implementation execution[result] => {
    _isOk(ok(_)) => .true.
    _isOk(bad(_)) => .false.
    _getval(ok(X)) => X.
    _errval(bad(X)) => X.
    _valis(X) => ok(X).
    _raise(S) => bad(S).
  }
}
