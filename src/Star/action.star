star.action{
  import star.core.
  import star.monad.
  import star.coerce.

  public implementation all e,a ~~ display[e],display[a] |:
    display[result[e,a]] => {
      disp(ok(O)) => "ok: $(O)".
      disp(bad(E)) => "bad: $(E)".
    }.

  public implementation execution[result] => {
    _isOk(ok(_)) => .true.
    _isOk(bad(_)) => .false.
    _perform(ok(X)) => X.
    _errval(bad(X)) => X.
    _valis(X) => ok(X).
    _raise(S) => bad(S).
  }

  public implementation all e ~~ monad[result[e]] => {
    (bad(E) >>= _) => bad(E).
    (ok(A) >>= F) => F(A).

    return X => ok(X).
  }

  public strmap:all x,y,m/1,s/1 ~~ monad[m],stream[s[x]->>x],
  sequence[s[y]->>y],reversible[s[y]] |:
    ((x)=>m[y],s[x]) => m[s[y]].
  strmap(F,S) => let{.
    mmm:(s[x],s[y]) => m[s[y]].
    mmm([],So) => return reverse(So).
    mmm([E,..Es],So) =>
      F(E) >>= (X)=>mmm(Es,[X,..So]).
  .} in mmm(S,[]).

/*  public seqmap:all x,y,e,m/2,s/1 ~~ execution[m],stream[s[x]->>x],sequence[s[y]->>y],reversible[s[y]] |:
    ((x)=>m[e,y],s[x]) => m[e,s[y]].
  seqmap(F,S) => let{.
    mmm:(s[x],s[y]) => m[e,s[y]].
    mmm([],So) => _valis(reverse(So)).
    mmm([E,..Es],So) =>
      _sequence(F(E),(X)=>mmm(Es,[X,..So])).
  .} in mmm(S,[]).
  */

  public logMsg:all m/2,e ~~ execution[m] |: (string)=>m[e,()].
  logMsg(Msg) => do{
    _ .= _logmsg(Msg);
    valis ()
  }

  public showMsg:all m/2,e ~~ execution[m] |: (string)=>m[e,()].
  showMsg(Msg) => do{
    _ .= _show(Msg);
    valis ()
  }
}
