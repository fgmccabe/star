star.action{
  import star.core.
  import star.monad.
  import star.coerce.

  public result[e,a] ::= ok(a) | bad(e).

  public implementation all e,a ~~ display[e],display[a] |:
    display[result[e,a]] => {
      disp(ok(O)) => "ok: $(O)".
      disp(bad(E)) => "bad: $(E)".
    }.

  public implementation execution[result] => {
    _perform(ok(X)) => X.

    _valis(X) => ok(X).

    _sequence(bad(E),_) => bad(E).
    _sequence(ok(A),F) => F(A).

    _catch(ok(X),_) => ok(X).
    _catch(bad(X),E) => E(X).

    _raise(S) => bad(S).
  }

  public implementation all e ~~ monad[result[e]] => {
    (bad(E) >>= _) => bad(E).
    (ok(A) >>= F) => F(A).

    return X => ok(X).
  }

  public all a,e ~~ action[e,a] ::= done(a) | delay(()=>action[e,a]) | err(e).

  public implementation all e ~~ monad[action[e]] => {
    (err(E) >>= _) => err(E).
    (done(A) >>= F) => delay(()=>F(A)).
    (delay(G) >>= F) => delay(()=>G()>>=F).

    return X => delay(()=>done(X)).
  }

  public implementation all e,a ~~ display[e],display[a] |: display[action[e,a]] => {
    disp(done(O)) => "done: $(O)".
    disp(err(E)) => "err: $(E)".
    disp(delay(_)) => "delayed".
  }

  public implementation execution[action] => {
    _perform(done(X)) => X.
    _perform(delay(F)) => _perform(F()).

    _valis(X) => delay(()=>done(X)).

    _sequence(err(E),_) => err(E).
    _sequence(done(A),F) => delay(()=>F(A)).
    _sequence(delay(G),F) => delay(()=>_sequence(G(),F)).

    _catch(done(X),_) => done(X).
    _catch(delay(A),E) => _catch(A(),E).
    _catch(err(X),E) => E(X).

    _raise(S) => err(S).
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

  public seqmap:all x,y,e,m/2,s/1 ~~ execution[m],stream[s[x]->>x],sequence[s[y]->>y],reversible[s[y]] |:
    ((x)=>m[e,y],s[x]) => m[e,s[y]].
  seqmap(F,S) => let{.
    mmm:(s[x],s[y]) => m[e,s[y]].
    mmm([],So) => _valis(reverse(So)).
    mmm([E,..Es],So) =>
      _sequence(F(E),(X)=>mmm(Es,[X,..So])).
  .} in mmm(S,[]).

  public implementation all e ~~ coercion[option[e],action[(),e]] => {
    _coerce(.none) => some(err(())).
    _coerce(some(X)) => some(done(X)).
  }

  public (:=):all a,m/2,e ~~ execution[m] |: (ref a,a) => m[e,()].
  (:=)(L,V) => do{
    valis _assign(L,V)
  }

  public logMsg:all m/2,e ~~ execution[m] |: (string)=>m[e,()].
  logMsg(Msg) => do{
    _ .= _logmsg(Msg);
    valis ()
  }

  public trace:all x ~~ display[x] |: (string,x)=>x.
  trace(M,X) => valof action{
    logMsg("#(M) - $(X)");
    valis X
  }

  public showMsg:all m/2,e ~~ execution[m] |: (string)=>m[e,()].
  showMsg(Msg) => do{
    _ .= _show(Msg);
    valis ()
  }
}
