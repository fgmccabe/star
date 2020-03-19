star.action{
  import star.core.
  import star.iterable.
  import star.monad.
  import star.option.
  import star.coerce.

  public all a,e ~~ action[e,a] ::= done(a) | delay(()=>action[e,a]) | err(e).

  public implementation all e ~~ monad[action[e]] => {
    (err(E) >>= _) => err(E).
    (done(A) >>= F) => delay(()=>F(A)).
    (delay(G) >>= F) => delay(()=>G()>>=F).

    return X => delay(()=>done(X)).
  }

  public implementation execution[action] => {
    _perform(done(X)) => X.
    _perform(delay(F)) => _perform(F()).

    _valis(X) => delay(()=>done(X)).

    _sequence(err(E),_) => err(E).
    _sequence(done(A),F) => delay(()=>F(A)).
    _sequence(delay(G),F) => delay(()=>_sequence(G(),F)).

    _handle(done(X),_) => done(X).
    _handle(delay(A),E) => _handle(A(),E).
    _handle(err(X),E) => E(X).

    _raise(S) => err(S).
  }

  public strmap:all x,y,m/1,s/1 ~~ monad[m],stream[s[x]->>x],sequence[s[y]->>y] |:
    ((x)=>m[y],s[x]) => m[s[y]].
  strmap(F,S) => let{
    mmm:(s[x],s[y]) => m[s[y]].
    mmm([],So) => return So.
    mmm([E,..Es],So) =>
      F(E) >>= (X)=>mmm(Es,[So..,X]).
  } in mmm(S,[]).

  public seqmap:all x,y,e,m/2,s/1 ~~ execution[m],stream[s[x]->>x],sequence[s[y]->>y] |:
    ((x)=>m[e,y],s[x]) => m[e,s[y]].
  seqmap(F,S) => let{
    mmm:(s[x],s[y]) => m[e,s[y]].
    mmm([],So) => _valis(So).
    mmm([E,..Es],So) =>
      _sequence(F(E),(X)=>mmm(Es,[So..,X])).
  } in mmm(S,[]).


  public implementation all e ~~ coercion[option[e],action[(),e]] => {
    _coerce(.none) => err(()).
    _coerce(some(X)) => done(X).
  }

  public (:=):all a,m/2,e ~~ execution[m] |: (ref a,a) => m[e,()].
  (:=)(L,V) => do{
    _ .= _assign(L,V);
    valis ()
  }

  public (!!):all a ~~ (ref a)=>a.
  (!!)(V) => _get(V).

/*  public (!!):all a ~~ (a)=>ref a.
  !! E => _cell(E).
*/

  public logMsg:all m/2,e ~~ execution[m] |: (string)=>m[e,()].
  logMsg(Msg) => do{
    _ .= _logmsg(Msg);
    valis ()
  }
}
