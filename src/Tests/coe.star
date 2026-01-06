test.coe{
  import star.core.
  import star.coerce.

  public _main(L) =>
    logStrs(L).

  logMsg:(string)=>integer.
  logMsg(M) => valof{
    try{
      _logmsg(M)
    } catch {_ do {unreachable}};
    valis 0
  }

  badCoerce:(string)=>integer.
  badCoerce(S) => S :? integer.

  logStrs(.nil) => valof{
    logMsg(_stringOf(badCoerce("10"),0));
    logMsg(_stringOf(badCoerce("hello"),0));
    valis 0
  }
  logStrs(.cons(H,T)) =>
    (_ .= logMsg(H) ??
	logStrs(T) ||
	logMsg("something went wrong")).
}
