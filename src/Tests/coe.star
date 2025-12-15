test.coe{
  import star.core.
  import star.coerce.

  public _main(L) =>
    logStrs(L).

  logMsg:(string)=>integer.
  logMsg(M) => valof{
    try{
      _logmsg(M)
    } catch {_ do {}};
    valis 0
  }

  logStrs(.nil) => 0.
  logStrs(.cons(H,T)) =>
    (_ .= logMsg(H) ??
	logStrs(T) ||
	logMsg("something went wrong")).
}
