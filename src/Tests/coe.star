test.coe{
  import star.core.
  import star.coerce.

  public _main(L) =>
    logStrs(L).

  logMsg:(string)=>().
  logMsg(M) => valof{
    _logmsg(M);
    valis ()}

  logStrs(.nil) => ().
  logStrs(.cons(H,T)) =>
    (_ .= logMsg(H) ?
	logStrs(T) ||
	logMsg("something went wrong")).
}
