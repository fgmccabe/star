test.coe{
  import star.core.
  import star.coerce.

  public _main(L) =>
    logStrs(L).

  logMsg:(string)=>().
  logMsg(M) where _ .= _logmsg(M) =>().

  logStrs(.nil) => ().
  logStrs(cons(H,T)) =>
    (_ .= logMsg(H) ?
	logStrs(T) ||
	logMsg("something went wrong")).
}
