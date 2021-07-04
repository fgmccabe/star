test.cn2{
  import test.cn0.
  import test.cn1.

  public result[e,o] ::= okResult(e) | eventResult(o).
  public action[a,e] ::= action(()=>result[a,e]).

  _perform:all a,e ~~ (action[a,e]) => a.
  _perform(action(F)) => case F() in {
    okResult(R) => R
  }

  logM:(string)=>result[(),()].
  logM(M) => do{
    _ .= _logmsg(M);
    return ()
  }

  _getResult:all a,e ~~ (result[a,e])=>a.
  _getResult(okResult(X)) => X.

  _main:(cons[string])=>().
  _main(_/*cons(A,.nil)*/) =>
    _getResult(main("hello")).

  main:(string)=>result[(),()].
  main(T) => do{
    logM(T)
  }
}
