test.cn2{
  import star.core.
  import star.action.

  
  public _perform:all a,e ~~ (action[a,e]) => a.
  _perform(action(F)) => case F() in {
    okResult(R) => R
  }

  public logM:(string)=>result[(),()].
  logM(M) => do{
    _ .= _logmsg(M);
    return ()
  }

  public _getResult:all a,e ~~ (result[a,e])=>a.
  _getResult(okResult(X)) => X.
}
