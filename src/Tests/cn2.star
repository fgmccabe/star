test.cn2{
  import test.cn0.
  import test.cn1.

  public result[e,o] ::= okResult(e) | eventResult(o).
  public action[a,e] ::= action(()=>result[a,e]).

  public contract all C,e,o ~~ _exec[C->>e,o] ::= {
    _valof:(C)=>e.
  }
  
  public implementation all e,o ~~ _exec[action[e,o]->>e,o] => {.
    _valof(action(F)) => _valof(F())
  .}

  public implementation all e,o ~~ _exec[result[e,o]->>e,o] => {.
    _valof(okResult(X)) => X
  .}

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
