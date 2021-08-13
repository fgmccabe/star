test.cn2{
  import star.core.
  import star.action.
  import star.monad.

  
  public _perform:all a,e ~~ (action[e,a]) => a.
  _perform(action(F)) => _valof(F()).


  public _getResult:all a,e ~~ (result[e,a])=>a.
  _getResult(R) => _valof(R).
}
