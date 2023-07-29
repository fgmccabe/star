star.fiber{
  import star.core.
  
  public implementation all e,a ~~ equality[(e)=>>a] => {
    F1 == F2 => _fiber_eq(F1,F2)
  }

  public implementation all d,e ~~ display[(d)=>>e] => {
    disp(T) => _stringOf(T,2)
  }
}
