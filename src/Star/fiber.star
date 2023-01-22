star.fiber{
  import star.core.
  
  public all e,a ~~ fiber[e,a] <~ {}.

  public implementation all e,a ~~ equality[fiber[e,a]] => {
    F1 == F2 => _fiber_eq(F1,F2)
  }

  public implementation all d,e ~~ display[fiber[d,e]] => {
    disp(T) => _stringOf(T,2)
  }
}
