star.fiber{
  import star.core.
  
  public implementation all e,a ~~ equality[fiber[e,a]] => {
    F1 == F2 => _fiber_eq(F1,F2)
  }
}
