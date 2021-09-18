test.fg{
  import star.
  import star.script.
  import test.fact.

  public _main:(cons[chars])=>().
  _main(_) => valof main(5).
  
  public main:(integer)=>action[(),()].
  main(F) => action{
    show fact(F)
  }
}
