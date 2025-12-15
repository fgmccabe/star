test.fg{
  import star.
  import star.assert.
  import test.lib.fact.

  public _main:(cons[string])=>integer.
  _main(_) => main(5).
  
  public main:(integer)=>integer.
  main(F) => valof{
    show fact(F);
    valis 0
  }
}
