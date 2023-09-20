test.fg{
  import star.
  import star.assert.
  import test.lib.fact.

  public _main:(cons[string])=>().
  _main(_) => main(5).
  
  public main:(integer)=>().
  main(F) => valof{
    show fact(F);
    valis ()
  }
}
