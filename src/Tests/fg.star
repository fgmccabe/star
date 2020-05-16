test.fg{
  import star.
  import test.fact.

  public _main:(cons[string])=>().
  _main([F,.._]) where Fn.=F::integer => valof action{
    logMsg("Fact of $(Fn) is $(fact(Fn))").
  }
  _main([]) => ().
}
