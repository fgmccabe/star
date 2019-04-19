test.fg{
  import star.
  import test.f.

  _main:(list[string])=>().
  _main([F,.._]) where Fn.=F::integer => valof action{
    logMsg("Fact of $(Fn) is $(fact(Fn))").
  }
  _main([]) => ().
}
