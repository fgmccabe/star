test.fg{
  import star.
  import test.f.

  _main:(list[string])=>().
  _main([F,.._]) where Fn.=F::integer &&
		 _.=_logmsg("Fact of $(Fn) is $(fact(Fn))") => ().
  _main([]) => ().
}
