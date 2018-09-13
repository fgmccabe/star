test.f{
  import star.

  fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).

  assert _int_eq(fact(3),6).

  _main:(list[string])=>().
  _main([F,.._]) where Fn.=F::integer &&
    _.=_logmsg("Fact of \(Fn) is \(fact(Fn))") => ().
}
