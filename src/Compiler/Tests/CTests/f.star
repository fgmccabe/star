test.f{
  import star.core.
  
  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).

  assert _int_eq(fact(3),6).
}
