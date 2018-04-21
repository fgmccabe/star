test.ll{
  import star.core.

  fact:()=>(integer)=>integer.
  fact() => let{
    public f:(integer)=>integer.
    f(N) where _int_eq(N,0) => 1.
    f(N) => _int_times(N,f(_int_minus(N,1))).
  } in f.

  assert _int_eq(fact()(3),6). 
}
