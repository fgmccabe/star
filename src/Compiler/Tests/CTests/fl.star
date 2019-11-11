test.fl{
  import star.core.
  
/*  public fact:(integer)=>integer.
  fact(N) => let{
    h(K) => f(K).
    
    f(0) => 1.
    f(K) => _int_times(K,g(_int_minus(K,1))).

    g(0) => 1.
    g(K) => _int_times(K,f(_int_minus(K,1))).
  } in h(N).
  checkOk = _int_eq(fact(3),6).
*/

  public ff:(integer)=>integer.
  ff(N)=>let{
    f(0)=>1.
    f(K) => _int_times(K,f(_int_minus(K,1))).
  } in let{
    r = f(N)
  } in r.

}
