test.f{
  import star.
  import star.script.
  
  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).

  main:()=>action[(),()].
  main()=>action{
    assert _int_eq(fact(3),6)
  }
}
