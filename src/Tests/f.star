test.f{
  import star.
  import star.assert.
  
  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).

  main:()=>().
  main()=>valof{
    assert _int_eq(fact(3),6);
    valis ()
  }
}
