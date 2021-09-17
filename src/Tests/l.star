test.l{
  import star.
  import star.script.

  fact:(integer)=>integer.
  fact(N) => let{
    public f:()=>integer.
    f() where _int_eq(N,0) => 1.
    f() => _int_times(N,fact(_int_minus(N,1))).
  } in f().

  main:()=>action[(),()].
  main()=>action{
    assert _int_eq(fact(3),6)
  }
}
