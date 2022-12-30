test.lib.fact{
  import star.arith.

  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>N*fact(N-1).
}
