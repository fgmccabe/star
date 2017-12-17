sample.fact{
  import star.

  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N) where N>0 => N*fact(N-1).
}
