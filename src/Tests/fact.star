test.fact{
  import star.

  fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>N*fact(N-1).

  assert fact(3)==6.
}
