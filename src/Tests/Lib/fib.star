test.lib.fib{
  import star.

  public fib:(integer)=>integer.
  fib(0) => 0.
  fib(1) => 1.
  fib(N) => fib(N-1)+fib(N-2).
}
