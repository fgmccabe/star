test.cnx{
  import star.
  import star.script.

  -- Test growing stacks

  fib(0) => 0.
  fib(1) => 1.
  fib(N) => fib(N-1)+fib(N-2).

  main:()=>().
  main()=>valof{
    show fib(10);
    valis ()
  }
}
