test.cnx{
  import star.
  import star.script.

  -- Test growing stacks

  fib(0) => 1.
  fib(1) => 1.
  fib(N) => fib(N-1)+fib(N-2).

  main:()=>().
  main()=>valis{
    show fib(10);
    valis ()
  }
}
