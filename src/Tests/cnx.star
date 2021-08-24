test.cnx{
  import star.
  import star.script.

  -- Test growing stacks

  fib(0) => 1.
  fib(1) => 1.
  fib(N) => fib(N-1)+fib(N-2).

  main:()=>action[(),()].
  main()=>action{
    show fib(10)
  }
}
