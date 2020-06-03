test.lt{
  import star.
  import star.script.

  genInt:()=>integer.
  genInt() => 10.

  ll(M) => let{
    S = M.

    f(A,N) where N>=S => A.
    f(A,N) => f(A*N,N+1)
  } in f(1,1).

  
  main:()=>action[(),()].
  main()=>do{
    show ll(genInt());
    show ll(20)
  }
}  
