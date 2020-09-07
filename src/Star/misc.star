star.misc{
  import star.core.
  import star.arith.

  public repeat:all a ~~ ((a)=>a,integer)=>(a)=>a.
  repeat(F,N) => let{
    ff(0,X) => X.
    ff(N,X) => ff(N-1,F(X))
  } in ((X)=>ff(N,X))
}
    
