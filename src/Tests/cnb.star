test.cnb{
  import star.core.
  import star.arith.
  import test.cn2.

  public factU:(integer)=>integer.
  factU(N) => valof do{
    I := 1;
    R := 1;

    do{
      R := R!*I!;
      I := I!+1
    } until I!>N;

    valis R!
  }

  public factF:(integer)=>integer.
  factF(N) => valof factFF(1,N,1).

  factFF(F,T,S) => do{
    if F>T then
      valis S
    else
    factFF(F+1,T,S*F)
  }
  
  _main:(cons[string])=>().
  _main(cons(A,.nil)) =>
    valof main(A).

  main:(string)=>action[(),()].
  main(T) => action{
    _ .= factU(6);
    _ .= factF(6);
    logM(T);
    valis ()
  }
}
  
