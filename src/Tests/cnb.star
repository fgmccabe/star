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
  
  _main:(cons[string])=>().
  _main(cons(A,.nil)) =>
    valof main(A).

  main:(string)=>action[(),()].
  main(T) => action{
    _ .= factU(6);
    logM(T);
    valis ()
  }
}
  
