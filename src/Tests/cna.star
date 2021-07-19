test.cna{
  import star.core.
  import test.cn0.
  import test.cn1.
  import test.cn2.

  public factW:(integer)=>integer.
  factW(N) => valof do{
    I := 1;
    R := 1;
    while ~eq(I!,N) do{
      I := plus(I!,1);
      R := times(R!,I!)
    };
    valis R!
  }

  public implementation equality[integer] => {
    X == Y => _int_eq(X,Y).
  }

  eq:(integer,integer)=>boolean.
  eq(X,Y) => let{.
    tst(0) => .true.
    tst(_) => .false
  .} in tst(minus(X,Y)).

  _main:(cons[string])=>().
  _main(cons(A,.nil)) =>
    valof main(A).

  main:(string)=>action[(),()].
  main(T) => action{
    _ .= factW(6);
    logM(T);
    valis ()
  }
  

}  
  
