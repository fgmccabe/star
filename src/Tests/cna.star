test.cna{
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

  eq:(integer,integer)=>boolean.
  eq(X,Y) => let{.
    tst(0) => .true.
    tst(_) => .false
  .} in tst(minus(X,Y)).

  public (!):all a ~~ (ref a)=>a.
  (!)(V) => _get(V).

  public (:=):all a,e ~~ (ref a,a) => result[(),e].
  (:=)(L,V) => do{
    valis _assign(L,V)
  }

  _main:(cons[string])=>().
  _main(_/*cons(A,.nil)*/) =>
    valof main("hello").

  main:(string)=>action[(),()].
  main(T) => action{
    logM(T)
  }
  

}  
  
