test.cnb{
  import star.
  import star.assert.

  public factW:(integer)=>integer.
  factW(N) => valof{
    I = ref 1;
    R = ref 1;

    while I!=<N do{
      R := R!*I!;
      I := I!+1
    };

    valis R!
  }

  public factF:(integer)=>integer.
  factF(N) => (try factFF(1,N,1) catch () in { _ => -1}).

  factFF:raises () |: (integer,integer,integer)=>integer.
  factFF(F,T,S) => valof{
    if F>T then
      valis S
    else
    valis factFF(F+1,T,S*F)
  }
  
  main:()=>().
  main() => valof{
    show factW(6);
    show factF(6);
    assert factW(10)==factF(10);
    valis ()
  }
}
  
