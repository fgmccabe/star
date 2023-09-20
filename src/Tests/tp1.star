test.tp1{
  import star.
  import star.assert.

  triple ~> (string,integer,float).

  first:(triple)=>string.
  first((X,_,_))=>X.

  sec:(triple)=>integer.
  sec((_,Y,_))=>Y.

  thd:(triple)=>float.
  thd((_,_,Z))=>Z.

  main:()=>().
  main()=>valof{
    TT = ("j",2,3.1);

    show "triple: $(TT)";
    show "first: $(first(TT))";
    show "third: $(thd(TT))";
    valis ()
  }
}
