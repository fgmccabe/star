test.cnd{
  import star.
  import star.script.

  T = tag().

  UU = T prompt 1.
  XX = T prompt (1 + (T cut K in 3)).

  YY = T prompt (1 + (T cut K in K..(3))).

  main:()=>action[(),()].
  main()=>action{
    show UU;
    assert UU == 1;
    show XX;
    assert XX==3;
    show YY
  }
    
}
