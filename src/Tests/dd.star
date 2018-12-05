test.dd{
  import star.
  import test.fact.

  ff:(integer)=>action[string,integer].
  ff(X) => delay(()=>return fact(X)).

  XX : action[string,integer].
  XX = do{
    A <- ff(4);
    B <- ff(3);
    ^^ A*B.
  }

  YY : action[string,integer].
  YY = do{
    ff(4)
  }

  show "XX=\(_perform(XX))".
  show "YY=\(_perform(YY))".
}
