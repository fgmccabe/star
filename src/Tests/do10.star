test.do10{
  import star.

  pp ::= pp{
    ri : string.
    wi : string.
  }

  handle:(cons[string],pp)=>action[(),()].
  handle(_,X) => do{
    logMsg(X.ri)
  }

  public _main:(cons[string])=>().
  _main(Args) => valof action{
    RI ^= some("fred");
    WI ^=  some("file:");
    handle(Args,pp{ ri=RI. wi=WI. })
  }
}
