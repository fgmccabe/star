test.do10{
  import star.

  pp ::= pp{
    ri : string.
    wi : string.
  }

  hndle:(cons[string],pp)=>result[(),()].
  hndle(_,X) => do{
    logMsg(X.ri)
  }

  public _main:(cons[string])=>().
  _main(Args) => valof action{
    RI ^= some("fred");
    WI ^=  some("file:");
    hndle(Args,pp{ ri=RI. wi=WI. })
  }
}
