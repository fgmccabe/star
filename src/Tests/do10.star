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

  public _main:(cons[chars])=>().
  _main(Args) => valof action{
    RI ^= some("fred");
    WI ^=  some("file:");
    hndle(Args//((C)=>chrs_(C)),pp{ ri=RI. wi=WI. })
  }
}
