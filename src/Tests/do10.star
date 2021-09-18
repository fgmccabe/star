test.do10{
  import star.

  pp ::= pp{
    ri : string.
    wi : string.
  }

  handle:(cons[string],pp)=>result[(),()].
  handle(_,X) => do{
    logMsg(X.ri)
  }

  public _main:(cons[chars])=>().
  _main(Args) => valof action{
    RI ^= some("fred");
    WI ^=  some("file:");
    handle(Args//((C)=>chrs_(C)),pp{ ri=RI. wi=WI. })
  }
}
