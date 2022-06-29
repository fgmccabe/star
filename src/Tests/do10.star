test.do10{
  import star.

  pp ::= pp{
    ri : string.
    wi : string.
  }

  hndle:all x/2 ~~ execution[x] |: (cons[string],pp)=>x[void,()].
  hndle(_,X) => do{
    logMsg(X.ri);
    valis ()
  }

  public _main:(cons[string])=>().
  _main(Args) => valof {
    RI ^= some("fred");
    WI ^=  some("file:");
    do (hndle(Args,pp{ ri=RI. wi=WI. }):result[void,()]);
    valis ()
  }
}
