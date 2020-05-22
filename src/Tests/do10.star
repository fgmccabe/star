test.do10{
  import star.

  pp ::= pp{
    ri : string.
    wi : string.
    age : integer.
  }

  repo:()=>string.
  repo()=>"hi".

  parse:(string)=>option[string].
  parse(X) => some(X).
  
  handle:(pp)=>action[(),()].
  handle(X) => do{
    logMsg(X.ri)
  }

  fred()=>"erick".
  john()=>"john".
  
  public _main:(cons[string])=>().
  _main(_) => valof action{
    RI ^= parse("fred"++fred());
    WI ^=  parse("file:"++john());
    handle(pp{ ri=RI. wi=WI. age=10 })
  }
}
