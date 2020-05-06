test.do10{
  import star.

  pp ::= pp{
    name : string.
  }

  repo:()=>string.
  repo()=>"hi".

  parse:(string)=>option[string].
  parse(X) => some(X).
  
  handle:(pp)=>action[(),()].
  handle(X) => do{
    logMsg(X.name)
  }
  
  _main:()=>().
  _main() => valof action{
    RI ^= parse("fred");
    WI^=  parse("file:");
    handle(pp{. name=RI .})
  }
}
