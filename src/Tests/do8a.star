test.do8a{
  import star.core.

  switch = .true.

  foo()=>
    ((A) => ((B) => switch ? logMsg("A wins") || logMsg("B wins"))("beta"))("alpha").

  logMsg:(string)=>().
  logMsg(_)=>().

  public _main(_) =>
    foo().
}
