test.do8a{
  import star.core.

  switch = .true.

  foo()=>
    ((A) => ((B) => switch ? logMsg("A wins") || logMsg("B wins"))("beta"))("alpha").

  logMsg:(string)=>().
  logMsg(_)=>().
}
