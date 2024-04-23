test.do8a{
  import star.core.

  switch = .true.

  foo()=>
    ((A) => ((B) => switch ?? showMsg("A wins") || showMsg("B wins"))("beta"))("alpha").

  showMsg:(string)=>().
  showMsg(_)=>().

  public _main(_) =>
    foo().
}
