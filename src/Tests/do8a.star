test.do8a{
  import star.core.

  switch = .true.

  foo()=>
    ((A) => ((B) => switch ?? showMsg("A wins") || showMsg("B wins"))("beta"))("alpha").

  showMsg:(string)=>().
  showMsg(M) => _show(M).

  public _main(_) => valof{
    foo();
    valis 0
  }
}
