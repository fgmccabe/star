test.p{
  -- Test ^ style Patterns
  import star.
  import star.script.

  isOk:(string)=>option[integer].
  isOk("0") => some(0).
  isOk("10") => some(10).

  isOk(_) => .none.

  onlyOk:(cons[string]) => cons[integer].
  onlyOk(.nil) => .nil.
  onlyOk(cons(Z where X^=isOk(Z),L)) => cons(X,onlyOk(L)).
  onlyOk(cons(_,L)) => onlyOk(L).

  main:() => action[(),()].
  main() => action{
    assert onlyOk(cons("0",cons("1",cons("10",.nil)))) == cons(0,cons(10,.nil))
  }

}
