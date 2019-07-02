test.case{
  import star.

  foo ::= foo(string) | bar(integer).

  strVal:(foo)=>option[string].
  strVal(foo("alpha"))=>some("aleph").
  strVal(foo("beta")) => some("two").
  strVal(bar(0)) => none.
  strVal(foo(X)) => some(X).

  show disp(strVal(foo("t"))).

  assert "TT" ^= strVal(foo("TT")).
}
