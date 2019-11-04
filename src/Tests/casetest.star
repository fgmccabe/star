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

  sVal:(foo) => option[string].
  sVal(X) => case X in {
    foo("alpha")=>some("aleph").
    foo("beta") => some("two").
    bar(0) => none.
    foo(U) => some(U).
    _ default => none.
  }

  assert "TT" ^= sVal(foo("TT")).

  show "$(sVal(foo("beta")))".
}
