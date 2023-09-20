test.cse{
  import star.
  import star.assert.

  foo ::= .foo(string) | .bar(integer).

  strVal:(foo)=>option[string].
  strVal(F) => case F in {
    .foo("alpha")=>.some("aleph")
      | .foo("beta") => .some("two")
      | .bar(0) => .none
      | .foo(X) => .some(X)
  }

  sVal:(foo) => option[string].
  sVal(X) => case X in {
    .foo("alpha")=>.some("aleph")
      | .foo("beta") => .some("two")
      | .bar(0) => .none
      | .foo(U) => .some(U)
      | _ default => .none
  }

  main:()=>().
  main()=>valof{
    assert "TT" ?= sVal(.foo("TT"));

    show sVal(.foo("beta"));

    assert sVal(.foo("beta")) == .some("two");
    valis ()
  }
}
