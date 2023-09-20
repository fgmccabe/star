test.sl{
  import star.
  import star.assert.

  -- test slice and splice operators
  F = "foobar".

  main:()=>().
  main() => valof{
    show F[3:6];
    assert F[3:6] == "bar";
    valis ()
  }
}
