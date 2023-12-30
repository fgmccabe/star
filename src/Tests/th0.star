test.th0{
  import star.
  import star.assert.

  vv = ref .true.

  thunk = $$ valof{
    assert vv!;
    logMsg("make a thunk");
    vv := .false;
    valis 2
  }

  main:()=>().
  main() => valof{
    assert thunk!! == 2;
    assert thunk!! == 2;
    valis ()
  }
}
