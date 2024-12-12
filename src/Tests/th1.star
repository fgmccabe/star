test.th1{
  import star.
  import star.assert.

  vv = ref .true.

  thunk = $$ valof{
    assert vv!;
    showMsg("make a thunk");
    vv := .false;
    valis 2
  }

  main:()=>().
  main() => valof{
    assert vv!;
    assert thunk!! == 2;
    assert ~vv!;
    assert thunk!! == 2;
    valis ()
  }
}
