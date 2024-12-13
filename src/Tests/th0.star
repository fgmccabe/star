test.th0{
  import star.
  import star.assert.

  vv = ref .true.

  thkFn:all x ~~ (()=>x)=>()=>x.
  thkFn(Lam) => 
    let{.
      K = Lam().
    .} in (()=>K).

  thunk = thkFn(()=>valof{
      assert vv!;
      showMsg("make a thunk");
      vv := .false;
      valis 2
    }).

  main:()=>().
  main() => valof{
    assert vv!;
    assert thunk() == 2;
    assert ~vv!;
    assert thunk() == 2;
    valis ()
  }
}
