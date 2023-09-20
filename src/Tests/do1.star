test.do1{
  import star.
  import star.assert.

  -- Test action notation

  doFirst:()=>().
  doFirst() => valof{
    A = ref 1;

    A := A!+A!;

    assert A!==2;
    valis ()
  }

  main:()=>().
  main() => valof{
    try{
      doFirst();
    } catch () in {
      _ => {}
    };
    valis ();
  }
}
