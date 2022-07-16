test.do1{
  import star.
  import star.script.

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
    } catch{
      _ => {}
    };
    valis ();
  }
}
