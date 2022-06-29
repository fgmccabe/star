test.do1{
  import star.
  import star.script.

  -- Test action notation

  doFirst:()=>result[void,()].
  doFirst() => do{
    A .= ref 1;

    A := A!+A!;

    assert A!==2;
    valis ()
  }

  main:()=>().
  main() => valof{
    try{
      do doFirst();
    } catch{
      _ => {}
    };
    valis ();
  }
}
