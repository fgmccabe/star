test.do1{
  import star.
  import star.script.

  -- Test action notation

  doFirst() => do{
    A .= ref 1;

    A := A!!+A!!;

    assert A!!==2
  }

  main:()=>action[(),()].
  main() => do{
    doFirst()
  }
}
