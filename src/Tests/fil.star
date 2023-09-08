test.fil{
  import star.
  import star.file.
  import star.script.

  -- Test getting a file

  main:()=>().
  main() => valof{
    assert ~ _?=getFile(".");
    valis ()
  }
}
