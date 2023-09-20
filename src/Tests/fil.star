test.fil{
  import star.
  import star.file.
  import star.assert.

  -- Test getting a file

  main:()=>().
  main() => valof{
    assert ~ _?=getFile(".");
    valis ()
  }
}
