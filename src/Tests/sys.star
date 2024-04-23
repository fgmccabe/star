test.sys{
  import star.
  import star.assert.
  import star.system.

  main:()=>().
  main() => valof{
    show envir();

    assert ~ ("$$","$$") .<. envir();
    assert getenv("$$")==.none;

    setenv("$$","$$");
    assert "$$" ?= getenv("$$");

    assert ("$$","$$") .<. envir();
    
    valis ()
  }
}
