test.sys{
  import star.
  import star.assert.
  import star.system.

  main:(){}.
  main(){
    assert ~ ("$$","$$") .<. envir();
    assert getenv("$$")==.none;

    setenv("$$","$$");
    show envir();
    assert "$$" ?= getenv("$$");

    assert ("$$","$$") .<. envir();
  }
}
