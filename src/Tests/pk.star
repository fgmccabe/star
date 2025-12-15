test.pk{
  import star.
  import star.pkg.
  import star.location.
  import star.assert.

  main:(){}.
  main(){
    show __pkg__;

    assert __pkg__ == "test.pk";
  }
}
