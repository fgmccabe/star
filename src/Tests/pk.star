test.pk{
  import star.
  import star.pkg.
  import star.location.
  import star.script.

  main:() => ().
  main() => valof{
    show __pkg__;

    assert __pkg__ == "test.pk";

    valis ()
  }
}
