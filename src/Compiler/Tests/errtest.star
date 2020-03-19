test.comp.err{
  import star.
  import star.compiler.errors.
  import star.compiler.location.
  import star.pkg.

  r = reports([]).

  r1 = reportError(r,"This is a test",locn(pkg("foo",defltVersion),0,0,0)).

  assert errorFree(r) && ! errorFree(r1).

  show disp(r1)::string.
}
