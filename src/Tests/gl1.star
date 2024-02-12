test.gl1{
  import star.
  import star.assert.

  import test.lib.fact.

  -- Define some globals that depend on each other

  g1 = g2+g3.
  g2 = g3+g4.

  g3 = fact(4).
  g4 = 1.

  main:()=>().
  main() => valof{
    show g1;
    assert g1 == 49
  }
}
