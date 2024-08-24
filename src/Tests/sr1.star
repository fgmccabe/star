test.sr1{
  import star.
  import star.assert.

  -- First test of shift/reset features.

  main:()=>().
  main() => valof{
    show 5+(reset 3+(shift c in c.(0)));
  }
}
