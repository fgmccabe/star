test.dis{
  import star.
  import star.assert.

  check:(option[(integer,integer)]) => option[integer].
  check(S) =>
    (.some((A,1)) .= S ||
      .some((2,A)) .= S) ?? .some(A) || .none.

  main:()=>().
  main()=>valof{
    assert 3 ?= check(.some((3,1)));
    assert 3 ?= check(.some((2,3)));
    valis ()
  }
}
