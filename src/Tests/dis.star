test.dis{
  import star.

  check:(option[(integer,integer)]) => option[integer].
  check(S) =>
    (some((A,1)) .= S ||
      some((2,A)) .= S) ? some(A) || none.
/*  check(S) =>
    (some((A,1)) .= S ? some(A) ||
      some((2,A)) .= S ? some(A) || none).
*/
  assert 3 ^= check(some((3,1))).

  assert 3 ^= check(some((2,3))).
}
