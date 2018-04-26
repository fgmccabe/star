test.a{
  import star.

  o : action[integer].
  o = return 1.

  p : action[integer].
  p = o >>= double.

  double:(integer) => action[integer].
  double(I) => return I+I.

  assert _perform(o)==1.

  assert _perform(p) == 2.
}
