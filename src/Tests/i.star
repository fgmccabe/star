test.i{
  import star.

  kk:map[string,integer].
  kk = ["alpha"->1, "beta"->2, "gamma"->3].

  ll:map[string,integer].
  ll = _replace(kk,"delta",4).

  assert 1^=kk["alpha"].
  assert some(1).=_index(ll,"alpha").
}
