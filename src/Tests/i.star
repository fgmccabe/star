test.i{
  import star.

  kk:map[string,integer].
  kk = ["alpha"->1, "beta"->2, "gamma"->3].

  ll:map[string,integer].
  ll = _put(kk,"delta",4).
}
