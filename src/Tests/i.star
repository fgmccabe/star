test.i{
  import star.
  import star.assert.

  kk:map[string,integer].
  kk = {"alpha"->1, "beta"->2, "gamma"->3}.
  
  ll:map[string,integer].
  ll = _put(kk,"delta",4).

  main:()=>().
  main()=>valof{
    assert 1?=kk["alpha"];
    assert .some(1).=_index(ll,"alpha");
    show kk[~"alpha"];
    show kk["delta"->4];
    valis ()
  }

  public all k,v ~~ mp[k,v] ::=
    .ihE |
    .ihL(integer,cons[(k,v)]).

  foo = .ihL(3,([("al",2)]:cons[_])).
  ef = .ihE.
}
