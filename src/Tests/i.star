test.i{
  import star.
  import star.script.

  kk:map[string,integer].
  kk = ["alpha"->1, "beta"->2, "gamma"->3].
  
  ll:map[string,integer].
  ll = _put(kk,"delta",4).

  main:()=>action[(),()].
  main()=>do{
    assert 1^=kk["alpha"];
    assert some(1).=_index(ll,"alpha");
    show kk[!"alpha"];
    show kk["delta"->4]
  }

  public all k,v ~~ mp[k,v] <~ {}.

  private ihE : all k,v ~~ () <=> mp[k,v].
  ihL: all k,v ~~ (integer,cons[(k,v)]) <=> mp[k,v].

  foo = ihL(3,([("al",2)]:cons[_])).
  ef = .ihE.
}
