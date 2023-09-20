test.p{
  -- Test ? ^ style expressions
  import star.
  import star.assert.

  isOk:(string)=>integer.
  isOk("0") => 0.
  isOk("10") => 10.

  onlyOk:(cons[option[string]]) => cons[option[integer]].
  onlyOk(.nil) => .nil.
  onlyOk(.cons(X,L)) => .cons((XX?=X??.some(isOk(XX))||.none),onlyOk(L)).

  main:() => action[(),()].
  main() => action{
    show onlyOk(.cons(.some("0"),.nil));
    show onlyOk(.cons(.some("0"),.cons(.none,.cons(.some("10"),.nil))));
    show onlyOk(.cons(.some("0"),.cons(.some("10"),.cons(.none,.nil))));

    assert onlyOk(.cons(.some("0"),.cons(.some("10"),.cons(.none,.nil)))) ==
      .cons(.some(0),.cons(.some(10),.cons(.none,.nil)))
  }

}
