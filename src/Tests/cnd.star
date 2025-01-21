test.cnd{
  import star.
  import star.assert.

  -- Test conditional expressions involving matching

  isValid(XX) where (_,I) ?= isQ(XX) => isValid(XX).
  isValid(XX) where (_,I) ?= isX(XX) => isValid(XX).
  isValid(XX) where (_,I) ?= isPr(XX) => isValid(XX).
  isValid(XX) where (_,I) ?= isPb(XX) => isValid(XX).
  isValid(XX) => _ ?= isB(XX).

  isQ("Q") => .some(("Q","Ok")).
  isQ(_) => .none.

  isX("X") => .some(("X","Ok")).
  isX(_) => .none.

  isPr("Pr") => .some(("Pr","Ok")).
  isPr(_) => .none.

  isPb("Pb") => .some(("Pb","Ok")).
  isPb(_) => .none.

  isB("Bin") => .some("Ok").
  isB(_) => .none.

  main()=>valof{
    assert isValid("Bin");
    valis ()
  }
}
