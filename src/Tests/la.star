test.la{
  import star.
  import star.script.

  -- Test let actions

  fooBar(K) => valof{
    V .= K;
    let{
      VV = V.

      UU(U) => valof{
	logMsg("We UU'd $(U)");
	valis ()
      }.
    } in UU(V);
    valis 5
  }

  main:()=>().
  main()=>valof{
    try{
      FF .= fooBar(6);
      assert FF == 5;
    } catch { _ => logMsg("bad happening")};
    valis ()
  }
}
