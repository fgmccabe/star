test.la{
  import star.
  import star.script.

  -- Test let actions

  fooBar(K) => do{
    V .= K;
    let{
      VV = V.

      UU(U) => do{
	logMsg("We UU'd $(U)");
	valis ()
      }.
    } in do UU(V);
    valis 5
  }

  main:()=>().
  main()=>valof{
    FF .= valof fooBar(6);
    assert FF == 5;
    valis ()
  }
}
