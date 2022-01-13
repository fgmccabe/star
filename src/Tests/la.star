test.la{
  import star.
  import star.script.

  -- Test let actions

  fooBar(K) => action{
    V .= K;
    let{
      VV = V.
      UU:(integer) => action[(),()].
      UU(U) => do{
	logMsg("We UU'd $(U)")
      }.
    } in UU(V);
    valis 5
  }

  main:()=>action[(),()].
  main()=>action{
    FF .= valof fooBar(6);
    assert FF == 5
  }
}
