test.la{
  import star.
  import star.assert.

  -- Test let actions

  fooBar(K) => valof{
    V = K;
    let{
      VV = V.

      UU(U) => valof{
	showMsg("We UU'd $(U)");
	valis ()
      }.
    } in UU(V);
    valis 5
  }

  main:()=>().
  main()=>valof{
    FF = fooBar(6);
    assert FF == 5;
    valis ()
  }
}
