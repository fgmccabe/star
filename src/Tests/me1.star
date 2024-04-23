test.me1{
  import star.
  import star.assert.

  -- simple test of memo expressions

  ff := 23.

  main:()=>action[(),()].
  main()=>do{
    XX = $$ ff!;

    showMsg("XX = $(XX!!)")
  }
}
