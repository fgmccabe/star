test.me1{
  import star.
  import star.script.

  -- simple test of memo expressions

  ff := 23.

  main:()=>action[(),()].
  main()=>do{
    XX .= $$ ff!;

    logMsg("XX = $(XX!!)")
  }
}
