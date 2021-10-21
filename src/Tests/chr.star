test.chr{
  import star.
  import star.script.

  C1 = #a.
  C2 = #\n.

  main:()=>action[(),()].
  main()=>action{
    show C1;
    assert C2==#\n
  }
}
