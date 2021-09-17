test.func{
  import star.
  import star.script.

  import test.fog.

  public main:()=>action[(),()].
  main()=>action{
    X .= (K(3)•id)(4);

    show X;
    assert X==3
  }
}
