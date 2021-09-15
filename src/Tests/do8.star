test.do8{
  import star.

  main:()=>action[(),()].
  main() => action{
    A .= "alpha";
    B .= "beta";

    if A<B then
      logMsg("A wins")
    else
      logMsg("B wins")
  }
}
