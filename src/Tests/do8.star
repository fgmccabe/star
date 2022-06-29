test.do8{
  import star.

  main:()=>().
  main() => valof{
    A .= "alpha";
    B .= "beta";

    if A<B then
      logMsg("A wins")
    else
    logMsg("B wins");
    valis ()
  }
}
