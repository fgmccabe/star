sample.main{
  import star.

  import sample.factorial.

  main:(integer) => ().
  main(Ix) => valof{
    logMsg("Factorial of $(Ix) is $(fact(Ix))");
    valis ()
  }
}
