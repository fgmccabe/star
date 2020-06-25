sample.main{
  import star.
  import star.script.

  import sample.factorial.

  main:(integer) => action[(),()].
  main(Ix) => do{
    logMsg("Factorial of $(Ix) is $(fact(Ix))")
  }
}
