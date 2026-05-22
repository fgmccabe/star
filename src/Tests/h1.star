test.h1{
  import star.core.
  import star.arith.
  import star.strings.

  public implementation all x ~~ hashable[x] |= hashable[option[x]] => {
    hash(.some(X)) => hash("?")*37+hash(X).
    hash(.none) => hash("none").
  }
}
  
