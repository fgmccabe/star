test.gr3{
  import star.
  import star.assert.

  -- Test optional pattern

  decimal >> Ix -->
    dec(0) >> Ix.

  dec(Sx) >> Ix -->
    [X], {isDigit(X)}, dec(Sx*10+digitVal(X))>>Ix.
  dec(Sx) >> Sx --> [].

  int >> isSigned(S,Dx) --> ? sign >> S, decimal >> Dx.

  sign >> 1 --> [`+`].
  sign >> -1 --> [`-`].

  isSigned(.none,Ix) => Ix.
  isSigned(.some(Sx),Ix) => Sx*Ix.

  main:(){}.
  main() {
    assert 123 ?= (int --> "123");
    assert -123 ?= (int --> "-123");
    assert 123 ?= (int --> "+123");

    if (Ix,_) ?= int("1-23") then {
      show Ix
    }
  }
}

    
