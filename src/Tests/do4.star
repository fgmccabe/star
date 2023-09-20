test.do4{
  import star.
  import star.range.
  import star.assert.

    -- Test action notation (for loop)

  doFact:(integer) => integer.
  doFact(X) => valof{
    Fx = ref 1;
    for Ix in .range(1,X+1,1) do{
      Fx := Fx!*Ix
    };
    valis Fx!
  }

  main:()=>().
  main() => valof{
    show doFact(4);
    assert doFact(4) == 24;
    valis ()
  }
}
