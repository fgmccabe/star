test.do4{
  import star.
  import star.range.

  import star.script.

    -- Test action notation (for loop)

  doFact:(integer) => result[(),integer].
  doFact(X) => do{
    Fx .= ref 1;
    for Ix in range(1,X+1,1) do{
      Fx := Fx!*Ix
    };
    valis Fx!
  }

  main:()=>().
  main() => valof{
    try{
      show valof doFact(4);
      assert valof doFact(4) == 24;
    } catch {
      _ => logMsg("huh?")
    };
    valis ()
  }
}
