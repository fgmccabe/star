test.do4{
  import star.
  import star.range.

  import star.script.

    -- Test action notation (for loop)

  doFact:(integer) => action[(),integer].
  doFact(X) => do{
    Fx .= ref 1;
    for Ix in range(1,X+1,1) do{
      Fx := Fx!!*Ix
    };
    valis Fx!!
  }

  main:()=>action[(),()].
  main() => do{
    show valof doFact(4);
    assert valof doFact(4) == 24
  }
}
  
