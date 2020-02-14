test.dd{
  import star.
  import test.fact.
  import star.script.

  ff:(integer)=>action[(),integer].
  ff(X) => delay(()=>(return fact(X))).

  AA : action[(),integer].
  AA = do{
    return 34.
  }.

  XX : action[(),integer].
  XX = do{
    A <- ff(4);
    B <- ff(3);
    return A*B.
  }

  YY : action[(),integer].
  YY = do{
    ff(4)
  }

  ZZ : action[(),integer].
  ZZ = do {
    try {
      throw ()
    } catch {
      return 10
    }
  }

  UU : (integer)=>action[(),integer].
  UU = (U)=>do {
    if valof ZZ == U then
      return 1
    else
      valis valof YY
  }

  -- Test different error types across try-catch
  VV : action[(),integer].
  VV = do {
    try{
      try {
        throw "fred"
      } catch (F) => do{
        logMsg("we got exception $(F)");
	throw ()
      }
    } catch (E) => do{
      logMsg("we got exception $(E)");
      valis 10
    }
  }

  main:()=>action[(),()].
  main()=>do{
    show "AA=$(valof AA)";
    show "XX=$(valof XX)";
    show "YY=$(valof YY)";

    assert valof ZZ == 10;

    show "UU(10)=$(valof UU(10))";
    show "UU(9) = $(valof UU(9))";

    assert valof VV == 10
  }
}
