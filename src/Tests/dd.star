test.dd{
  import star.
  import test.fact.
  import star.script.

  ff:(integer)=>action[(),integer].
  ff(X) => _valis(fact(X)).

  fc:(integer)=>action[(),()].
  fc(X) => do{
    show fact(X)
  }

  AA : action[(),integer].
  AA = action{
    valis 34.
  }.

  XX : action[(),integer].
  XX = action{
    A .= valof ff(4);
    B .= valof ff(3);
    valis A*B.
  }

  YY : action[(),integer].
  YY = action{
    fc(4);
    ff(5)
  }

  ZZ : action[(),integer].
  ZZ = action {
    try {
      raise ()
    } catch {
      valis 10
    }
  }

  UU : (integer)=>action[(),integer].
  UU = (U)=>action {
    if valof ZZ == U then
      valis 1
    else
      valis valof YY
  }

  -- Test different error types across try-catch
  VV : action[(),integer].
  VV = action {
    R .= fact(10);
    try{
      fc(10);
      
      try {
        raise "fred"
      } catch (F) => do{
        logMsg("we got exception $(F)");
	raise ()
      }
    } catch (E) => do{
      logMsg("we got exception $(E)");
      valis R
    }
  }

  main:()=>action[(),()].
  main()=>action{
    show valof AA;
    show valof XX;
    show valof YY;

    assert valof ZZ == 10;

    show valof UU(10);
    show valof UU(9);

    show valof VV;
    assert valof VV == 3628800
  }
}
