test.dd{
  import star.
  import test.fact.
  import star.script.

  ff:(integer)=>result[(),integer].
  ff(X) => _valis(fact(X)).

  fc:(integer)=> result[(),()].
  fc(X) => do{
    show fact(X);
    valis ()
  }

  AA : result[(),integer].
  AA = do{
    valis 34.
  }.

  XX : result[(),integer].
  XX = do{
    A .= valof ff(4);
    B .= valof ff(3);
    valis A*B.
  }

  YY : result[(),integer].
  YY = do{
    perform fc(4);
    valis valof ff(5)
  }

  ZZ : result[(),integer].
  ZZ = do {
    try {
      A <- ff(6);
      B <- ff(7);
      raise ()
    } catch {
      _ => valis 10
    }
  }

  UU : (integer)=>result[(),integer].
  UU = (U)=> do{
    if valof ZZ == U then
      valis 1
    else
      valis valof YY
  }

  -- Test different error types across try-catch
  VV : result[(),integer].
  VV = do {
    R .= fact(10);
    try{
      do fc(10);
      
      try {
        raise "fred"
      } catch {
	(F) => {
	  logMsg("we got exception $(F)");
	  raise ()
	}
      }
    } catch {
      (E) => {
	logMsg("we got exception $(E)");
	valis R
      }
    }
  }

  main:()=>().
  main()=>valof{
    try{
      show valof AA;
      show valof XX;
      show valof YY;

      assert valof ZZ == 10;
      
      show valof UU(10);
      show valof UU(9);

      show valof VV;
      assert valof VV == 3628800
    } catch {
      _ => logMsg("Huh")
    };
    valis ()
  }
}
