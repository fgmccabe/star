test.dd{
  import star.
  import test.lib.fact.
  import star.script.

  ff:(integer)=>integer.
  ff(X) => fact(X).

  fc:(integer)=> ().
  fc(X) => valof{
    show fact(X);
    valis ()
  }

  AA : integer.
  AA = valof{
    valis 34.
  }.

  XX : integer.
  XX = valof{
    A = ff(4);
    B = ff(3);
    valis A*B.
  }

  YY : integer.
  YY = valof{
    fc(4);
    valis ff(5)
  }

  ZZ : integer.
  ZZ = valof {
    try {
      A = ff(6);
      B = ff(7);
      throw ()
    } catch {
      _ => valis 10
    }
  }

  UU:(integer)=>integer.
  UU = (U)=> valof{
    if ZZ == U then
      valis 1
    else
      valis YY
  }

  -- Test different error types across try-catch
  VV : integer.
  VV = valof {
    R = fact(10);
    try{
      fc(10);
      
      try {
        throw "fred"
      } catch {
	(F) => {
	  logMsg("we got an exception $(F)");
	  throw ()
	}
      }
    } catch {
      (E) => {
	logMsg("we got exception $(E), returning $(R)");
	valis R
      }
    }
  }

  TT : (integer)=>integer throws string.
  TT(U)=> valof{
    if U>0 then
      valis U
    else
    throw "Negative $(U)"
  }

  main:()=>().
  main()=>valof{
    try{
      show AA;
      show XX;
      show YY;

      assert ZZ == 10;
      
      show UU(10);
      show UU(9);

      show VV;
      assert VV == 3628800;

      assert TT(1)==1;
      show TT(-1)			-- never finish this
    } catch {
      M => logMsg("Huh: #(M)")
    };
    valis ()
  }
}
