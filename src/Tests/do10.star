test.do10{
  import star.
  import star.assert.
  import star.io.

  main:()=>().
  main() => valof{
    YY = do{
      N1 <- .some(42);
      { N2 <- .some(N1+34);
	valis N2 }
    };

    try{
      showMsg("YY = $(valof YY)");
      assert valof YY == 76;
    } catch {
      _ => showMsg("We got an exception, instead of YY")
    };
    
    valis ()
  }
}
