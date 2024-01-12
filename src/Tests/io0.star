test.io0{
  import star.
  import star.assert.
  import star.io.

  main:()=>().
  main() => valof{
    StdIn = _stdfile(0);

    try{
      while Ch.=_inchar(StdIn) && Ch~=`z` do{
	logMsg("char: $(Ch)");
      }
    } catch errorCode in {
      Cde => logMsg("error code $(Cde)")
    };

    valis ()
  }
}
