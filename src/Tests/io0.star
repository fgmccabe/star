test.io0{
  import star.
  import star.assert.
  import star.io.

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("io0.star").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = _openInFile(Fl,3);
      while Ch.=_inchar(In) do{
	logMsg("char: $(Ch)");
      }
    } catch errorCode in {
      | .eof => logMsg("end of file")
      | Cde => logMsg("error code $(Cde)")
    };

    valis ()
  }
}
