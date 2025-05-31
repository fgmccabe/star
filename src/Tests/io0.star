test.io0{
  import star.
  import star.assert.
  import star.io.

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = _openInFile(Fl,3);
      while Ch.=_inchar(In) do{
	showMsg("char: $(Ch)");
      }
    } catch {
      | .eEOF => showMsg("end of file")
      | Cde => showMsg("error code $(Cde)")
    };

    valis ()
  }
}
