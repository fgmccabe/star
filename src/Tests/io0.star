test.io0{
  import star.
  import star.assert.
  import star.io.

  _main:(cons[string]){}.
  _main([Fl,.._]) do main(Fl).
  _main([]) do main("test.txt").

  main:(string){}.
  main(Fl){
    try{
      In = _openInFile(Fl,3);
      while Ch.=_inchar(In) do{
	showMsg("char: $(Ch)");
      }
    } catch {
      | .eEOF do showMsg("end of file")
      | Cde do showMsg("error code $(Cde)")
    }
  }
}
