test.io2{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[cons[string]],ioHandle) => cons[string].
  readAll(this,IO) => valof{
    out := [];
    try{
      while Ln.=rdLine(IO) do{
	showMsg("Ln: $(Ln)");
	out := [Ln,..out!]
      }
    } catch {
      | .ioError => showMsg("bad io")
      | .pastEof => showMsg("all done")
    };
    valis reverse(out!)
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = _openInFile(Fl,3);

      try{
	Rd = (Tsk) => readAll(Tsk,In);
	  
	Text = nursery([Rd]);
	showMsg("output: $(Text)");
      } catch {
	.deadlock => showMsg("Reader got deadlocked")
      };
      valis ()
    } catch {
      | .eof => showMsg("end of file")
      | Cde => showMsg("error code $(Cde)")
    };

    valis ()
  }
}
