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
	logMsg("Ln: $(Ln)");
	out := [Ln,..out!]
      }
    } catch ioException in {
      | .ioError => logMsg("bad io")
      | .pastEof => logMsg("all done")
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
	logMsg("output: $(Text)");
      } catch mboxException in {
	.deadlock => logMsg("Reader got deadlocked")
      };
      valis ()
    } catch errorCode in {
      | .eof => logMsg("end of file")
      | Cde => logMsg("error code $(Cde)")
    };

    valis ()
  }
}
