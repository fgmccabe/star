test.io1{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],ioHandle) => ().
  readAll(this,IO) => valof{
    try{
      while Ch.=rdCharAsync(IO) do{
	showMsg("char: $(Ch)");
      }
    } catch {
      | .ioError => showMsg("bad io")
      | .pastEof => showMsg("all done")
    };
    valis ()
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
	  
	nursery([Rd]);
	showMsg("reader done");
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
