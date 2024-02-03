test.io4{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],string) => ().
  readAll(this,Fl) => valof{
    try{
      if Txt.=rdFileAsync(Fl) then{
	logMsg("file text: $(Txt)");
      }
    } catch ioException in {
      | .ioError => logMsg("bad io")
      | .pastEof => logMsg("all done")
    };
    this retire .result(())
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string)=>().
  main(Fl) => valof{
    try{
      try{
	Rd = (Tsk) => valof{
	  readAll(Tsk,Fl);
	  Tsk retire .retired_
	};
	  
	Eras = nursery([Rd]);
	logMsg("reader done");
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
