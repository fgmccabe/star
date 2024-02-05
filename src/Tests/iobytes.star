test.iobytes{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],ioHandle) => ().
  readAll(this,Io) => valof{
    try{
      while Data.=rdBytesAsync(Io,25) do{
	logMsg("file data: $(Data)");
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
      In = _openInFile(Fl,3);
      
      try{
	Rd = (Tsk) => valof{
	  readAll(Tsk,In);
	  Tsk retire .retired_
	};
	  
	Eras = nursery([Rd]);
	logMsg("reader done");
      } catch mboxException in {
	| .deadlock => logMsg("Reader got deadlocked")
	| .canceled => logMsg("Everything got canceled")
      };
      valis ()
    } catch errorCode in {
      | .eof => logMsg("end of file")
      | Cde => logMsg("error code $(Cde)")
    };

    valis ()
  }
}
