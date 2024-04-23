test.iochars{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],ioHandle) => ().
  readAll(this,Io) => valof{
    try{
      while Data.=rdCharsAsync(Io,25) do{
	showMsg("file data: $(Data)");
      }
    } catch ioException in {
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
      } catch mboxException in {
	| .deadlock => showMsg("Reader got deadlocked")
	| .canceled => showMsg("Everything got canceled")
      };
      valis ()
    } catch errorCode in {
      | .eof => showMsg("end of file")
      | Cde => showMsg("error code $(Cde)")
    };

    valis ()
  }
}
