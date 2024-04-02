test.io4{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readFile:(task[string],string) => string.
  readFile(this,Fl) => valof{
    try{
      if Txt.=rdFileAsync(Fl) then{
	logMsg("file text: $(Txt)");
	valis Txt
      }
    } catch ioException in {
      | .ioError => logMsg("bad io")
      | .pastEof => logMsg("all done")
    };
    valis ""
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string)=>().
  main(Fl) => valof{
    try{
      try{
	Rd = (Tsk) => readFile(Tsk,Fl);
	  
	Text = nursery([Rd]);
	logMsg("reader done: $(Text)");
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
