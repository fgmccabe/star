test.io4{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readFile:(task[string],string) => string.
  readFile(this,Fl) => valof{
    try{
      if Txt.=rdFileAsync(Fl) then{
	showMsg("file text: $(Txt)");
	valis Txt
      }
    } catch ioException in {
      | .ioError => showMsg("bad io")
      | .pastEof => showMsg("all done")
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
	showMsg("reader done: $(Text)");
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
