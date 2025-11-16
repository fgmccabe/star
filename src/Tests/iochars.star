test.iochars{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],inHandle) => ().
  readAll(this,I) => valof{
    try{
      while Data.=rdCharsAsync(I,25) do{
	showMsg("file data: $(Data)");
      }
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
    valis ()
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = openInFile(Fl,.utf8Encoding);
      
      try{
	Rd = (Tsk) => readAll(Tsk,In);
	  
	taskManager([Rd]);
	showMsg("reader done");
      } catch {
	| .deadlock do showMsg("Reader got deadlocked")
	| .canceled do showMsg("Everything got canceled")
      }
    } catch {
      | Cde do showMsg("error $(Cde)")
    };

    valis ()
  }
}
