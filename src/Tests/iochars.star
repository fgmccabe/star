test.iochars{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],inHandle) => ().
  readAll(this,I) => valof{
    count := 0;
    try{
      while ~ atEof(I) && Ch.=rdCharAsync(I) do{
	count := count!+1;
      }
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("past eof")
    };
    showMsg("We got $(count!) chars");
    valis ()
  }

  _main:(cons[string]) => integer.
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string) => integer.
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

    valis 0
  }
}
