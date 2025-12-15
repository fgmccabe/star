test.io1{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],inHandle) => ().
  readAll(this,I) => valof{
    try{
      while Ch.=rdCharAsync(I) do{
	showMsg("char: $(Ch)");
      }
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
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
	.deadlock do showMsg("Reader got deadlocked")
      }
    } catch {
      | Cde do showMsg("error $(Cde)")
    };
    valis 0
  }
}
