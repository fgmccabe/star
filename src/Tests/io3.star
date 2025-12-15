test.io3{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[()],inHandle) => ().
  readAll(this,I) => valof{
    try{
      while Ln.=rdLineAsync(I) do{
	showMsg("line: $(Ln)");
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
      };
      valis 0
    } catch {
      | Cde do showMsg("error $(Cde)")
    };

    valis 0
  }
}
