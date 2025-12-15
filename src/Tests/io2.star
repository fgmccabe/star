test.io2{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readAll:(task[cons[string]],inHandle) => cons[string].
  readAll(this,I) => valof{
    out := [];
    try{
      while Ln.=rdLine(I) do{
	showMsg("Ln: $(Ln)");
	out := [Ln,..out!]
      }
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
    valis reverse(out!)
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
	  
	Text = taskManager([Rd]);
	showMsg("output: $(Text)");
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
