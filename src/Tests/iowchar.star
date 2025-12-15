test.iowchar{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  writeAll:(task[()],outHandle,string) => ().
  writeAll(this,IO,txt) => valof{
    try{
      for Ch in txt do{
	showMsg("write char: $(Ch)");
	wrCharAsync(IO,Ch);
      }
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
    valis ()
  }

  _main:(cons[string])=>integer.
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("output").

  main:(string)=>integer.
  main(Fl) => valof{
    try{
      In = openOutFile(Fl,.utf8Encoding);

      try{
	Rd = (Tsk) => valof{
	  writeAll(Tsk,In,"This is a test");
	  valis ()
	};
	  
	taskManager([Rd]);
	showMsg("writer done");
      } catch {
	.deadlock do showMsg("Writer got deadlocked")
      }
    } catch {
      | Cde do showMsg("error code $(Cde)")
    };

    valis 0
  }
}
  
