test.iowtext{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  writeAll:(task[()],outHandle,string) => ().
  writeAll(this,IO,txt) => valof{
    try{
      wrTextAsync(IO,txt);
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
    valis ()
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("output").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = openOutFile(Fl,.utf8Encoding);

      try{
	Rd = (Tsk) => valof{
	  writeAll(Tsk,In,"This is a slightly longer test");
	  valis ()
	};
	  
	taskManager([Rd]);
	showMsg("writer done");
      } catch {
	.deadlock do showMsg("Writer got deadlocked")
      }
    } catch {
      | Cde do showMsg("error $(Cde)")
    };

    valis ()
  }
}
  
