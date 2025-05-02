test.iowtext{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  writeAll:(task[()],ioHandle,string) => ().
  writeAll(this,IO,txt) => valof{
    try{
      wrTextAsync(IO,txt);
    } catch {
      | .ioError => showMsg("bad io")
      | .pastEof => showMsg("all done")
    };
    valis ()
  }

  _main:(cons[string])=>().
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("output").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = _openOutFile(Fl,3);

      try{
	Rd = (Tsk) => valof{
	  writeAll(Tsk,In,"This is a slightly longer test");
	  valis ()
	};
	  
	nursery([Rd]);
	showMsg("writer done");
      } catch {
	.deadlock => showMsg("Writer got deadlocked")
      };
      valis ()
    } catch {
      | .eof => showMsg("end of file")
      | Cde => showMsg("error code $(Cde)")
    };

    valis ()
  }
}
  
