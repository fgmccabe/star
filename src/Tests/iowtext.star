test.iowtext{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  writeAll:(task[()],ioHandle,string) => ().
  writeAll(this,IO,txt) => valof{
    try{
      wrTextAsync(IO,txt);
    } catch ioException in {
      | .ioError => logMsg("bad io")
      | .pastEof => logMsg("all done")
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
	  
	Eras = nursery([Rd]);
	logMsg("writer done");
      } catch mboxException in {
	.deadlock => logMsg("Writer got deadlocked")
      };
      valis ()
    } catch errorCode in {
      | .eof => logMsg("end of file")
      | Cde => logMsg("error code $(Cde)")
    };

    valis ()
  }
}
  
