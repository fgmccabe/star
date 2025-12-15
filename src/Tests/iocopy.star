test.iocopy{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  copyFl:(task[()],string,string) => ().
  copyFl(this,src,dest) => valof{
    try{
      Text = rdFileAsync(src);
      wrFileAsync(dest,Text);
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
    valis ()
  }

  _main:(cons[string])=>integer.
  _main([S,D,.._]) => main(S,D).
  _main([]) => main("iocopy.star","output").

  main:(string,string)=>integer.
  main(S,D) => valof{
    try{
      try{
	Rd = (Tsk) => copyFl(Tsk,S,D);
	
	Eras = taskManager([Rd]);
	showMsg("file copy done");
      } catch {
	.deadlock do showMsg("Writer got deadlocked")
      }
    } catch {
      | .eEOF do showMsg("end of file")
      | Cde do showMsg("error code $(Cde)")
    };

    valis 0
  }
}
  
