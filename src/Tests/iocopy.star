test.iocopy{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  copyFl:(task[()],string,string) => ().
  copyFl(this,src,dest) => valof{
    try{
      Text = rdFileAsync(src);
--      showMsg("File text: #(Text)");
      wrFileAsync(dest,Text);
    } catch ioException in {
      | .ioError => showMsg("bad io")
      | .pastEof => showMsg("all done")
    };
    valis ()
  }

  _main:(cons[string])=>().
  _main([S,D,.._]) => main(S,D).
  _main([]) => main("iocopy.star","output").

  main:(string,string)=>().
  main(S,D) => valof{
    try{
      try{
	Rd = (Tsk) => copyFl(Tsk,S,D);
	
	Eras = nursery([Rd]);
	showMsg("file copy done");
      } catch mboxException in {
	.deadlock => showMsg("Writer got deadlocked")
      };
      valis ()
    } catch errorCode in {
      | .eof => showMsg("end of file")
      | Cde => showMsg("error code $(Cde)")
    };

    valis ()
  }
}
  
