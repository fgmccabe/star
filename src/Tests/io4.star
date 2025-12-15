test.io4{
  import star.
  import star.assert.
  import star.io.
  import star.mbox.

  readFile:(task[string],string) => string.
  readFile(this,Fl) => valof{
    try{
      if Txt.=rdFileAsync(Fl) then{
	showMsg("file text: $(Txt)");
	valis Txt
      }
    } catch {
      | .ioError do showMsg("bad io")
      | .pastEof do showMsg("all done")
    };
    valis ""
  }

  _main:(cons[string]) => integer.
  _main([Fl,.._]) => main(Fl).
  _main([]) => main("test.txt").

  main:(string) => integer.
  main(Fl) => valof{
    try{
      try{
	Rd = (Tsk) => readFile(Tsk,Fl);
	  
	Text = taskManager([Rd]);
	showMsg("reader done: $(Text)");
      } catch {
	| .deadlock do showMsg("Reader got deadlocked")
	| .canceled do showMsg("Everything got canceled")
      };
      valis 0
    } catch {
      | .eEOF do showMsg("end of file")
      | Cde do showMsg("error code $(Cde)")
    };

    valis 0
  }
}
