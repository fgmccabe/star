test.pp1{
  import star.
  import star.assert.

  readFromPipe:(string,cons[string])=>cons[string].
  readFromPipe(Cmd,Args) => valof{
    try{
      (StdIn,StdOut,StdErr) = _popen(Cmd,Args,[]);

      valis readLines(StdOut)
    } catch { C do {
	showMsg("Error in popen: $(C)");
	valis []
    }
    }
  }

  readLines(Fl) => let{.
    rdLns:()=>cons[string] throws errorCode.
    rdLns() => valof{
      if _end_of_file(Fl) then
	valis [];
      Ln = _inline(Fl);
      Rst = rdLns();
      valis [Ln,..Rst]
    }
  .} in valof{
    try{
      valis rdLns()
    } catch {
      | .eEOF do valis []
      | Other do {
	showMsg("io error: $(Other)");
	unreachable
      }
    }
  }

  readSomething()=>valof{
    Text = readFromPipe("/bin/ls", ["-l"]);
    for Line in Text do{
      showMsg("We have $(Line)");
    };
    valis ()
  }

  main:(){}.
  main(){
    readSomething();
  }
}
    
