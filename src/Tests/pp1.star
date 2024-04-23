test.pp1{
  import star.
  import star.assert.

  readFromPipe:(string,cons[string])=>cons[string].
  readFromPipe(Cmd,Args) => valof{
    try{
      (StdIn,StdOut,StdErr) = _popen(Cmd,Args,[]);

      valis readLines(StdOut)
    } catch errorCode in { C => {
	showMsg("Error in popen: $(C)");
	valis []
    }
    }
  }

  readLines(Fl) => valof{
    if _end_of_file(Fl) then
      valis []
    else{
      try{
	Ln = _inline(Fl);
	Rst = readLines(Fl);
	valis [Ln,..Rst]
      } catch errorCode in {
	.eof => valis []
	| Other => {
	  showMsg("io error: $(Other)");
	  _abort(Other,"error")
	}
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

  main:()=>().
  main()=>valof{
    readSomething();
    valis ()
  }
}
    
