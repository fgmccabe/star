test.pp1{
  import star.
  import star.script.

  readFromPipe:(string,cons[string])=>cons[string].
  readFromPipe(Cmd,Args) => valof{
    (StdIn,StdOut,StdErr) .= _popen(Cmd,Args,[]);

    valis readLines(StdOut)
  }

  readLines(Fl) => valof{
    if _end_of_file(Fl) then
      valis []
    else{
      Ln.=_inline(Fl);
      Rst .= readLines(Fl);
      valis [Ln,..Rst]
    }
  }

  readSomething()=>valof{
    Text .= readFromPipe("/bin/ls", ["-l"]);
    for Line in Text do{
      logMsg("We have $(Line)");
    };
    valis ()
  }

  main:()=>().
  main()=>valof{
    readSomething();
    valis ()
  }
}
    
