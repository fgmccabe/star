test.pp1{
  import star.
  import star.script.

  readFromPipe:(string,cons[string])=>cons[string].
  readFromPipe(Cmd,Args) => valof{
    (StdIn,StdOut,StdErr) .= _popen(Cmd,Args,[]);

    Data .= ref [];

    while ~ _end_of_file(StdOut) do{
      Seg .= _inline(StdOut);
      Data := [Seg,..Data!]
    };

    valis reverse(Data!)
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
    
