test.iostrm{
  import star.
  import star.io.
  import star.iostream.

  _main:(cons[string])=>().
  _main([S,.._]) => main(S).
  _main([]) => main("iocopy.star").

  main:(string)=>().
  main(Fl) => valof{
    try{
      In = _openInFile(Fl,3);
      Strm = inStream(In);

      logMsg("unforced stream: $(Strm)");
      Forced = forceStream(Strm);

      logMsg("forced stream: #(_implode(Forced))");
    } catch errorCode in {
      | .eof => logMsg("end of file")
      | Cde => logMsg("error code $(Cde)")
    };
    valis ()
  }
}
  
      
      

