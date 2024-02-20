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
      Strm = inCharStream(Fl);

      logMsg("unforced stream: $(Strm)");
      Forced = forceStream(Strm);

      logMsg("forced stream: #(_implode(Forced))");

      logMsg("forced line stream $(forceStream(inLineStream(Fl)))");
    } catch ioException in {
      | Cde => logMsg("error code $(Cde)")
    };
    valis ()
  }
}
  
      
      

