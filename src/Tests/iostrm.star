test.iostrm{
  import star.
  import star.io.
  import star.iostream.
  import star.assert.

  countCPs:raises ioException |: (string)=>integer.
  countCPs(Fl) => let{.
   count([],Cx) => Cx.
   count([_,..Rst],Cx) => count(Rst,Cx+1)
  .} in count(inCharStream(Fl),0).

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

      logMsg("Char count $(foldLeft((_,Ix)=>Ix+1,0,inCharStream(Fl)))");

      assert countCPs(Fl)==foldLeft((_,Ix)=>Ix+1,0,inCharStream(Fl));

    } catch ioException in {
      | Cde => logMsg("error code $(Cde)")
    };
    valis ()
  }
}
  
      
      

