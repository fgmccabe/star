star.log{
  import star.core.

  public traceCall:all x ~~ display[x] |: (string,x) => x.
  traceCall(M,X) => valof{
    _show("#(M) - $(X)");
    valis X
  }

  public trMsg:all x,y ~~ display[x] |: (string,x,y)=>y.
  trMsg(M,X,Y) => valof{
    logMsg("#(M) - $(X)");
    valis Y
  }

  public logMsg:(string)=>().
  logMsg(Msg) => valof{
    valis _logmsg(Msg);
  }

  public showMsg:(string)=>().
  showMsg(Msg) => valof{
    valis _show(Msg)
  }
  

}
  