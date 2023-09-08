star.log{
  import star.core.

  public traceCall:all x ~~ display[x] |: (string,x) => x.
  traceCall(M,X) => valof{
    _show("\e[34m#(M)\e[0m - $(X)");
    valis X
  }

  public trMsg:all x,y ~~ display[x] |: (string,x,y)=>y.
  trMsg(M,X,Y) => valof{
    logMsg("#(M) - $(X)");
    valis Y
  }

  public logMsg:(string)=>().
  logMsg(Msg) => valof{
    try{
      valis _logmsg(Msg);
    } catch errorCode in { _ => {}};
    valis ()
  }

  public showMsg:(string)=>().
  showMsg(Msg) => valof{
    valis _show(Msg)
  }
  

}
  
