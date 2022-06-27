star.trace{
  import star.core.
  import star.action.

  public trace:all x ~~ display[x] |: (string,x)=>x.
  trace(M,X) => valof{
    logMsg("#(M) - $(X)");
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
  
