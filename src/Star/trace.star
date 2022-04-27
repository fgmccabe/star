star.trace{
  import star.action.
  import star.core.
  import star.monad.
  import star.coerce.

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

}
  
