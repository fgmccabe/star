star.trace{
  import star.

  public trace:all e ~~ (e,string) => e.
  trace(E,Msg) => valof do{
    showMsg(Msg);
    valis E
  }
}
  
