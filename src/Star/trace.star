star.trace{
  import star.

  public trace:all e ~~ display[e] |: (e,string) => e.
  trace(E,Msg) => valof do{
    showMsg("#(Msg): $(E)");
    valis E
  }

  public trce:all e ~~ (e,string) => e.
  trce(E,Msg) => valof do{
    showMsg("#(Msg)");
    valis E
  }
}
  
