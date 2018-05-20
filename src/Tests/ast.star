test.ast{
  import star.

  public ast ::= idn(string) | num(string) | str(string) | tpl(string,list[ast]) | app(ast,ast).
}
