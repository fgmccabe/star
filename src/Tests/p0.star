test.p0{
  import star.

  public all e,s ~~ parser[s,e] ::= parser((s)=>list[(e,s)]).

  public parse:all e,s ~~ (parser[s,e],s) => list[(e,s)].
  parse(parser(P),S) => P(S).

  pick:all s,t ~~ stream[s->>t] |: (s) => list[(t,s)].
  pick([C,..L]) => [(C,L)].
  pick([]) => [].

  public _item:all s,u ~~ stream[s->>u] |: parser[s,u].
  _item=parser(pick).

  show disp(parse(_item,([0]:list[integer])))::string.
}
