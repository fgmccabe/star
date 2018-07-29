test.p0{
  import star.

  public all el,st ~~ parser[st,el] ::= parser((st)=>list[(el,st)]).

  public parse:all e,s ~~ (parser[s,e],s) => list[(e,s)].
  parse(parser(P),S) => P(S).

  pick:all s,t ~~ stream[s->>t] |: (s) => list[(t,s)].
  pick([C,..L]) => [(C,L)].
  pick([]) => [].

  public _item:all s,u ~~ stream[s->>u] |: parser[s,u].
  _item=parser(pick).

  aa = parse(_item,([0]:list[integer])).

  bb = disp(aa).

  show bb::string.

  show disp(parse(_item,([0]:list[integer])))::string.
}
