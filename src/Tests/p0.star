test.p0{
  import star.
  import star.script.

  public all el,st ~~ parser[st,el] ::= .parser((st)=>cons[(el,st)]).

  public parse:all e,s ~~ (parser[s,e],s) => cons[(e,s)].
  parse(.parser(P),S) => P(S).

  pick:all s,t ~~ stream[s->>t] |: (s) => cons[(t,s)].
  pick([C,..L]) => [(C,L)].
  pick([]) => [].

  public _item:all s,u ~~ stream[s->>u] |: parser[s,u].
  _item= .parser(pick).

  aa = parse(_item,([0]:cons[integer])).

  main:() => ().
  main() => valof{
    show aa;

    show parse(_item,([0]:cons[integer]));

    valis ()
  }
}
