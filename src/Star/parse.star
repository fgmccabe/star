star.parse{
  import star.

  public all e ~~ parser[e] ::= parser((list[integer])=>list[(e,list[integer])]).

  public parse:all e ~~ (parser[e],list[integer]) => list[(e,list[integer])].
  parse(parser(P),S) => P(S).

  public item:parser[integer].
  item=let{
    t:(list[integer])=>list[(integer,list[integer])].
    t([C,..L]) => [(C,L)].
    t([]) => [].
  } in parser(t).

  public implementation monad[parser] => {
    return a => parser((S)=>[(a,S)]).

    P >>= F => parser((S)=>flatten(parse(P,S)//(((a,S1))=>parse(F(a),S1)))).
  }
}
