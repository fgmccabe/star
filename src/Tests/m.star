test.m{
  import star.

  public all e,t ~~ parser[e,t] ::= parser((list[t])=>list[(e,list[t])]).

  public parse:all e,t ~~ (parser[e,t],list[t]) => list[(e,list[t])].
  parse(parser(P),S) => P(S).

  public contract all m/2 ~~ combinator[m] ::= {
    (>>=) : all a,b,t ~~ (m[a,t],(a)=>m[b,t]) => m[b,t].
    (return): all a,t ~~ (a) => m[a,t].
  }

  public implementation all t~~monad[all s~~(s)~>parser[s,t]] => {
    return a => parser((S)=>[(a,S)]).

    (P >>= F) => parser((S)=>flatten(parse(P,S)//(((a,S1))=>parse(F(a),S1)))).
  }

  public item:all t ~~ parser[t,t].
  item=let{
    t([C,..L]) => [(C,L)].
    t([]) => [].
  } in parser(t).
}
