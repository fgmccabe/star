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

  public sat:((integer)=>boolean) => parser[integer].
  sat(T) => item >>= ((Ch) => (T(Ch) ? return Ch | zed)).

  public chr:(integer) => parser[integer].
  chr(Chr) => sat((Ch)=>Ch==Chr).

  public str:(string) => parser[()].
  str(S) => let{
    prs:(list[integer]) => parser[()].
    prs([]) => return ().
    prs([Cx,..L]) => chr(Cx) >>= ((_) =>
      prs(L)).
  } in prs(S::list[integer]).

  public implementation monad[parser] => {
    return a => parser((S)=>[(a,S)]).

    P >>= F => parser((S)=>flatten(parse(P,S)//(((a,S1))=>parse(F(a),S1)))).
  }

  public implementation all x ~~ concat[parser[x]] => {.
    P1 ++ P2 => parser((S)=>parse(P1,S)++parse(P2,S)).
  .}

  public (+++): all e ~~ (parser[e],parser[e])=>parser[e].
  p+++q => let{
    first:all e ~~ (list[e])=>list[e].
    first([])=>[].
    first([E,.._])=>[E].
  } in parser((S)=>first(parse(p++q,S))).


  public implementation monadZero[parser] => {
    zed = parser((_)=>[]).
  }
}
