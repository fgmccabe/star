star.parse{
  import star.

  public all e,t ~~ parser[t,e] ::= parser((list[t])=>list[(e,list[t])]).

  public parse:all e,t ~~ (parser[t,e],list[t]) => list[(e,list[t])].
  parse(parser(P),S) => P(S).

  public item:all t ~~ parser[t,t].
  item=let{
    t([C,..L]) => [(C,L)].
    t([]) => [].
  } in parser(t).

  public hed:all e,t ~~ (parser[t,e])=>parser[t,e].
  hed(P) => let{
    hd([],_) => [].
    hd([(F,_),.._],S) => [(F,S)].
  } in parser((S)=>hd(parse(P,S),S)).

  public sat:all t ~~ ((t)=>boolean) => parser[t,t].
  sat(T) => item >>= (Ch) => (T(Ch) ? return Ch | zed).

  public tk:all t ~~ equality[t]|:(t)=>parser[t,t].
  tk(Chr) => sat((Ch)=>Ch==Chr).

  public str:(string) => parser[integer,()].
  str(S) => let{
    prs([]) => return ().
    prs([Cx,..L]) => tk(Cx) >>= (_) => prs(L).
  } in prs(S::list[integer]).

  all t ~~ eparser[t] ~> (all e ~~ (e)~>parser[t,e]).

  public implementation monad[eparser] => {
    return a => parser((S)=>[(a,S)]).

    (P >>= F) => parser((S)=>flatten(parse(P,S)//(((a,S1))=>parse(F(a),S1)))).
  }

  public implementation all e,t ~~ concat[parser[t,e]] => {.
    P1 ++ P2 => parser((S)=>parse(P1,S)++parse(P2,S)).
  .}

  public (+++): all e,t ~~ (parser[t,e],parser[t,e])=>parser[t,e].
  p+++q => let{
    first([])=>[].
    first([E,.._])=>[E].
  } in parser((S)=>first(parse(p++q,S))).


  public implementation all e ~~ monadZero[all t~~(t)~>parser[t,e]] => {
    zed = parser((_)=>[]).
  }

  public many:all e,t ~~ (parser[t,e]) => parser[t,list[e]].
  many(P) => many1(P) +++ return [].

  public many1:all e,t ~~ (parser[t,e]) => parser[t,list[e]].
  many1(P) =>
    P >>= (A)=> many(P) >>= (As) => return [A,..As].

  public chainl:all e,t ~~ (parser[t,e],parser[t,(e,e)=>e],e)=>parser[t,e].
  chainl(P,Op,A) => chainl1(P,Op)+++return A.

  public chainl1:all e,t ~~ (parser[t,e],parser[t,(e,e)=>e])=>parser[t,e].
  chainl1(P,Op) => let{
    rest(A) => (Op >>= (F)=> P >>= (B) => rest(F(A,B))) +++ return A
  } in (P >>= rest).

  public spaces:parser[integer,()].
  spaces = many(sat(isSpace)) >>= (_) => return ().

  public skip:all e ~~ (parser[integer,e])=>parser[integer,e].
  skip(P) => spaces >>= (_) => P
}
