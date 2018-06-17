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

  public _literal:all t ~~ equality[t] |: (list[t]) => parser[t,()].
  _literal([]) => return ().
  _literal([Cx,..L]) => tk(Cx) >>= (_) => _literal(L).

  public _ahead:all t,v ~~ (parser[t,v]) => parser[t,v].
  _ahead(P) => let{
    hd([],_) => [].
    hd([(F,_),.._],S) => [(F,S)].
    } in parser((S)=>hd(parse(P,S),S)).

  public _str:(string) => parser[integer,()].
  _str(S) => _literal(S::list[integer]).

  public _pKy:all k ~~ (string,k)=>parser[integer,k].
  _pKy(K,V) => let{
    prs([]) => return V.
    prs([Cx,..L]) => tk(Cx) >>= (_) => prs(L).
  } in prs(K::list[integer]).

  public implementation all t ~~ monad[parser[t]] => {
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

  public implementation all t ~~ monadZero[parser[t]] => {
    zed = parser((_)=>[]).
  }

  public _star:all e,t ~~ (parser[t,e]) => parser[t,list[e]].
  _star(P) => _plus(P) +++ return [].

  public _plus:all e,t ~~ (parser[t,e]) => parser[t,list[e]].
  _plus(P) =>
    P >>= (A)=> _star(P) >>= (As) => return [A,..As].

  public sepby:all a,b,t ~~ (parser[t,a],parser[t,b])=>parser[t,list[a]].
  sepby(P,Sep) => sepby1(P,Sep) +++ return [].

  public sepby1:all a,b,t ~~ (parser[t,a],parser[t,b])=>parser[t,list[a]].
  sepby1(P,Sep) => P >>= (A) => _star(Sep>>=(_)=>P) >>= (AS) => return [A,..AS].

  public chainl:all e,t ~~ (parser[t,e],parser[t,(e,e)=>e],e)=>parser[t,e].
  chainl(P,Op,A) => chainl1(P,Op)+++return A.

  public chainl1:all e,t ~~ (parser[t,e],parser[t,(e,e)=>e])=>parser[t,e].
  chainl1(P,Op) => let{
    rest(A) => (Op >>= (F)=> P >>= (B) => rest(F(A,B))) +++ return A
  } in (P >>= rest).

  public _pstar:all e,t,u ~~ (parser[t,e],(e,u)=>u,u)=>parser[t,u].
  _pstar(P,Op,Z) => let{
    prs:(u) => parser[t,u].
    prs(A) => (P >>= (O) => prs(Op(O,A))) +++ return A
  } in prs(Z).

  public _pplus:all t,u ~~ (parser[t,u],(u,u)=>u)=>parser[t,u].
  _pplus(P,Op) => let{
    prs:(u) => parser[t,u].
    prs(A) => (P >>= (O) => prs(Op(O,A))) +++ return A
  } in (P>>=(Z) => prs(Z)).

  public spaces:parser[integer,()].
  spaces = _star(sat(isSpace)) >>= (_) => return ().

  public space:parser[integer,()].
  space = sat(isSpace) >>= (_) => return ().

  public skip:all e ~~ (parser[integer,e])=>parser[integer,e].
  skip(P) => spaces >>= (_) => P.

  public digit:parser[integer,integer].
  digit = sat(isDigit).

  public natural:parser[integer,integer].
  natural = _pplus(digit,(d,s)=>s*10+digitVal(d)).
}
