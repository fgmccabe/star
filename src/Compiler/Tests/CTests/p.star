test.p{
  import star.

  public all e,s ~~ parser[s,e] ::= parser((s)=>cons[(e,s)]).

  public implementation all t ~~ monad[parser[t]] => {.
    return a => parser((S)=>[(a,S)]).

    (P >>= F) => parser((S)=>multicat(parse(P,S)//(((a,S1))=>parse(F(a),S1)))).
  .}

  public implementation all e,t ~~ concat[parser[t,e]] => {.
    P1 ++ P2 => parser((S)=>parse(P1,S)++parse(P2,S)).
  .}

  public parse:all e,s ~~ (parser[s,e],s) => cons[(e,s)].
  parse(parser(P),S) => P(S).

  pick:all s,t ~~ stream[s->>t] |: (s) => cons[(t,s)].
  pick([C,..L]) => [(C,L)].
  pick([]) => [].

  public _tk:all s,t ~~ stream[s->>t], equality[t]|:(t)=>parser[s,t].
  _tk(Chr) => _sat((Ch)=>Ch==Chr).

  public _item:all s,t ~~ stream[s->>t] |: parser[s,t].
  _item=parser(pick).

  epsilon : all e,t ~~ parser[e,t].
  epsilon = parser((_)=>[]).

  public _sat:all s,t ~~ stream[s->>t] |: ((t)=>boolean) => parser[s,t].
  _sat(T) => _item >>= (Ch) => (T(Ch) ?? (return Ch) || epsilon).

  public real:parser[cons[integer],float].
  real = (_tk(0c-) >>= (_) =>
      real >>= (N) => return -N).

}
