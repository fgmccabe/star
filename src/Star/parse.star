star.parse{
  import star.

  public all e,s ~~ parser[s,e] ::= parser((s)=>cons[(e,s)]).

  public parse:all e,s ~~ (parser[s,e],s) => cons[(e,s)].
  parse(parser(P),S) => P(S).

  pick:all s,t ~~ stream[s->>t] |: (s) => cons[(t,s)].
  pick([C,..L]) => [(C,L)].
  pick([]) => [].

  public _item:all s,t ~~ stream[s->>t] |: parser[s,t].
  _item=parser(pick).

  public _hed:all e,t ~~ (parser[t,e])=>parser[t,e].
  _hed(P) => let{
    hd([],_) => [].
    hd([(F,_),.._],S) => [(F,S)].
  } in parser((S)=>hd(parse(P,S),S)).

  public _sat:all s,t ~~ stream[s->>t] |: ((t)=>boolean) => parser[s,t].
  _sat(T) => _item >>= (Ch) => (T(Ch) ? (return Ch) || epsilon).

  public _test:all p,s,t ~~ stream[s->>t] |: ((t)=>option[p]) => parser[s,p].
  _test(P) => _item >>= (Ch) => (X^=P(Ch) ? (return X) || epsilon).

  public _pred:all p,s,t ~~ stream[s->>t] |: (()=>option[p]) => parser[s,p].
  _pred(P) => let{
    check(S) where X^=P() => [(X,S)].
    check(_) => [].
  } in parser(check).

  public _tk:all s,t ~~ stream[s->>t], equality[t]|:(t)=>parser[s,t].
  _tk(Chr) => _sat((Ch)=>Ch==Chr).

  public _literal:all s,t ~~ stream[s->>t], equality[t] |: (cons[t]) => parser[s,()].
  _literal([]) => return ().
  _literal([Cx,..L]) => _tk(Cx) >>= (_) => _literal(L).

  public _ahead:all s,t,v ~~ stream[s->>t] |: (parser[s,v]) => parser[s,v].
  _ahead(P) => let{
    hd([],_) => [].
    hd([(F,_),.._],S) => [(F,S)].
    } in parser((S)=>hd(parse(P,S),S)).

  public _neg:all s,t,v ~~ stream[s->>t] |: (parser[s,v]) => parser[s,()].
  _neg(P) => let{
    ng([],S) => [((),S)].
    ng([_,.._],S) => [].
    } in parser((S)=>ng(parse(P,S),S)).

  public _str:(string) => parser[cons[char],()].
  _str(S) => _literal(S::cons[char]).

  public _pKy:all k ~~ (string,k)=>parser[cons[char],k].
  _pKy(K,V) => let{.
    prs([]) => return V.
    prs([Cx,..L]) => _tk(Cx) >>= (_) => prs(L).
 .} in prs(K::cons[char]).
 
  public implementation all t ~~ monad[parser[t]] => {
    return a => parser((S)=>[(a,S)]).

    (P >>= F) => parser((S)=>(parse(P,S)//(((a,S1))=>parse(F(a),S1)))*).
  }

  public implementation all e,t ~~ concat[parser[t,e]] => {
    P1 ++ P2 => parser((S)=>parse(P1,S)++parse(P2,S)).
    _multicat(P) => parserChoice(P).
  }

  parserChoice: all e,t ~~ (cons[parser[t,e]])=>parser[t,e].
  parserChoice(.nil) => parser((_)=>[]).
  parserChoice(cons(P,Ps)) => P++parserChoice(Ps).

  public (+++): all e,t ~~ (parser[t,e],parser[t,e])=>parser[t,e].
  p+++q => let{
    first([])=>[].
    first([E,.._])=>[E].
  } in parser((S)=>first(parse(p++q,S))).

  epsilon:all e,t ~~ parser[e,t].
  epsilon = parser((_)=>[]).

  public _opt:all e,t ~~ (parser[t,e]) => parser[t,e].
  _opt(P) => P +++ epsilon.

  public _star:all e,t ~~ (parser[t,e]) => parser[t,cons[e]].
  _star(P) => _plus(P) ++ (return []).

  public _plus:all e,t ~~ (parser[t,e]) => parser[t,cons[e]].
  _plus(P) =>
    P >>= (A)=> _star(P) >>= (As) => (return [A,..As]).

  public sepby:all a,b,t ~~ (parser[t,a],parser[t,b])=>parser[t,cons[a]].
  sepby(P,Sep) => sepby1(P,Sep) ++ (return []).

  public sepby1:all a,b,t ~~ (parser[t,a],parser[t,b])=>parser[t,cons[a]].
  sepby1(P,Sep) => P >>= (A) => _star(Sep>>=(_)=>P) >>= (AS) => return [A,..AS].

  public chainl:all e,t ~~ (parser[t,e],parser[t,(e,e)=>e],e)=>parser[t,e].
  chainl(P,Op,A) => chainl1(P,Op)++(return A).

  public chainl1:all e,t ~~ (parser[t,e],parser[t,(e,e)=>e])=>parser[t,e].
  chainl1(P,Op) => let{.
    rest(A) => (Op >>= (F)=> P >>= (B) => rest(F(A,B))) ++ (return A)
 .} in (P >>= rest).

  public _pstar:all e,t,u ~~ (parser[t,e],(e,u)=>u,u)=>parser[t,u].
  _pstar(P,Op,Z) => let{.
    prs:(u) => parser[t,u].
    prs(A) => (P >>= (O) => prs(Op(O,A))) ++ (return A)
 .} in prs(Z).

  public _pplus:all t,u ~~ (parser[t,u],(u,u)=>u)=>parser[t,u].
  _pplus(P,Op) => let{.
    prs:(u) => parser[t,u].
    prs(A) => (P >>= (O) => prs(Op(O,A))) ++ (return A)
 .} in (P>>=(Z) => prs(Z)).

  public spaces:parser[cons[char],()].
  spaces = _star(_sat(isSpace)) >>= (_) => return ().

  public space:parser[cons[char],()].
  space = _sat(isSpace) >>= (_) => return ().

  public skip:all e ~~ (parser[cons[char],e])=>parser[cons[char],e].
  skip(P) => spaces >>= (_) => P.

  public digit:parser[cons[char],char].
  digit = _sat(isDigit).

  numeral:parser[cons[char],integer].
  numeral = digit >>= (D) => return digitVal(D).

  public natural:parser[cons[char],integer].
  natural = _pplus(numeral,(d,s)=>s*10+d).

  public decimal:parser[cons[char],integer].
  decimal = (_tk(`-`) >>= (_) => natural >>= (N) => return -N) ++ natural.

  public real:()=>parser[cons[char],float].
  real() => (_tk(`-`) >>= (_) =>
      real() >>= (N) => return -N) +++
  (natural >>= (M) =>
      ((_tk(`.`) >>= (_) =>
	    fraction(M::float,0.1) >>= (F) =>
	      exponent >>= (E) => return F*E) +++ (return M::float))).

  fraction:(float,float) => parser[cons[char],float].
  fraction(SoFar,Scale) =>
    (numeral >>= (D) => fraction(SoFar+Scale*(D::float),Scale*0.1)) +++
    (return SoFar).

  exponent:parser[cons[char],float].
  exponent = (_tk(`e`) >>= (_) =>
		decimal >>= (E) => return 10.0**(E::float)) +++ (return 1.0).
}
