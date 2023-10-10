:- module(grammar,[grammarMacro/3,
		   makeGrammar/2,
		   grammarCallMacro/3,
		   grammarTypeMacro/3]).

:- use_module(astdisp).
:- use_module(operators).
:- use_module(abstract).
:- use_module(location).
:- use_module(errors).
:- use_module(misc).
:- use_module(wff).

/*
  * implement grammar notation, using rules of the form:

  NT(Ptns) >> Val --> [Term], NT(Exp)>>Ptn, {Test}, || ... || [] ||...

  The type of such a rule looks like:

  all s,t ~~ stream[s->>t],hasLoc[t] |: (s,Ptypes) => (s,option[Valtype])
  */


parseRule(A,grRule(Lc,Nm,Args,Cond,Deflt,Val,Body)) :-
  isBinary(A,Lc,"-->",L,R),
  parseHead(L,Nm,Args,Cond,Val,Deflt),
  parseBody(R,Body),!.

parseHead(A,Nm,Args,R,Val,Deflt) :-
  isWhere(A,_,L,R),!,
  parseHead(L,Nm,Args,_,Val,Deflt).
parseHead(A,Nm,Args,Cond,R,Deflt) :-
  isBinary(A,_,">>",L,R),!,
  parseHead(L,Nm,Args,Cond,_,Deflt).
parseHead(A,Nm,Args,Cond,Val,true) :-
  isDefault(A,_,L),!,
  parseHead(L,Nm,Args,Cond,Val,_Deflt).
parseHead(A,Nm,Args,none,Unit,false) :-
  isRoundTerm(A,Op,Args),
  isIden(Op,Nm),!,
  locOfAst(A,Lc),
  unitTpl(Lc,Unit).
parseHead(A,Nm,[],none,Unit,false) :-
  isIden(A,Nm),!,
  locOfAst(A,Lc),
  unitTpl(Lc,Unit).

parseBody(A,dis(Lc,Lft,Rgt)) :-
  isBinary(A,Lc,"|",L,R),!,
  parseBody(L,Lft),
  parseBody(R,Rgt).
parseBody(A,seq(Lc,Lft,Rgt)) :-
  isComma(A,Lc,L,R),!,
  parseBody(L,Lft),
  parseBody(R,Rgt).
parseBody(A,neg(Lc,Rgt)) :-
  isNegation(A,Lc,R),!,
  parseBody(R,Rgt).
parseBody(A,epsilon(Lc)) :-
  isSquareTuple(A,Lc,[]),!.
parseBody(A,Ts) :-
  isSquareTuple(A,_Lc,Els),!,
  parseTerminals(Els,Ts).
parseBody(A,B) :-
  isTuple(A,_Lc,[El]),!,
  parseBody(El,B).
parseBody(A,test(Lc,El)) :-
  isBraceTuple(A,Lc,[El]),!.
parseBody(A,rep(Lc,B)) :-
  isUnary(A,Lc,"*",L),!,
  parseBody(L,B).
parseBody(A,sep(Lc,Lft,Rgt)) :-
  isBinary(A,Lc,"*",L,R),!,
  parseBody(L,Lft),
  parseBody(R,Rgt).
parseBody(A,prod(Lc,NT,B)) :-
  isBinary(A,Lc,">>",L,B),!,
  parseBody(L,NT).
parseBody(A,fail(Lc)) :-
  isName(A,Lc,"fail"),!.
parseBody(A,eof(Lc)) :-
  isName(A,Lc,"end"),!.
parseBody(A,P) :-
  isRoundTuple(A,_Lc,Els),!,
  reComma(Els,I),
  parseBody(I,P).
parseBody(A,P) :-
  isString(A,Lc,Txt),
  string_chars(Txt,Cs),
  makeTextTerminals(Cs,Lc,P).
parseBody(A,B) :-
  parseNonTerminal(A,B).

parseNonTerminal(A,nonterm(Lc,Nm,Args)) :-
  isRoundTerm(A,Lc,Op,Args),!,
  isName(Op,Nm),!.
parseNonTerminal(A,nonterm(Lc,Nm,[])) :-
  isName(A,Lc,Nm),!.
parseNonTerminal(A,epsilon(Lc)) :-
  locOfAst(A,Lc),!,
  reportError("invalid grammar symbol %s",[ast(A)],Lc).

parseTerminals([A],term(Lc,A)) :-
  locOfAst(A,Lc),!.
parseTerminals([A|As],seq(Lc,term(Lc,A),B)) :-
  locOfAst(A,Lc),
  parseTerminals(As,B).

makeTextTerminals([],Lc,epsilon(Lc)) :-!.
makeTextTerminals([C],Lc,term(Lc,Ch)) :- mkChar(Lc,C,Ch),!.
makeTextTerminals([C|Cs],Lc,seq(Lc,term(Lc,C),Cx)) :-
  makeTextTerminals(Cs,Lc,Cx).

dispRules([]) :- !.
dispRules([Rl|Rls]) :-
  dispRule(Rl),!,
  dispRules(Rls).

dispRule(Rl) :- dispRule(Rl,Chrs,[]), string_chars(Res,Chrs), writeln(Res).

dispRule(grRule(Lc,Nm,Args,Cond,Deflt,Val,Body),O,Ox) :-
  dispNonTerm(Lc,Nm,Args,O,O2),
  (Deflt=true -> appStr(" default ",O2,O3) ; O2=O3),
  appStr(">>",O3,O4),
  dispAst(Val,1200,O4,O5),
  (Cond=none -> O5=O6 ;
   Cond=some(C),
   appStr(" where ",O5,O7),
   dispAst(C,1100,O7,O6)),
  appStr(" --> ",O6,O8),
  dispSeq(Body,O8,Ox).

dispBody(dis(_,Lft,Rgt),O,Ox) :-
  appStr("(",O,O1),
  dispBody(Lft,O1,O2),
  appStr(" | ",O2,O3),
  dispBody(Rgt,O3,O4),
  appStr(")",O4,Ox).
dispBody(seq(Lc,Lft,Rgt),O,Ox) :-
  appStr("(",O,O1),
  dispSeq(seq(Lc,Lft,Rgt),O1,O2),
  appStr(")",O2,Ox).
dispBody(neg(_,Rgt),O,Ox) :-
  appStr(" ~ ",O,O1),
  dispBody(Rgt,O1,Ox).
dispBody(epsilon(_),O,Ox) :-
  appStr(" [] ",O,Ox).
dispBody(fail(_),O,Ox) :-
  appStr(" fail ",O,Ox).
dispBody(eof(_),O,Ox) :-
  appStr(" end ",O,Ox).
dispBody(test(_,T),O,Ox) :-
  appStr("{",O,O1),
  dispAst(T,2000,O1,O2),
  appStr("}",O2,Ox).
dispBody(prod(_,N,T),O,Ox) :-
  dispBody(N,O,O1),
  appStr(">>",O1,O2),
  dispAst(T,2000,O2,Ox).
dispBody(rep(_,Lft),O,Ox) :-
  dispBody(Lft,O,O2),
  appStr(" * ",O2,Ox).
dispBody(sep(_,Lft,Rgt),O,Ox) :-
  dispBody(Lft,O,O2),
  appStr(" * ",O2,O3),
  dispBody(Rgt,O3,Ox).
dispBody(nonterm(Lc,Nm,Args),O,Ox) :-
  dispNonTerm(Lc,Nm,Args,O,Ox).
dispBody(term(_,T),O,Ox) :-
  appStr("[",O,O1),
  dispAst(T,1000,O1,O2),
  appStr("]",O2,Ox).

dispSeq(seq(_,Lft,Rgt),O,Ox) :-!,
  dispBody(Lft,O,O2),
  appStr(" , ",O2,O3),
  dispSeq(Rgt,O3,Ox).
dispSeq(B,O,Ox) :-
  dispBody(B,O,Ox).


dispNonTerm(_,Nm,[],O,Ox) :-!,
  appStr(Nm,O,Ox).
dispNonTerm(Lc,Nm,Args,O,Ox) :-
  appStr(Nm,O,O1),
  dispAst(tuple(Lc,"()",Args),2000,O1,Ox).

makeRule(grRule(Lc,Nm,Args,Cond,Deflt,_,fail(_)),Rl) :-
  genIden(Lc,"Str",Str),
  mkEnum(Lc,"none",Fl),
  buildEquation(Lc,name(Lc,Nm),[Str|Args],Cond,Deflt,Fl,Rl).
makeRule(grRule(Lc,Nm,Args,Cond,Deflt,Val,Body),Rl) :-
  genIden(Lc,"Nxt",Nxt),
  genIden(Lc,"Str",Str),
  makeBody(Body,Str,Nxt,none,Test),
  mergeCond(Lc,Cond,some(Test),Cnd),
  roundTuple(Lc,[Val,Nxt],A),
  mkConApply(Lc,name(Lc,"some"),[A],Rs),
  buildEquation(Lc,name(Lc,Nm),[Str|Args],Cnd,Deflt,Rs,Rl).

makeBody(epsilon(Lc),Str,Nxt,none,B) :-!,
  match(Lc,Nxt,Str,B).
makeBody(epsilon(Lc),Str,Nxt,some(V),B) :-!,
  reportError("Not permitted to produce %s here",[ast(V)],Lc),
  match(Lc,Nxt,Str,B).
makeBody(term(Lc,T),Str,Nxt,none,B) :- !,
  hdtl(Lc,T,Nxt,Str,B).
makeBody(term(Lc,T),Str,Nxt,some(V),B) :- !,
  hdtl(Lc,T,Nxt,Str,B1),
  unary(Lc,"_value",T,TV),
  match(Lc,V,TV,B2),
  conjunct(Lc,B1,B2,B).
makeBody(nonterm(Lc,Nm,Args),Str,Nxt,none,B) :- !,
  mkAnon(Lc,Anon),
  roundTerm(Lc,name(Lc,Nm),[Str|Args],NT),
  resultOf(Lc,Anon,Nxt,NT,B).
makeBody(nonterm(Lc,Nm,Args),Str,Nxt,some(V),B) :- !,
  roundTerm(Lc,name(Lc,Nm),[Str|Args],NT),
  resultOf(Lc,V,Nxt,NT,B).
makeBody(prod(_,I,P),Str,Nxt,none,B) :-!,
  makeBody(I,Str,Nxt,some(P),B).
makeBody(prod(Lc,B,P),Str,Nxt,some(V),B) :-!,
  reportError("conflicting production: %s cannot override %s",[ast(P),ast(V)],Lc),
  makeBody(B,Str,Nxt,some(V),B).
makeBody(test(Lc,T),Str,Nxt,none,B) :-!,
  match(Lc,Nxt,Str,B1),
  conjunct(Lc,T,B1,B).
makeBody(test(Lc,T),Str,Nxt,some(V),B) :-!,
  reportError("cannot produce %s from %s",[ast(V),ast(T)],Lc),
  match(Lc,Nxt,Str,B1),
  conjunct(Lc,T,B1,B).
makeBody(fail(Lc),_Str,_Nxt,_,B) :-
  mkEnum(Lc,"false",B).
makeBody(eof(Lc),Str,Nxt,_,B) :-
  unary(Lc,"_eof",Str,B1),
  match(Lc,Nxt,Str,B2),
  conjunct(Lc,B1,B2,B).
makeBody(seq(Lc,L,R),Str,Nxt,V,B) :-!,
  genIden(Lc,"I",I),
  makeBody(L,Str,I,none,B1),
  makeBody(R,I,Nxt,V,B2),
  conjunct(Lc,B1,B2,B).
makeBody(dis(Lc,L,R),Str,Nxt,V,B) :-!,
  makeBody(L,Str,Nxt,V,B1),
  makeBody(R,Str,Nxt,V,B2),
  disjunct(Lc,B1,B2,B).
makeBody(neg(Lc,R),Str,Nxt,none,B) :-!,
  genIden(Lc,"I",I),
  makeBody(R,Str,I,none,Ng),
  negation(Lc,Ng,B1),
  match(Lc,Nxt,Str,B2),
  conjunct(Lc,B1,B2,B).
makeBody(neg(Lc,N),Str,Nxt,some(V),B) :-!,
  reportError("cannot produce %s from negate %s",[ast(V),ast(N)],Lc), 
  makeBody(neg(Lc,N),Str,Nxt,none,B).
makeBody(rep(Lc,L),Str,Nxt,V,B) :-!,
  /*
    let{.
    s(S0,SoF) where (X,S1)?=lft(S0) => s(S1,[X,..SoF]).
    s(S0,SoF) => .some((reverse(SoF),S0))
    .} in (V,Nxt) ?= s(S0,[]))
  */
  genIden(Lc,"s",S),
  genIden(Lc,"S0",S0),
  genIden(Lc,"S1",S1),
  genIden(Lc,"SoF",SoF),
  genIden(Lc,"X",X),
  makeBody(L,S0,S1,some(X),Lft),
  mkConApply(Lc,name(Lc,"cons"),[X,SoF],SoF1),
  roundTerm(Lc,S,[S1,SoF1],Val),
  buildEquation(Lc,S,[S0,SoF],some(Lft),false,Val,Eq1),
  unary(Lc,"reverse",SoF,Rslt),
  roundTuple(Lc,[Rslt,S0],T1),
  mkConApply(Lc,name(Lc,"some"),[T1],Val2),
  buildEquation(Lc,S,[S0,SoF],none,true,Val2,Eq2),
  mkEnum(Lc,"nil",Nil),
  roundTerm(Lc,S,[Str,Nil],BB),
  (some(VV) = V ; mkAnon(Lc,VV)),
  mkLetRec(Lc,[Eq1,Eq2],BB,Rh),
  roundTuple(Lc,[VV,Nxt],T2),
  optionMatch(Lc,T2,Rh,B).
makeBody(sep(Lc,L,R),Str,Nxt,V,B) :-
  /*
    let{.
    s(S0,SoF) where (_,S1) ?= right(S0) && (X,S2)?=lft(S1) => s(S2,[X,..SoF]).
    s(S0,SoF) => .some((reverse(SoF),S0))
    .} in ((S0,F)?=lft(Str) && (V,Nxt) ?= s(S0,[F]))

  */
  genIden(Lc,"s",S),
  genIden(Lc,"S0",S0),
  genIden(Lc,"S1",S1),
  genIden(Lc,"S2",S2),
  genIden(Lc,"Si",Si),
  genIden(Lc,"SoF",SoF),
  genIden(Lc,"X",X),
  genIden(Lc,"Fs",Fs),
  makeBody(R,S0,S1,none,Rgt),
  makeBody(L,S1,S2,some(X),Lft),
  mkConApply(Lc,name(Lc,"cons"),[X,SoF],SoF1),
  roundTerm(Lc,S,[S2,SoF1],Val),
  conjunct(Lc,Rgt,Lft,Tst),
  buildEquation(Lc,S,[S0,SoF],some(Tst),false,Val,Eq1),
  unary(Lc,"reverse",SoF,Rslt),
  roundTuple(Lc,[Rslt,S0],T1),
  mkConApply(Lc,name(Lc,"some"),[T1],Val2),
  buildEquation(Lc,S,[S0,SoF],none,true,Val2,Eq2),
  makeBody(L,Str,Si,some(Fs),AA),
  mkEnum(Lc,"nil",Nil),
  mkConApply(Lc,name(Lc,"cons"),[Fs,Nil],Frst),
  roundTerm(Lc,S,[Si,Frst],BB),
  mkLetRec(Lc,[Eq1,Eq2],BB,B1),
  (some(VV) = V ; mkAnon(Lc,VV)),
  roundTuple(Lc,[VV,Nxt],Vl),
  optionMatch(Lc,Vl,B1,B2),
  conjunct(Lc,AA,B2,B).

resultOf(Lc,T,Nxt,S,B) :-
  roundTuple(Lc,[T,Nxt],A),
  optionMatch(Lc,A,S,B).
  
hdtl(Lc,T,Nxt,Str,B) :-
  unary(Lc,"_hdtl",Str,S),
  resultOf(Lc,T,Nxt,S,B).

grammarMacro(A,statement,Ax) :-
  parseRule(A,Rl),!,
%  dispRule(Rl),
  makeRule(Rl,Ax),
  dispAst(Ax).

makeGrammar(Sts,Sx) :-
  collectGrRules(Sts,Oth,rls{},Mp),
  dict_pairs(Mp,_,Prs),
  makeGrs(Prs,Grs,[]),
  concat(Oth,Grs,Sx).

collectGrRules([],[],Mp,Mp) :-!.
collectGrRules([St|Ss],Rls,Mp,Mpx) :-
  isBinary(St,_,"-->",_,_),!,
  parseRule(St,Rl),
%  dispRule(Rl),
  addRule(Rl,Mp,Mp1),
  collectGrRules(Ss,Rls,Mp1,Mpx).
collectGrRules([St|Ss],[St|Rls],Mp,Mpx) :-
  collectGrRules(Ss,Rls,Mp,Mpx).

addRule(Rl,Mp,Mp1) :-
  ruleName(Rl,Nm),
  makeKey(Nm,Ky),
  (get_dict(Ky,Mp,Rls) ->
   put_dict(Ky,Mp,[Rl|Rls],Mp1);
   put_dict(Ky,Mp,[Rl],Mp1)).

ruleName(grRule(_,Nm,_,_,_,_,_),Nm).

makeGrs([],Sx,Sx) :-!.
makeGrs([_-Rs|Prs],S,Sx) :-
  (hasDefault(Rs) ->
   reverse(Rs,RRs) ;
   defaultRl(Rs,Def),
   reverse([Def|Rs],RRs)),
%  dispRules(RRs),
  makeGrRls(RRs,S,S0),
  makeGrs(Prs,S0,Sx).

hasDefault(Rls) :-
  member(grRule(_Lc,_Nm,_Args,_Cond,true,_Val,_Body),Rls),!.

defaultRl([grRule(Lc,Nm,Args,_,_,_,_)|_],
	  grRule(Lc,Nm,Args,none,true,Unit,fail(Lc))) :-
  unitTpl(Lc,Unit).

makeGrRls([],Sx,Sx) :-!.
makeGrRls([Rl|Rs],[Ax|S],Sx) :-
  makeRule(Rl,Ax),
  makeGrRls(Rs,S,Sx).

% (Args)>>P --> S, as a type, becomes
% (S,..Args) => option[(P,S)]

grammarTypeMacro(A,type,Tx) :-
  isBinary(A,Lc,"-->",L,S),
  isBinary(L,_,">>",G,P),
  isTuple(G,_Lc,Els),!,
  roundTuple(Lc,[S|Els],Arg),
  roundTuple(Lc,[P,S],Rs),
  squareTerm(Lc,name(Lc,"option"),[Rs],Rt),
  funcType(Lc,Arg,Rt,Tx).

% NT --> Tks, as an expression, becomes
% ( (Rslt,[]) ?= NT(Tks) ?? .some(Rslt) || .none)
grammarCallMacro(A,expression,Ax) :-
    isParseCall(A,Lc,L,Str),!,
    parseBody(L,Body),
    genIden(Lc,"Nxt",Nxt),
    genIden(Lc,"Rslt",Rslt),
    makeBody(Body,Str,Nxt,some(Rslt),Cond),
    unary(Lc,"_eof",Nxt,End),
    conjunct(Lc,Cond,End,Test),
    mkConApply(Lc,name(Lc,"some"),[Rslt],Rs),
    mkEnum(Lc,"none",El),
    conditional(Lc,Test,Rs,El,Ax).
%    dispAst(Ax).
