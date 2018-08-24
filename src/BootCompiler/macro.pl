:- module(macro,[macroRewrite/2,mkWhere/3,mkWherePtn/4,mkWhereEquality/2,
    hasPromotion/1,promoteOption/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(display).

macroRewrite(Stmts,Reslt) :-
  rewriteStmts(Stmts,Reslt),!.
  % displayAll(Reslt).

rewriteStmts([],[]).
rewriteStmts([St|More],[StX|Stmts]) :-
  rewriteStmt(St,StX),
  rewriteStmts(More,Stmts).

rewriteStmt(St,StX) :-
  isParsingRule(St,_,_,_),!,
  genParserRule(St,StX).
  %display(StX).
rewriteStmt(X,X).

% handle grammar notation
genParserRule(Rl,St) :-
  isParsingRule(Rl,Lc,Hd,Rhs),
  genBody(Rhs,Body),
  (isRoundTerm(Hd,_,_) -> binary(Lc,"=>",Hd,Body,St) ; binary(Lc,"=",Hd,Body,St)).

genBody(B,Bd) :-
  isRoundTuple(B,_,Els),
  reComma(Els,BB),
  genBody(BB,Bd).
genBody(B,Bd) :-
  isBinary(B,Lc,",",L,R),
  genCall(L,Bnd,LL),
  genBody(R,RR),
  genBind(Bnd,Lc,LL,RR,Bd).
genBody(B,Bd) :-
  isBinary(B,Lc,"^^",L,R),
  unary(Lc,"return",R,RR),
  genCall(L,Bnd,LL),
  genBind(Bnd,Lc,LL,RR,Bd).
genBody(B,Bd) :-
  genCall(B,_,Bd).

genCall(C,V,Cl) :-
  isRoundTuple(C,_Lc,Els),
  reComma(Els,CC),!,
  genCall(CC,V,Cl).
genCall(C,V,Cl) :-
  isRtn(C,_,V,CC),
  genCall(CC,_,Cl).
genCall(C,void,Cl) :-
  isBinary(C,_,",",_,_),
  genBody(C,Cl).
genCall(T,void,Cl) :-
  isBinary(T,Lc,"|",L,R),
  genBody(L,LL),
  genBody(R,RR),
  binary(Lc,"++",LL,RR,Cl).
genCall(T,void,Cl) :-
  isBinary(T,Lc,"||",L,R),
  genBody(L,LL),
  genBody(R,RR),
  binary(Lc,"+++",LL,RR,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"+",L),
  genBody(L,LL),
  unary(Lc,"_plus",LL,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"*",L),
  genBody(L,LL),
  unary(Lc,"_star",LL,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"\\+",A),!,
  genBody(A,LL),
  unary(Lc,"_neg",LL,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"^+",A),!,
  genBody(A,LL),
  unary(Lc,"_ahead",LL,Cl).
genCall(T,Bnd,Cl) :-
  isSquareTuple(T,_Lc,Els),
  genSquare(Els,Bnd,Cl).
genCall(T,void,Cl) :-
  isString(T,Lc,_),
  unary(Lc,"_str",T,Cl).
genCall(T,void,Cl) :-
  isWhere(T,Lc,P,Cnd),!,
  genCall(P,_,Pr),
  conditional(Lc,Cnd,Pr,name(Lc,"zed"),Cl).
genCall(T,void,T).

genSquare([E],tuple(Lc,"()",[tuple(Lc,"()",Vrs)]),Bd) :-
  locOfAst(E,Lc),
  (isWhere(E,_,Arg,Cond) ; E=Arg,Cond=name(Lc,"true")),
  ptnVars(E,[],Vrs),
  (Vrs=[Vr] -> unary(Lc,"some",Vr,Rhs) ; unary(Lc,"some",tuple(Lc,"()",Vrs),Rhs)),
  genstr("Q",Nm),
  unary(Lc,Nm,Arg,Lhs),
  eqn(Lc,Lhs,Cond,Rhs,Eqn),
  unary(Lc,Nm,name(Lc,"_"),Anon),
  eqn(Lc,Anon,name(Lc,"true"),name(Lc,"none"),Deflt),
  mkLetDef(Lc,[Eqn,Deflt],name(Lc,Nm),Fun),
  unary(Lc,"_test",Fun,Bd).

genBind(void,Lc,LL,RR,Bd) :-
  anonArg(Lc,A),
  genBind(A,Lc,LL,RR,Bd).
genBind(A,Lc,LL,RR,Bd) :-
  binary(Lc,"=>",A,RR,Fn),
  binary(Lc,">>=",LL,Fn,Bd).

isRtn(T,Lc,R,E) :-
  isBinary(T,Lc,"<-",Rh,E),
  isTuple(R,Lc,[Rh]).

anonArg(Lc,tuple(Lc,"()",[name(Lc,"_")])).

ptnVars(T,SoFar,Vrs) :-
  isWhere(T,_,Lhs,Rhs),!,
  ptnVars(Lhs,SoFar,V1),
  condVars(Rhs,V1,Vrs).
ptnVars(name(Lc,V),SoFar,Vrs) :-
  is_member(name(_,V),SoFar) -> Vrs=SoFar ; Vrs = [name(Lc,V)|SoFar].
ptnVars(app(_,_,Args),SoFar,Vrs) :-
  ptnVars(Args,SoFar,Vrs).
ptnVars(tuple(_,_,Els),SoFar,Vrs) :-
  rfold(Els,macro:ptnVars,SoFar,Vrs).
ptnVars(integer(_,_),Vrs,Vrs).
ptnVars(float(_,_),Vrs,Vrs).
ptnVars(string(_,_),Vrs,Vrs).

condVars(C,V,Vx) :-
  isBinary(C,_,"&&",L,R),!,
  condVars(L,V,V0),
  condVars(R,V0,Vx).
condVars(C,V,Vx) :-
  isBinary(C,_,"||",L,R),!,
  condVars(L,V,V0),
  condVars(R,V0,Vx).
condVars(C,V,Vx) :-
  isBinary(C,_,".=",L,_),!,
  condVars(L,V,Vx).
condVars(C,V,Vx) :-
  isBinary(C,_,"^=",L,_),!,
  condVars(L,V,Vx).
condVars(C,V,Vx) :-
  isBinary(C,_,"in",L,_),!,
  condVars(L,V,Vx).
condVars(_,V,V).

mkWherePtn(Lc,Ptn,Ex,Ptrn) :-
  genIden(Lc,V), % create a new variable
  nary(Lc,Ex,[V],Cl), % call pattern generator
  unary(Lc,"some",Ptn,Lhs),
  binary(Lc,"=.",Cl,Lhs,Test), % Ex(V)=.some(Ptn)
  binary(Lc,"where",V,Test,Ptrn).

mkWhereEquality(name(Lc,V),Ptrn) :-
  genIden(Lc,V,VV),
  binary(Lc,"==",VV,name(Lc,V),Test),
  binary(Lc,"where",VV,Test,Ptrn).

mkWhere(Lc,Fn,Ptrn) :-
  genIden(Lc,V),
  unary(Lc,Fn,V,Tst),
  binary(Lc,"where",V,Tst,Ptrn).

hasPromotion(Term) :-
  isRound(Term,_,_,Els),
  is_member(E,Els),
  isUnary(E,_,"^",_),!.

promoteOption(Term,T1) :-
  isRound(Term,Lc,Op,Els),
  promoteArgs(Els,NEls,name(Lc,"true"),Cond),
  roundTerm(Lc,Op,NEls,Reslt),
  conditional(Lc,Cond,Reslt,name(Lc,"none"),T1).

promoteArgs([],[],Cond,Cond) :- !.
promoteArgs([A|As],[V|NAs],C,Cx) :-
  isUnary(A,Lc,"^",AA),!,
  genIden(Lc,V),
  optionMatch(Lc,V,AA,C1),
  extendCondition(Lc,C,C1,C2),
  promoteArgs(As,NAs,C2,Cx).
promoteArgs([A|As],[A|NAs],C,Cx) :-
  promoteArgs(As,NAs,C,Cx).

extendCondition(_,C,name(_,"true"),C) :-!.
extendCondition(Lc,C,C0,Cx) :-
  binary(Lc,"&&",C0,C,Cx).
