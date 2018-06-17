:- module(macro,[macroRewrite/2,mkWhere/3,mkWherePtn/4,mkWhereEquality/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(display).

macroRewrite(Stmts,Reslt) :-
  rewriteStmts(Stmts,Reslt),!.
%  displayAll(Reslt).

rewriteStmts([],[]).
rewriteStmts([St|More],[StX|Stmts]) :-
  rewriteStmt(St,StX),
  rewriteStmts(More,Stmts).

rewriteStmt(St,StX) :-
  isParsingRule(St,_,_,_),!,
  genParserRule(St,StX).
rewriteStmt(X,X).

% handle grammar notation
genParserRule(Rl,St) :-
  isParsingRule(Rl,Lc,Hd,Rhs),
  genBody(Rhs,Body),
  (isRoundTerm(Hd,_,_) -> binary(Lc,"=>",Hd,Body,St) ; binary(Lc,"=",Hd,Body,St)).

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
  isRtn(C,_,V,CC),
  genCall(CC,_,Cl).
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
  isSquareTuple(T,Lc,_Els),
  unary(Lc,"_literal",T,Cl).
genCall(T,void,Cl) :-
  isString(T,Lc,_),
  unary(Lc,"_str",T,Cl).
genCall(T,void,T).

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
