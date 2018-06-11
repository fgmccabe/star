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
rewriteStmts([St|More],[St|Stmts]) :-
  rewriteStmts(More,Stmts).

% handle parser notation
rewriteStmt(St,PSt) :-
  isParsingRule(St,Lc,Hd,Rhs),!,
  genParserRule(Hd,Lc,Rhs,PSt).
rewriteStmt(X,X).

genParserRule(Lhs,Lc,Rhs,St) :-
  pickupRtn(Lhs,Hd,Rtn),
  isRoundTerm(Hd,_,_),!,
  genBody(Rhs,Rtn,Body),
  binary(Lc,"=>",Hd,Body).
genParserRule(Lhs,Lc,Rhs,Rt) :-
  pickupRtn(Lhs,Hd,Rtn),
  genBody(Bd,Body),
  genReturn(Rt,Body,Rtn),
  binary(Lc,"=",Hd,Rtn).

genBody(B,Rs,Rx,Rtn) :-
  isRtn(B,Lc,Hd,Rx),!,
  binary(Lc,"=>",Rs,)

isRtn(T,Lc,H,R) :-
  isBinary(T,Lc,H,Rh),
  isTuple(R,Lc,[Rh]).

pickupRtn(T,H,R) :-
  isBinary(T,Lc,"^",H,A),!,
  isTuple(R,Lc,[A]).
pickupRtn(T,T,A) :-
  locOfAst(T,Lc),
  isTuple(A,Lc,[name(Lc,"_")]).

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
