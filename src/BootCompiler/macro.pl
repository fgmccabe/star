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
  genParserRule(Hd,Lc,Rhs,PSt),
  display(PSt).
rewriteStmt(X,X).

genParserRule(Lhs,Lc,Rhs,St) :-
  pickupRtn(Lhs,Hd,Rtn),
  isRoundTerm(Hd,_,_),!,
  genBody(Rhs,Rtn,Body),
  binary(Lc,"=>",Hd,Body,St).
genParserRule(Lhs,Lc,Rhs,St) :-
  pickupRtn(Lhs,Hd,Rtn),
  genBody(Rhs,Rtn,Body),
  binary(Lc,"=",Hd,Body,St).

genBody(B,Rs,Rtn) :-
  isBinary(B,Lc,",",L,R),
  (isRtn(L,LLc,H,A) ->
    genBody(R,Rs,BB),
    binary(LLc,"=>",A,BB,BBC),
    genCall(H,Cl),
    binary(Lc,">>=",Cl,BBC,Rtn);
   anonArg(Lc,A),
   genBody(R,Rs,BB),
   binary(Lc,"=>",A,BB,BBC),
   genCall(L,Cl),
   binary(Lc,">>=",Cl,BBC,Rtn)).
genBody(T,Rs,Cl) :-
  isBinary(T,Lc,"|",L,R),
  genBody(L,Rs,LL),
  genBody(R,Rs,RR),
  binary(Lc,"++",LL,RR,Cl).
genBody(T,Rs,Cl) :-
  isBinary(T,Lc,"||",L,R),
  genBody(L,Rs,LL),
  genBody(R,Rs,RR),
  binary(Lc,"+++",LL,RR,Cl).
genBody(T,Rs,Cl) :-
  isUnary(T,Lc,"\\+",L),
  genBody(L,void,LL),
  unary(Lc,"_noparse",LL,Cl).
genBody(T,Rs,Cl) :-
  isUnary(T,Lc,"+",L),
  genBody(L,Rs,LL),
  unary(Lc,"_plus",LL,Cl).
genBody(T,Rs,Cl) :-
  isUnary(T,Lc,"*",L),
  genBody(L,Rs,LL),
  unary(Lc,"_star",LL,Cl).
genBody(B,void,B) :-
 (isRtn(B,Lc,_H,_A) ->
   reportError("parser nt may not have output %s",[B],Lc);
   true).
genBody(B,Rs,Rtn) :-
  locOfAst(B,RLc),
  unary(RLc,"return",Rs,RR),
  (isRtn(B,Lc,Hd,A) ->
    genCall(Hd,Cl),
    binary(Lc,"=>",A,RR,BB),
    binary(RLc,">>=",Cl,BB,Rtn);
   anonArg(RLc,A),
   genCall(B,Cl),
   binary(RLc,"=>",A,RR,BB),
   binary(RLc,">>=",Cl,BB,Rtn)).

genCall(T,Cl) :-
  isSquareTuple(T,Lc,_Els),
  unary(Lc,"_literal",T,Cl).
genCall(T,Cl) :-
  isString(T,Lc,Txt),
  unary(Lc,"_str",Txt,Cl).
genCall(T,T).

isRtn(T,Lc,H,R) :-
  isBinary(T,Lc,"^",H,Rh),
  isTuple(R,Lc,[Rh]).

pickupRtn(T,H,R) :-
  isRtn(T,_,H,R),!.
pickupRtn(T,T,void).

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
