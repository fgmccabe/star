:- module(do,[genAction/2,genValof/2]).

:- use_module(wff).
:- use_module(abstract).
:- use_module(misc).
:- use_module(display).

genAction(Trm,Exp) :-
  isDoTerm(Trm,_Lc,Stmt),!,
  genBody(Stmt,Exp),
  display(Exp).
genAction(Trm,Exp) :-
  isDoTerm(Trm,Lc),
  unary(Lc,"return",tuple(Lc,"()",[]),Exp).

genBody(B,Bd) :-
  isBinary(B,Lc,";",L,R),
  genStmt(L,Bnd,LL),
  genBody(R,RR),
  genBind(Bnd,Lc,LL,RR,Bd).
genBody(B,Bd) :-
  isParen(B,I),!,
  genBody(I,Bd).
genBody(B,Exp) :-
  genStmt(B,_,Exp).

genStmt(C,V,Cl) :-
  isBind(C,_,V,CC),
  genStmt(CC,_,Cl).
genStmt(S,void,Cl) :-
  isBinary(S,_,";",_,_),!,
  genBody(S,Cl).
genStmt(T,V,Cl) :-
  isParen(T,I),!,
  genStmt(I,V,Cl).
genStmt(B,void,Bd) :-
  isBinary(B,Lc,"^^",L,R),
  unary(Lc,"return",R,RR),
  genStmt(L,Bnd,LL),
  genBind(Bnd,Lc,LL,RR,Bd).
genStmt(T,void,Cl) :-
  isUnary(T,Lc,"^^",Exp),
  unary(Lc,"return",Exp,Cl).
genStmt(T,void,T).

genBind(void,Lc,LL,RR,Bd) :-
  anonArg(Lc,A),
  genBind(A,Lc,LL,RR,Bd).
genBind(A,Lc,LL,RR,Bd) :-
  binary(Lc,"=>",A,RR,Fn),
  binary(Lc,">>=",LL,Fn,Bd).

anonArg(Lc,tuple(Lc,"()",[name(Lc,"_")])).

isBind(T,Lc,R,E) :-
  isBinary(T,Lc,"<-",Rh,E),
  isTuple(R,Lc,[Rh]).

genValof(T,E) :-
  isUnary(T,Lc,"valof",A),
  unary(Lc,"_perform",A,E).
