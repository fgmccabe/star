:- module(parse,[genParser/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(display).


% handle grammar notation

genParser(B,Bd) :-
  isBinary(B,Lc,";",L,R),
  genCall(L,Bnd,LL),
  genParser(R,RR),
  genBind(Bnd,Lc,LL,RR,Bd).
genParser(B,Bd) :-
  isBinary(B,Lc,"^^",L,R),
  unary(Lc,"return",R,RR),
  genCall(L,Bnd,LL),
  genBind(Bnd,Lc,LL,RR,Bd).
genParser(B,Bd) :-
  isParen(B,I),!,
  genParser(I,Bd).
genParser(B,Bd) :-
  genCall(B,_,Bd).

genCall(C,V,Cl) :-
  isRtn(C,_,V,CC),
  genCall(CC,_,Cl).
genCall(C,void,Cl) :-
  isBinary(C,_,";",_,_),
  genParser(C,Cl).
genCall(T,void,Cl) :-
  isBinary(T,Lc,"|",L,R),
  genParser(L,LL),
  genParser(R,RR),
  binary(Lc,"++",LL,RR,Cl).
genCall(T,void,Cl) :-
  isBinary(T,Lc,"||",L,R),
  genParser(L,LL),
  genParser(R,RR),
  binary(Lc,"+++",LL,RR,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"+",L),
  genParser(L,LL),
  unary(Lc,"_plus",LL,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"*",L),
  genParser(L,LL),
  unary(Lc,"_star",LL,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"\\+",A),!,
  genParser(A,LL),
  unary(Lc,"_neg",LL,Cl).
genCall(T,void,Cl) :-
  isUnary(T,Lc,"^+",A),!,
  genParser(A,LL),
  unary(Lc,"_ahead",LL,Cl).
genCall(T,Bnd,Cl) :-
  isSquareTuple(T,Lc,Els),
  genSquare(Lc,Els,Bnd,Cl).
genCall(T,void,Cl) :-
  isString(T,Lc,_),
  unary(Lc,"_str",T,Cl).
genCall(T,V,Cl) :-
  isBraceTuple(T,Lc,[C]),!,
  genCond(Lc,C,V,Cl).
genCall(T,V,Cl) :-
  isParen(T,I),!,
  genCall(I,V,Cl).
genCall(T,void,T).

genSquare(_,[E],V,Bd) :-
  locOfAst(E,Lc),
  (isWhere(E,_,Arg,Cond) ; E=Arg,Cond=name(Lc,"true")),
  ptnVars(E,[],Vrs),
  roundTuple(Lc,Vrs,VV),
  (Vrs=[Vr] ->
    unary(Lc,"some",Vr,Rhs), V=VV;
    unary(Lc,"some",VV,Rhs), roundTuple(Lc,[VV],V)),
  genstr("Q",Nm),
  unary(Lc,Nm,Arg,Lhs),
  eqn(Lc,Lhs,Cond,Rhs,Eqn),
  unary(Lc,Nm,name(Lc,"_"),Anon),
  eqn(Lc,Anon,name(Lc,"true"),name(Lc,"none"),Deflt),
  mkLetDef(Lc,[Eqn,Deflt],name(Lc,Nm),Fun),
  unary(Lc,"_test",Fun,Bd).
genSquare(Lc,[],V,name(Lc,"zed")) :-
  roundTuple(Lc,[],V).

genCond(Lc,C,V,Cl) :-
  condVars(C,[],Vrs),
  roundTuple(Lc,Vrs,Arg),
  (Vrs=[Vr] ->
    unary(Lc,"some",Vr,Rhs), V=Arg;
    unary(Lc,"some",Arg,Rhs), roundTuple(Lc,[Arg],V)),
  genstr("P",Nm),
  zeroary(Lc,Nm,Empty),
  eqn(Lc,Empty,C,Rhs,E1),
  eqn(Lc,Empty,name(Lc,"true"),name(Lc,"none"),E2),
  mkLetDef(Lc,[E1,E2],name(Lc,Nm),Fun),
  unary(Lc,"_pred",Fun,Cl).

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
ptnVars(name(_,"_"),SoFar,SoFar) :-!.
ptnVars(name(Lc,V),SoFar,Vrs) :-
  is_member(name(_,V),SoFar) -> Vrs=SoFar ; Vrs = [name(Lc,V)|SoFar].
ptnVars(app(_,_,Args),SoFar,Vrs) :-
  ptnVars(Args,SoFar,Vrs).
ptnVars(tuple(_,_,Els),SoFar,Vrs) :-
  rfold(Els,parse:ptnVars,SoFar,Vrs).
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
  ptnVars(L,V,Vx).
condVars(C,V,Vx) :-
  isBinary(C,_,"^=",L,_),!,
  ptnVars(L,V,Vx).
condVars(C,V,Vx) :-
  isBinary(C,_,"in",L,_),!,
  condVars(L,V,Vx).
condVars(_,V,V).
