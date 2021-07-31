:- module(macro,[synthesizeConDisplay/5]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).

synthesizeConDisplay(Dspr,ATp,T,[Eqn|Eqs],Eqs) :-
  isRoundTerm(T,Lc,Nm,ElTps),!,
  isName(Nm,Id),
  synthesizeTpDisplays(Dspr,ATp,ElTps,Args,Ds),
  unary(Lc,"ss",string(Lc,","),SS),
  interleave(Ds,SS,DSS),
  isSquareTuple(DSA,Lc,DSS),
  unary(Lc,"ssSeq",DSA,Dargs),
  unary(Lc,"ss",string(Lc,"("),LB),
  unary(Lc,"ss",string(Lc,")"),RB),
  unary(Lc,"ss",string(Lc,Id),NN),
  isSquareTuple(DT,Lc,[NN,LB,Dargs,RB]),
  unary(Lc,"ssSeq",DT,Rhs),
  roundTerm(Lc,Nm,Args,LArg),
  unary(Lc,Dspr,LArg,Lhs),
  eqn(Lc,Lhs,Rhs,Eqn).

synthesizeTpDisplays(_,_,[],[],[]).
synthesizeTpDisplays(Dspr,ATp,[T|Tps],[A|As],[D|Ds]) :-
  synthTpDisplay(Dspr,ATp,T,A,D),!,
  synthesizeTpDisplays(Dspr,ATp,Tps,As,Ds).

synthTpDisplay(Dspr,AT,T,V,D) :-
  sameType(AT,T),!,
  locOfAst(T,Lc),
  genIden(Lc,V),
  unary(Lc,Dspr,V,D).
synthTpDisplay(_,_,T,V,D) :-
  \+ isFuncType(T,_,_,_),!,
  locOfAst(T,Lc),
  genIden(Lc,V),
  unary(Lc,"disp",V,D).

sameType(T1,T2) :-
  isName(T1,N1),
  isName(T2,N1).
sameType(T1,T2) :-
  isSquare(T1,N1,A1),
  isSquare(T2,N1,A2),
  sameTypes(A1,A2).

sameTypes([],[]).
sameTypes([T1|Ts1],[T2|Ts2]) :-
  sameType(T1,T2),
  sameTypes(Ts1,Ts2).

