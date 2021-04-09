:- module(macro,[mkWhere/3,mkWherePtn/4,mkWhereEquality/3,
    hasPromotion/1,promoteOption/2,build_main/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(astdisp).

mkWherePtn(Lc,Ptn,Ex,Ptrn) :-
  genIden(Lc,V), % create a new variable
  nary(Lc,Ex,[V],Cl), % call pattern generator
  unary(Lc,"some",Ptn,Lhs),
  binary(Lc,".=",Lhs,Cl,Test), % Ex(V)=.some(Ptn)
  binary(Lc,"where",V,Test,Ptrn).

mkWhereEquality(Lc,name(ILc,V),Ptrn) :-
  genIden(Lc,V,VV),
  binary(Lc,"==",VV,name(ILc,V),Test),
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
  locOfAst(Term,Lc),
  genIden(Lc,V),
  promoteArgs(Els,NEls,V,XV),
  roundTerm(Lc,Op,NEls,Rs),
  roundTuple(Lc,[V],LA),
  eqn(Lc,LA,Rs,Lm),
  binary(Lc,">>=",XV,Lm,T1).

promoteArgs([],[],_,_) :- !.
promoteArgs([A|As],[V|As],V,AA) :-
  isUnary(A,_,"^",AA),!.
promoteArgs([A|As],[A|NAs],V,XV) :-
  promoteArgs(As,NAs,V,XV).

build_main(As,Bs) :-
  look_for_signature(As,"main",Lc,Ms),
  \+look_for_signature(As,"_main",_,_),!,
  synthesize_main(Lc,Ms,As,Bs).
build_main(A,A).

look_for_signature([St|_],Nm,Lc,Ms) :-
  (isTypeAnnotation(St,Lc,V,T) ;
   (isPublic(St,_,I),isTypeAnnotation(I,Lc,V,T))),
  isIden(V,_,Nm),
  isFunType(T,_,L,_),
  isTuple(L,_,Ms),!.
look_for_signature([_|As],Nm,Lc,Ms) :-
  look_for_signature(As,Nm,Lc,Ms).

synthesize_main(Lc,Ts,As,[MainTp,Main|As]) :-
  synthesize_coercions(Ts,Vs,Cs),
  list_pttrn(Lc,Vs,Arg),
  roundTerm(Lc,name(Lc,"_main"),[Arg],Lhs),
  roundTerm(Lc,name(Lc,"main"),Cs,Rhs),
  unary(Lc,"valof",Rhs,MnCall),
  eqn(Lc,Lhs,MnCall,Main),
  squareTerm(Lc,name(Lc,"cons"),[name(Lc,"string")],T1),
  roundTuple(Lc,[T1],T3),
  roundTuple(Lc,[],Unit),
  binary(Lc,"=>",T3,Unit,TU),
  binary(Lc,":",name(Lc,"_main"),TU,MainTp).
%  astDisp(Main),
%  astDisp(MainTp).
  
synthesize_coercions([],[],[]).
synthesize_coercions([T|Ts],[V|Vs],[C|Cs]) :-
  locOfAst(T,Lc),
  genIden(Lc,V),
  coerce(Lc,V,T,C),
  synthesize_coercions(Ts,Vs,Cs).

list_pttrn(Lc,[],Arg) :-
  isSquareTuple(Arg,Lc,[]),!.
list_pttrn(Lc,Ts,Arg) :-
  reComma(Ts,As),
  isSquareTuple(Arg,Lc,[As]).

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
  \+ isFunType(T,_,_,_),!,
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

