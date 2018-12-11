:- module(macro,[mkWhere/3,mkWherePtn/4,mkWhereEquality/2,
    hasPromotion/1,promoteOption/2]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(misc).
:- use_module(errors).
:- use_module(display).

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
  roundTerm(Lc,Op,NEls,Rs),
  unary(Lc,"some",Rs,Reslt),
  conditional(Lc,Cond,Reslt,name(Lc,"none"),T1).

promoteArgs([],[],Cond,Cond) :- !.
promoteArgs([A|As],[V|NAs],C,Cx) :-
  isUnary(A,Lc,"^",AA),!,
  genIden(Lc,V),
  optionMatch(Lc,V,AA,C1),
  mergeCond(C,C1,Lc,C2),
  promoteArgs(As,NAs,C2,Cx).
promoteArgs([A|As],[A|NAs],C,Cx) :-
  promoteArgs(As,NAs,C,Cx).
