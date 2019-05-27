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
  locOfAst(Term,Lc),
  genIden(Lc,V),
  promoteArgs(Els,NEls,V,XV),
  roundTerm(Lc,Op,NEls,Rs),
  roundTuple(Lc,[V],LA),
  eqn(Lc,LA,name(Lc,"true"),Rs,Lm),
  binary(Lc,">>=",XV,Lm,T1).

promoteArgs([],[],_,_) :- !.
promoteArgs([A|As],[V|As],V,AA) :-
  isUnary(A,_,"^",AA),!.
promoteArgs([A|As],[A|NAs],V,XV) :-
  promoteArgs(As,NAs,V,XV).
