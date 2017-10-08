:- module(matcher,[compileMatch/3,functionMatcher/3]).

:- use_module(canon).
:- use_module(errors).
:- use_module(types).
:- use_module(misc).
:- use_module(location).
:- use_module(freevars).


functionMatcher(Tp,Eqns,Reslt) :-
  typeArity(Tp,Ar),
  genVars(Ar,NVrs),
  makeTriples(Eqns,matcher:makeEqnTriple,Tpls),
  firstLoc(Eqns,Lc,Nm)
  compileMatch(Lc,NVrs,Tpls,matcher:genFunction(Nm,Lc),Reslt).


argMode(v(_,_),inVars).
argMode(intLit(_),inScalars).
argMode(floatLit(_),inScalars).
argMode(stringLit(_),inScalars).
argMode(where(T,_),M) :- argMode(T,M).
argMode(enumLit(_),inConstructors).
argMode(consApply(_,_),inConstructors).


firstLoc(Eqns,Nm,Lc) :- is_member(equation(Lc,Nm,_,_,_),Eqns),!.

makeEqnTriple(equation(Lc,_,Args,Cond,Value),(Args,Cond,(Lc,Value))).

makeTriples([],_,[]).
makeTriples([Rl|L],C,[Tr|LL]) :-
  call(C,Rl,Tr),
  makeTriples(L,C,LL).

genVars(0,_,[]).
genVars(Ar,Lc,[v(Lc,NN)|LL]) :-
  Ar>0,
  genstr("_",NN),
  Ar1 is Ar-1,
  genVars(Ar1,Lc,LL).
