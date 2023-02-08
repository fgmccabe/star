:-module(freevars,[freeVars/5,ptnVars/3,freeVarsInAction/5]).

:- use_module(canon).
:- use_module(errors).
:- use_module(misc).
:- use_module(escapes).
:- use_module(types).

%! freeVars(++,++,++,++,--) is det.
freeVars(v(_,Nm,_),Ex,Q,F,Fv) :-
  \+isEscape(Nm,_),!,
  call(Q,Ex,Nm,F,Fv).
freeVars(v(_,_,_),_Ex,_Q,Fv,Fv).
freeVars(anon(_,_),_,_,F,F).
freeVars(enm(_,_,_),_,_,F,F).
freeVars(intLit(_,_),_,_,F,F).
freeVars(bigLit(_,_),_,_,F,F).
freeVars(floatLit(_,_),_,_,F,F).
freeVars(charLit(_,_),_,_,F,F).
freeVars(stringLit(_,_),_,_,F,F).
freeVars(tple(_,Els),Ex,Q,F,FV) :- freeVarsList(Els,Ex,Q,F,FV).
freeVars(apply(_,Op,A,_),Ex,Q,F,FV) :- freeVars(Op,Ex,Q,F,F0), freeVars(A,Ex,Q,F0,FV).
freeVars(capply(_,_,A,_),Ex,Q,F,FV) :- freeVars(A,Ex,Q,F,FV).
freeVars(invoke(_,Op,A,_),Ex,Q,F,FV) :- freeVars(Op,Ex,Q,F,F0), freeVars(A,Ex,Q,F0,FV).
freeVars(dot(_,Rc,_,_),Ex,Q,F,FV) :- freeVars(Rc,Ex,Q,F,FV).
freeVars(open(_,E,_),Ex,Q,F,Fv) :- freeVars(E,Ex,Q,F,Fv).
freeVars(cell(_,Cll),Ex,Q,F,FV) :- freeVars(Cll,Ex,Q,F,FV).
freeVars(deref(_,Cll),Ex,Q,F,FV) :- freeVars(Cll,Ex,Q,F,FV).
freeVars(where(_,T,C),Ex,Q,F,FV) :- ptnGoalVars(C,Ex,E1),
  freeVars(T,E1,Q,F,F0),
  freeVars(C,E1,Q,F0,FV).
freeVars(cond(_,C,T,E,_),Ex,Q,F,FV) :- ptnGoalVars(C,Ex,E1),
  freeVars(T,E1,Q,F,F0),
  freeVars(C,E1,Q,F0,F1),
  freeVars(E,Ex,Q,F1,FV).
freeVars(lambda(_,_,Eqn,_),Ex,Q,F,FV) :- freeVarsInRule(Ex,Q,freevars:freeVars,Eqn,F,FV).
freeVars(conj(Lc,L,R),Ex,Q,F,FV) :- ptnGoalVars(conj(Lc,L,R),Ex,E1),
  freeVars(L,E1,Q,F,F0),freeVars(R,E1,Q,F0,FV).
freeVars(disj(_,L,R),Ex,Q,F,FV) :- freeVars(L,Ex,Q,F,F0),freeVars(R,Ex,Q,F0,FV).
freeVars(neg(_,L),Ex,Q,F,FV) :- freeVars(L,Ex,Q,F,FV).
freeVars(implies(_,G,T),Ex,Q,F,FV) :- ptnGoalVars(G,Ex,E1),
  freeVars(G,E1,Q,F,F0),
  freeVars(T,E1,Q,F0,FV).
freeVars(match(_,L,R),Ex,Q,F,FV) :-
  ptnVars(L,Ex,Ex1),
  freeVars(L,Ex1,Q,F,F0),
  freeVars(R,Ex,Q,F0,FV).
freeVars(letExp(_,_,Defs,Bnd),Ex,Q,F,Fv) :-
  definedVars(Defs,Ex,Ex1),
  freeVarsInDefs(Defs,Ex1,Q,F,F0),
  freeVars(Bnd,Ex1,Q,F0,Fv).
freeVars(letRec(_,_,Defs,Bnd),Ex,Q,F,Fv) :-
  definedVars(Defs,Ex,Ex1),
  freeVarsInDefs(Defs,Ex1,Q,F,F0),
  freeVars(Bnd,Ex1,Q,F0,Fv).
freeVars(case(_,Gov,Cses,_),Ex,Q,F,Fv) :-
  freeVars(Gov,Ex,Q,F,F0),
  freeVarsInRules(Cses,Ex,Q,freevars:freeVars,F0,Fv).
freeVars(valof(_,A,_),Ex,Q,F,Fv) :-!,
  freeVarsInAction(A,Ex,Q,F,Fv).
freeVars(raise(_,A,T,_),Ex,Q,F,Fv) :-!,
  freeVars(T,Ex,Q,F,F0),
  freeVars(A,Ex,Q,F0,Fv).
freeVars(tryCatch(_,B,T,H),Ex,Q,F,Fv) :-!,
  ptnVars(T,Ex,Ex1),  
  freeVars(B,Ex1,Q,F,F0),
  freeVars(T,Ex1,Q,F0,F1),
  freeVarsInRules(H,Ex,Q,freevars:freeVars,F1,Fv).
freeVars(fiber(_,A,_),Ex,Q,F,Fv) :-
  freeVars(A,Ex,Q,F,Fv).
freeVars(T,_,_,F,F) :-
  locOfCanon(T,Lc),
  reportError("cannot find free vars in %s",[can(T)],Lc).

freeVarsInAction(Act,Ex,Q,F,Fv) :-
  freeVarsInAction(Act,Ex,_,Q,F,Fv),!.

freeVarsInAction(doNop(_),Ex,Ex,_,F,F) :-!.
freeVarsInAction(doSeq(_,L,R),E,Ex,Q,F,Fv) :-!,
  freeVarsInAction(L,E,E0,Q,F,F0),
  freeVarsInAction(R,E0,Ex,Q,F0,Fv).
freeVarsInAction(doLbld(_,_,R),E,Ex,Q,F,Fv) :-!,
  freeVarsInAction(R,E,Ex,Q,F,Fv).
freeVarsInAction(doBrk(_,_),Ex,Ex,_,F,F) :-!.
freeVarsInAction(doValis(_,E),Ex,Ex,Q,F,Fv) :-!,
  freeVars(E,Ex,Q,F,Fv).
freeVarsInAction(doRaise(_,T,E),Ex,Ex,Q,F,Fv) :-!,
  freeVars(T,Ex,Q,F,F0),
  freeVars(E,Ex,Q,F0,Fv).
freeVarsInAction(doDefn(_,V,E),Ex,Ex1,Q,F,Fv) :-!,
  ptnVars(V,Ex,Ex1),
  freeVars(E,Ex,Q,F,Fv).
freeVarsInAction(doMatch(_,P,E),Ex,Ex1,Q,F,Fv) :-!,
  ptnVars(P,Ex,Ex1),
  freeVars(P,Ex1,Q,F,F0),
  freeVars(E,Ex,Q,F0,Fv).
freeVarsInAction(doAssign(_,P,E),Ex,Ex,Q,F,Fv) :-!,
  freeVars(P,Ex,Q,F,F0),
  freeVars(E,Ex,Q,F0,Fv).
freeVarsInAction(doTryCatch(_,B,T,H),Ex,Exx,Q,F,Fv) :-!,
  ptnVars(T,Ex,Ex1),  
  freeVars(T,Ex1,Q,F,F0),
  freeVarsInAction(B,Ex1,Exx,Q,F0,F1),
  freeVarsInRules(H,Ex,Q,freevars:freeVarsInAct,F1,Fv).
freeVarsInAction(doIfThenElse(_,T,L,R),Ex,Exx,Q,F,Fv) :-!,
  ptnGoalVars(T,Ex,Ex1),
  freeVars(T,Ex1,Q,F,F0),
  freeVarsInAction(L,Ex1,Ex2,Q,F0,F1),
  freeVarsInAction(R,Ex,Ex3,Q,F1,Fv),
  mergeVars(Ex2,Ex3,Exx).
freeVarsInAction(doWhile(_,T,L),Ex,Ex,Q,F,Fv) :-!,
  ptnGoalVars(T,Ex,Ex1),
  freeVars(T,Ex1,Q,F,F0),
  freeVarsInAction(L,Ex1,_,Q,F0,Fv).
freeVarsInAction(doLet(_,_,Defs,Bnd),Ex,Ex,Q,F,Fv) :-!,
  definedVars(Defs,Ex,Ex1),
  freeVarsInDefs(Defs,Ex1,Q,F,F0),
  freeVarsInAction(Bnd,Ex1,_,Q,F0,Fv).
freeVarsInAction(doLetRec(_,_,Defs,Bnd),Ex,Ex,Q,F,Fv) :-!,
  definedVars(Defs,Ex,Ex1),
  freeVarsInDefs(Defs,Ex1,Q,F,F0),
  freeVarsInAction(Bnd,Ex1,_,Q,F0,Fv).
freeVarsInAction(doCase(_,G,Cs,_),Ex,Ex,Q,F,Fv) :-!,
  freeVars(G,Ex,Q,F,F0),
  freeVarsInRules(Cs,Ex,Q,freevars:freeVarsInAct,F0,Fv).
freeVarsInAction(doRetire(_,T,E),Ex,Ex,Q,F,Fv) :-!,
  freeVars(T,Ex,Q,F,F0),
  freeVars(E,Ex,Q,F0,Fv).
freeVarsInAction(doCall(_,C),Ex,Ex,Q,F,Fv) :-!,
  freeVars(C,Ex,Q,F,Fv).
freeVarsInAction(A,Ex,Ex,_,F,F) :-
  locOfCanon(A,Lc),
  reportMsg("cannot find free vars in %s",[can(A)],Lc).

freeVarsInAct(A,Ex,Q,F,Fv) :-
  freeVarsInAction(A,Ex,_,Q,F,Fv).

definedVars(Defs,Q,Qx) :-
  varsInList(Defs,freevars:defVar,Q,Qx).

defVar(funDef(_,Nm,_,_,_,_,_),Q,[idnt(Nm)|Q]).
defVar(varDef(_,Nm,_,_,_,_),Q,[idnt(Nm)|Q]).
defVar(_,Q,Q).

excluded(V,Ex) :- is_member(idnt(V),Ex).

qualifed(V,Q) :- is_member(idnt(V),Q).

freeVarsInDefs(L,Ex,Q,F,Fv) :-
  rfold(L,freevars:freeVarsInDef(Ex,Q),F,Fv).

freeVarsInDef(Ex,Q,funDef(_,_,_,_,_,_,Eqns),F,Fv) :-
  freeVarsInRules(Eqns,Ex,Q,freevars:freeVars,F,Fv).
freeVarsInDef(Ex,Q,varDef(_,_,_,_,_Tp,Value),F,Fv) :-
  freeVars(Value,Ex,Q,F,Fv).
freeVarsInDef(_,_,_,F,F).

freeVarsInRules(Eqns,Ex,Q,C,F,Fv) :-
  varsInList(Eqns,freevars:freeVarsInRule(Ex,Q,C),F,Fv).

freeVarsInRule(Ex,Q,C,rule(_,H,none,Exp),F,FV) :-!,
  ptnVars(H,Ex,Ex1),
  freeVars(H,Ex1,Q,F,F0),
  call(C,Exp,Ex1,Q,F0,FV).
freeVarsInRule(Ex,Q,C,rule(_,H,some(Cond),Exp),F,FV) :-
  ptnVars(H,Ex,Ex1),
  ptnGoalVars(Cond,Ex1,Ex2),
  freeVars(H,Ex2,Q,F,F0),
  call(C,Exp,Ex2,Q,F0,F1),
  freeVars(Cond,Ex2,Q,F1,FV).

freeVarsList(L,Ex,Q,F,Fv) :- varsInList(L,freevars:frVars(Ex,Q),F,Fv).

frVars(Ex,Q,Trm,F,Fv) :- freeVars(Trm,Ex,Q,F,Fv).

ptnVars(v(_,Nm,_),Q,Qx) :- add_mem(idnt(Nm),Q,Qx).
ptnVars(anon(_,_),Q,Q).
ptnVars(intLit(_,_),Q,Q).
ptnVars(bigLit(_,_),Q,Q).
ptnVars(floatLit(_,_),Q,Q).
ptnVars(charLit(_,_),Q,Q).
ptnVars(stringLit(_,_),Q,Q).
ptnVars(enm(_,_,_),Q,Q).
ptnVars(where(_,Ptn,C),Q,Qx) :- ptnVars(Ptn,Q,Q0), ptnGoalVars(C,Q0,Qx).
ptnVars(tple(_,Els),Q,Qx) :- ptnVarsInList(Els,Q,Qx).
ptnVars(apply(_,_,Arg,_),Q,Qx) :- ptnVars(Arg,Q,Qx).
ptnVars(capply(_,_,Arg,_),Q,Qx) :- ptnVars(Arg,Q,Qx).
ptnVars(dot(_,_,_,_),Q,Q).

ptnVarsInList([],Q,Q).
ptnVarsInList([P|L],Q,Qx) :-
  ptnVars(P,Q,Q0),
  ptnVarsInList(L,Q0,Qx).

ptnGoalVars(conj(_,L,R),Q,Qx) :-
  ptnGoalVars(L,Q,Q0),
  ptnGoalVars(R,Q0,Qx).
ptnGoalVars(disj(_,L,R),Q,Qx) :-
  ptnGoalVars(L,[],LQ),
  ptnGoalVars(R,[],RQ),
  intersect(LQ,RQ,DQ),
  merge(Q,DQ,Qx).
ptnGoalVars(match(_,L,G),Q,Qx) :-
  ptnVars(L,Q,Q0),
  ptnGoalVars(G,Q0,Qx).
ptnGoalVars(cond(_,T,L,R,_),Q,Qx) :-
  ptnGoalVars(T,Q,Q0),
  ptnGoalVars(L,Q0,LQ),
  ptnGoalVars(R,[],RQ),
  intersect(LQ,RQ,DQ),
  merge(Q,DQ,Qx).
ptnGoalVars(implies(_,_,_),Qx,Qx). % implies does not add to available bound vars
ptnGoalVars(given(_,P,_),Q,Qx) :-
  ptnVars(P,Q,Qx).
ptnGoalVars(_,Q,Q).

varsInList([],_,F,F).
varsInList([T|L],C,F,Fv) :-
  call(C,T,F,F0),
  varsInList(L,C,F0,Fv).

mergeVars(F1,F2,Fv) :-
  intersect(F1,F2,Fv).

