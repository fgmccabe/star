:-module(freevars,[freeVars/5,goalVars/2]).

:- use_module(misc).
:- use_module(escapes).
:- use_module(types).

%! freeVars(++,++,++,++,--) is det.
freeVars(v(_Lc,Lb,_),Ex,Q,F,Fv) :-
  \+ excluded(Lb,Ex),
  qualifed(Lb,Q),!,
  addFrVar(Lb,F,Fv).
freeVars(v(_,_,_),_,_,F,F).
freeVars(enm(_,_,_),_,_,F,F).
freeVars(cons(_,_,_),_,_,F,F).
freeVars(intLit(_,_),_,_,F,F).
freeVars(floatLit(_,_),_,_,F,F).
freeVars(stringLit(_,_),_,_,F,F).
freeVars(tple(_,Els),Ex,Q,F,FV) :- freeVarsList(Els,Ex,Q,F,FV).
freeVars(apply(_,Op,A,_),Ex,Q,F,FV) :- freeVars(Op,Ex,Q,F,F0), freeVars(A,Ex,Q,F0,FV).
freeVars(dot(_,Rc,_,_),Ex,Q,F,FV) :- freeVars(Rc,Ex,Q,F,FV).
freeVars(varRef(_,Cll),Ex,Q,F,FV) :- freeVars(Cll,Ex,Q,F,FV).
freeVars(cell(_,Cll),Ex,Q,F,FV) :- freeVars(Cll,Ex,Q,F,FV).
freeVars(assign(_,Lhs,Rhs),Ex,Q,F,FV) :- freeVars(Lhs,Ex,Q,F,F0),freeVars(Rhs,Ex,Q,F0,FV).
freeVars(where(_,T,C),Ex,Q,F,FV) :- ptnGoalVars(C,freevars:addFrVar,Ex,E1),freeVars(T,E1,Q,F,F0),freeVars(C,E1,Q,F0,FV).
freeVars(cond(_,C,T,E,_),Ex,Q,F,FV) :- ptnGoalVars(C,freevars:addFrVar,Ex,E1),freeVars(T,E1,Q,F,F0),freeVars(C,E1,Q,F0,F1),freeVars(E,Ex,Q,F1,FV).
freeVars(lambda(_,Eqn,_),Ex,Q,F,FV) :- freeVarsInRule(Ex,Q,Eqn,F,FV).
freeVars(conj(Lc,L,R),Ex,Q,F,FV) :- ptnGoalVars(conj(Lc,L,R),freevars:addFrVar,Ex,E1),freeVars(L,E1,Q,F,F0),freeVars(R,E1,Q,F0,FV).
freeVars(disj(_,L,R),Ex,Q,F,FV) :- freeVars(L,Ex,Q,F,F0),freeVars(R,Ex,Q,F0,FV).
freeVars(neg(_,L),Ex,Q,F,FV) :- freeVars(L,Ex,Q,F,FV).
freeVars(implies(_,G,T),Ex,Q,F,FV) :- ptnGoalVars(G,freevars:addFrVar,Ex,E1),freeVars(G,E1,Q,F,F0),freeVars(T,E1,Q,F0,FV).
freeVars(match(_,L,R),Ex,Q,F,FV) :-
  ptnVars(L,freevars:addFrVar,Ex,Ex1),
  freeVars(L,Ex1,Q,F,F0),freeVars(R,Ex,Q,F0,FV).
freeVars(search(_,L,R,I),Ex,Q,F,FV) :-
  ptnVars(L,freevars:addVar,Ex,Ex1),
  freeVars(L,Ex1,Q,F,F0),
  freeVars(R,Ex1,Q,F0,F1),
  freeVars(I,Ex1,Q,F1,FV).
freeVars(abstraction(_,B,C,Z,G,_),Ex,Q,F,FV) :-
  ptnGoalVars(C,freevars:addFrVar,Ex,Ex1),
  freeVars(B,Ex1,Q,F,F0),
  freeVars(C,Ex1,Q,F0,F1),
  freevars(Z,Ex1,Q,F1,F2),
  freeVars(G,Ex1,Q,F2,FV).
freeVars(theta(_,_,_,Defs,Others,_,_),Ex,Q,F,Fv) :-
  definedVars(Defs,Ex,Ex1),
  freeVarsInDefs(Defs,Ex1,Q,F,F0),
  freeVarsInOthers(Others,Ex1,Q,F0,Fv).
freeVars(record(_,_,_,Defs,Others,_,_),Ex,Q,F,Fv) :-
  freeVarsInDefs(Defs,Ex,Q,F,F0),
  freeVarsInOthers(Others,Ex,Q,F0,Fv).
freeVars(letExp(_,Rc,Bnd),Ex,Q,F,Fv) :-
  freeVars(Rc,Ex,Q,F,F0),
  freeVars(Bnd,Ex,Q,F0,Fv).

definedVars(Defs,Q,Qx) :-
  varsInList(Defs,freevars:defVar,Q,Qx).

defVar(funDef(_,Nm,_,_,_,_),Q,[idnt(Nm)|Q]).
defVar(varDef(_,Nm,_,_,_,_),Q,[idnt(Nm)|Q]).
defVar(_,Q,Q).

excluded(V,Ex) :- is_member(idnt(V),Ex).

qualifed(V,Q) :- is_member(idnt(V),Q).

freeVarsInDefs(L,Ex,Q,F,Fv) :-
  varsInList(L,freevars:freeVarsInDef(Ex,Q),F,Fv).

freeVarsInOthers(L,Ex,Q,F,Fv) :-
  varsInList(L,freevars:freeVarsInOther(Ex,Q),F,Fv).

freeVarsInDef(Ex,Q,funDef(_,_,_,_,_,Eqns),F,Fv) :-
  freeVarsInRules(Eqns,Ex,Q,F,Fv).
freeVarsInDef(Ex,Q,varDef(_,_,_,_,_Tp,Value),F,Fv) :-
  freeVars(Value,Ex,Q,F,Fv).
freeVarsInDef(_,_,_,F,F).

freeVarsInRules(Eqns,Ex,Q,F,Fv) :-
  varsInList(Eqns,freevars:freeVarsInRule(Ex,Q),F,Fv).

freeVarsInRule(Ex,Q,equation(_,H,Cond,Exp),F,FV) :-
  ptnVars(H,freevars:addVar,Ex,Ex1),
  freeVars(H,Ex1,Q,F,F0),
  freeVars(Exp,Ex1,Q,F0,F1),
  freeVars(Cond,Ex1,Q,F1,FV).

freeVarsList(L,Ex,Q,F,Fv) :- varsInList(L,freevars:frVars(Ex,Q),F,Fv).

frVars(Ex,Q,Trm,F,Fv) :- freeVars(Trm,Ex,Q,F,Fv).

freeVarsInOther(Ex,Q,assertion(_,C),F,Fv) :-
  freeVars(C,Ex,Q,F,Fv).
freeVarsInOther(Ex,Q,show(_,C),F,Fv) :-
  freeVars(C,Ex,Q,F,Fv).

ptnVars(v(Lc,Nm,Tp),A,Q,Qx) :- call(A,v(Lc,Nm,Tp),Q,Qx).
ptnVars(intLit(_,_),_,Q,Q).
ptnVars(floatLit(_,_),_,Q,Q).
ptnVars(stringLit(_,_),_,Q,Q).
ptnVars(enm(_,_,_),_,Q,Q).
ptnVars(cons(_,_,_),_,Q,Q).
ptnVars(where(_,Ptn,C),A,Q,Qx) :- ptnVars(Ptn,A,Q,Q0), ptnGoalVars(C,A,Q0,Qx).
ptnVars(tple(_,Els),A,Q,Qx) :- ptnVarsInList(Els,A,Q,Qx).
ptnVars(apply(_,_,Arg,_),A,Q,Qx) :- ptnVars(Arg,A,Q,Qx).
ptnVars(theta(_,_,Els,_,_,_),A,Q,Qx) :- ptnVarsInDefs(Els,A,Q,Qx).
ptnVars(record(_,_,Els,_,_,_),A,Q,Qx) :- ptnVarsInDefs(Els,A,Q,Qx).

ptnVarsInList([],_,Q,Q).
ptnVarsInList([P|L],A,Q,Qx) :-
  ptnVars(P,A,Q,Q0),
  ptnVarsInList(L,A,Q0,Qx).

ptnVarsInDefs([],_,Q,Q).
ptnVarsInDefs([P|L],A,Q,Qx) :-
  ptnVarsInDef(P,A,Q,Q0),
  ptnVarsInDefs(L,A,Q0,Qx).

ptnVarsInDef(varDef(_,_,_,_,_,Value),A,Q,Qx) :-
  ptnVars(Value,A,Q,Qx).
ptnVarsInDef(_,_,Q,Q).

goalVars(G,Q) :-
  ptnGoalVars(G,freevars:addGlVar,[],Q).

addGlVar(V,Q,Qx) :- add_mem(V,Q,Qx).

addVar(v(_,Nm,_),Q,Qx) :- add_mem(idnt(Nm),Q,Qx).

addFrVar(Nm,Q,Qx) :- add_mem(idnt(Nm),Q,Qx).

ptnGoalVars(conj(_,L,R),A,Q,Qx) :-
  ptnGoalVars(L,A,Q,Q0),
  ptnGoalVars(R,A,Q0,Qx).
ptnGoalVars(disj(_,L,R),A,Q,Qx) :-
  ptnGoalVars(L,A,[],LQ),
  ptnGoalVars(R,A,[],RQ),
  intersect(LQ,RQ,DQ),
  merge(Q,DQ,Qx).
ptnGoalVars(match(_,L,G),A,Q,Qx) :-
  ptnVars(L,A,Q,Q0),
  ptnGoalVars(G,A,Q0,Qx).
ptnGoalVars(cond(_,T,L,R,_),A,Q,Qx) :-
  ptnGoalVars(T,A,Q,Q0),
  ptnGoalVars(L,A,Q0,Q1),
  ptnGoalVars(R,A,Q1,Qx).
ptnGoalVars(implies(_,G,T),A,Q,Qx) :-
  ptnGoalVars(G,A,Q,Q0),
  ptnGoalVars(T,A,Q0,Qx).
ptnGoalVars(given(_,P,_),A,Q,Qx) :-
  ptnVars(P,A,Q,Qx).
ptnGoalVars(search(_,K,_,_),A,Q,Qx) :-
  ptnVars(K,A,Q,Qx).
ptnGoalVars(ixsearch(_,K,V,_,_),A,Q,Qx) :-
  ptnVars(K,A,Q,Q0),
  ptnVars(V,A,Q0,Qx).
ptnGoalVars(_,_,Q,Q).

varsInList([],_,F,F).
varsInList([T|L],C,F,Fv) :-
  call(C,T,F,F0),
  varsInList(L,C,F0,Fv).
