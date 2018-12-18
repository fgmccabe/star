:-module(freevars,[freeVars/5,goalVars/2]).

:- use_module(misc).
:- use_module(escapes).

%! freeVars(++,++,++,++,--) is det.
freeVars(v(Lc,Lb,_),Ex,Q,F,Fv) :-
  \+ excluded(Lb,Ex),
  qualifed(Lb,Q),!,
  addVar(Lc,Lb,F,Fv).
freeVars(v(_,_,_),_,_,F,F).
freeVars(enm(_,_,_),_,_,F,F).
freeVars(cons(_,_,_),_,_,F,F).
freeVars(intLit(_,_),_,_,F,F).
freeVars(floatLit(_,_),_,_,F,F).
freeVars(stringLit(_,_),_,_,F,F).
freeVars(tple(_,Els),Ex,Q,F,FV) :- freeVarsList(Els,Ex,Q,F,FV).
freeVars(apply(_,Op,Arg,_),Ex,Q,F,FV) :- freeVars(Op,Ex,Q,F,F0), freeVars(Arg,Ex,Q,F0,FV).
freeVars(dot(_,Rc,_,_),Ex,Q,F,FV) :- freeVars(Rc,Ex,Q,F,FV).
freeVars(varRef(_,Cll),Ex,Q,F,FV) :- freeVars(Cll,Ex,Q,F,FV).
freeVars(cell(_,Cll),Ex,Q,F,FV) :- freeVars(Cll,Ex,Q,F,FV).
freeVars(assign(_,Lhs,Rhs),Ex,Q,F,FV) :- freeVars(Lhs,Ex,Q,F,F0),freeVars(Rhs,Ex,Q,F0,FV).
freeVars(where(_,T,C),Ex,Q,A,F,FV) :- ptnGoalVars(C,Ex,E1),freeVars(T,E1,Q,F,F0),freeVars(C,E1,Q,F0,FV).
freeVars(cond(_,C,T,E,_),Ex,Q,F,FV) :- ptnGoalVars(C,Ex,E1),freeVars(T,E1,Q,F,F0),freeVars(C,E1,Q,F0,F1),freeVars(E,Ex,Q,F1,FV).
freeVars(lambda(_,Eqns,_),Ex,Q,F,FV) :- freeVarsInRules(Eqns,Ex,Q,F,FV).
freeVars(conj(Lc,L,R),Ex,Q,F,FV) :- ptnGoalVars(conj(Lc,L,R),Ex,E1),freeVars(L,E1,Q,F,F0),freeVars(R,E1,Q,F0,FV).
freeVars(disj(_,L,R),Ex,Q,F,FV) :- freeVars(L,Ex,Q,F,F0),freeVars(R,Ex,Q,F0,FV).
freeVars(neg(_,L),Ex,Q,F,FV) :- freeVars(L,Ex,Q,F,FV).
freeVars(match(_,L,R),Ex,Q,F,FV) :- ptnVars(L,Ex,Ex1), freeVars(L,Ex1,Q,F,F0),freeVars(R,Ex,Q,F0,FV).
freeVars(search(_,L,R,I),Ex,Q,F,FV) :-
  ptnVars(L,Ex,Ex1),
  freeVars(L,Ex1,Q,F,F0),
  freeVars(R,Ex1,Q,F0,F1),
  freeVars(I,Ex1,Q,F1,FV).
freeVars(abstraction(_,B,C,G,_),Ex,Q,F,FV) :-
  ptnGoalVars(C,Ex,Ex1),
  freeVars(B,Ex1,Q,F,F0),
  freeVars(C,Ex1,Q,F0,F1),
  freeVars(G,Ex1,Q,F1,FV).
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

excluded(V,Ex) :- is_member(V,Ex).

addVar(_,Nm,V,Vx) :-
  add_mem(Nm,V,Vx).

definedVars(Defs,Q,Qx) :-
  varsInList(Defs,freevars:defVar,Q,Qx).

defVar(funDef(_,Nm,_,_,_,_),Q,[Nm|Q]).
defVar(varDef(_,Nm,_,_,_,_),Q,[Nm|Q]).
defVar(_,Q,Q).

freeVarsInDefs(L,Ex,Q,F,Fv) :-
  varsInList(L,freevars:freeVarsInDef(Ex,Q),F,Fv).

freeVarsInOthers(L,Ex,Q,F,Fv) :-
  varsInList(L,freevars:freeVarsInOther(Ex,Q),F,Fv).

freeVarsInDef(Ex,Q,funDef(_,_,_,_,_,Eqns),F,Fv) :-
  freeVarsInRules(Eqns,Ex,Q,F,Fv).
freeVarsInDef(Ex,Q,varDef(_,_,_,_,_,Value),F,Fv) :-
  freeVars(Value,Ex,Q,F,Fv).
freeVarsInDef(_,_,_,F,F).

freeVarsInRules(Eqns,Ex,Q,F,Fv) :-
  varsInList(Eqns,freevars:freeVarsInRule(Ex,Q),F,Fv).

freeVarsInRule(Ex,Q,equation(_,A,Cond,Exp),F,FV) :-
  ptnVars(A,Ex,Ex1),
  freeVars(A,Ex1,Q,F,F0),
  freeVars(Exp,Ex1,Q,F0,F1),
  freeVars(Cond,Ex1,Q,F1,FV).

freeVarsList(L,Ex,Q,F,Fv) :- varsInList(L,freevars:frVars(Ex,Q),F,Fv).

frVars(Ex,Q,Trm,F,Fv) :- freeVars(Trm,Ex,Q,F,Fv).

freeVarsInOther(Ex,Q,assertion(_,C),F,Fv) :-
  freeVars(C,Ex,Q,F,Fv).
freeVarsInOther(Ex,Q,show(_,C),F,Fv) :-
  freeVars(C,Ex,Q,F,Fv).

ptnVars(v(L,Nm,c_),Q,Qx) :- addVar(Lc,Nm,Q,Qx).
ptnVars(intLit(_,_),Q,Q).
ptnVars(floatLit(_,_),Q,Q).
ptnVars(stringLit(_,_),Q,Q).
ptnVars(enm(_,_,_),Q,Q).
ptnVars(cons(_,_,_),Q,Q).
ptnVars(where(_,Ptn,C),Q,Qx) :- ptnVars(Ptn,Q,Q0), ptnGoalVars(C,Q0,Qx).
ptnVars(tple(_,Els),Q,Qx) :- ptnVarsInList(Els,Q,Qx).
ptnVars(apply(_,_,A,_),Q,Qx) :- ptnVars(A,Q,Qx).
ptnVars(theta(_,_,_,Els,_,_,_),Q,Qx) :- ptnVarsInDefs(Els,Q,Qx).
ptnVars(record(_,_,_,Els,_,_,_),Q,Qx) :- ptnVarsInDefs(Els,Q,Qx).

ptnVarsInList([],Q,Q).
ptnVarsInList([P|L],Q,Qx) :-
  ptnVars(P,Q,Q0),
  ptnVarsInList(L,Q0,Qx).

ptnVarsInDefs([],Q,Q).
ptnVarsInDefs([P|L],Q,Qx) :-
  ptnVarsInDef(P,Q,Q0),
  ptnVarsInDefs(L,Q0,Qx).

ptnVarsInDef(varDef(_,_,_,_,_,Value),Q,Qx) :-
  ptnVars(Value,Q,Qx).
ptnVarsInDef(_,Q,Q).

goalVars(G,Q) :-
  ptnGoalVars(G,[],Q).

ptnGoalVars(conj(_,L,R),Q,Qx) :-
  ptnGoalVars(L,Q,Q0),
  ptnGoalVars(R,Q0,Qx).
ptnGoalVars(disj(_,L,R),Q,Qx) :-
  ptnGoalVars(L,Q,Q0),
  ptnGoalVars(R,Q0,Qx).
ptnGoalVars(match(_,L,G),Q,Qx) :-
  ptnVars(L,Q,Q0),
  ptnGoalVars(G,Q0,Qx).
ptnGoalVars(cond(_,T,L,R,_),Q,Qx) :-
  ptnGoalVars(T,Q,Q0),
  ptnGoalVars(L,Q0,Q1),
  ptnGoalVars(R,Q1,Qx).
ptnGoalVars(given(_,P,_),Q,Qx) :-
  ptnVars(P,Q,Qx).
ptnGoalVars(_,Q,Q).

varsInList([],_,F,F).
varsInList([T|L],C,F,Fv) :-
  call(C,T,F,F0),
  varsInList(L,C,F0,Fv).
