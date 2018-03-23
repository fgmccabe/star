:-module(freevars,[freeVars/4,definedVars/3]).

:- use_module(misc).
:- use_module(escapes).

freeVars(v(Lc,Lb),Q,F,Fv) :-
  \+ isEscape(Lb),
  \+(is_member(v(_,Lb),Q);is_member(lbl(Lb,_),Q)),!,
  (is_member(v(_,Lb),F) -> F=Fv ; Fv=[v(Lc,Lb)|F]).
freeVars(v(_,_),_,F,F).
freeVars(enm(_,_),_,F,F).
freeVars(cns(_,_),_,F,F).
freeVars(intLit(_),_,F,F).
freeVars(floatLit(_),_,F,F).
freeVars(stringLit(_),_,F,F).
freeVars(tple(_,Els),Q,F,FV) :- freeVarsList(Els,Q,F,FV).
freeVars(apply(_,Op,A),Q,F,FV) :- freeVars(Op,Q,F,F0), freeVars(A,Q,F0,FV).
freeVars(dot(_,Rc,_),Q,F,FV) :- freeVars(Rc,Q,F,FV).
freeVars(where(_,T,C),Q,F,FV) :- freeVars(T,Q,F,F0),freeCondVars(C,Q,F0,FV).
freeVars(cond(_,C,T,E),Q,F,FV) :- freeVars(T,Q,F,F0),freeVars(C,Q,F0,F1),freeVars(E,Q,F1,FV).
freeVars(lambda(_,Rl,_),Q,F,FV) :- freeVarsInRule(Rl,Q,F,FV).
freeVars(conj(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVars(disj(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVars(neg(_,L),Q,F,FV) :- freeVars(L,Q,F,FV).
freeVars(match(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVars(theta(_,_,Defs,Others,_,_),Q,F,Fv) :-
  definedVars(Defs,Q,Q1),
  freeVarsInDefs(Defs,Q1,F,F0),
  freeVarsInOthers(Others,Q1,F0,Fv).
freeVars(record(_,_,Defs,Others,_,_),Q,F,Fv) :-
  definedVars(Defs,Q,Q1),
  freeVarsInDefs(Defs,Q1,F,F0),
  freeVarsInOthers(Others,Q1,F0,Fv).

definedVars(Defs,Q,Qx) :-
  varsInList(Defs,freevars:defVar,[],Q,Qx).

defVar(funDef(Lc,Nm,_,_,_),_,Q,[v(Lc,Nm)|Q]).
defVar(varDef(Lc,Nm,_,_,_),_,Q,[v(Lc,Nm)|Q]).
defVar(_,_,Q,Q).

freeVarsInDefs(L,Q,F,Fv) :-
  varsInList(L,freevars:freeVarsInDef,Q,F,Fv).

freeVarsInOthers(L,Q,F,Fv) :-
  varsInList(L,freevars:freeVarsInOther,Q,F,Fv).

freeVarsInDef(funDef(_,_,_,_,Eqns),Q,F,Fv) :-
  freeVarsInEqns(Eqns,Q,F,Fv).
freeVarsInDef(varDef(_,_,_,_,Value),Q,F,Fv) :-
  freeVars(Value,Q,F,Fv).
freeVarsInDef(_,_,F,F).

freeVarsInEqns(Eqns,Q,F,Fv) :-
  varsInList(Eqns,freevars:eqnVars,Q,F,Fv).

eqnVars(equation(_,_,A,Cond,Exp),Q,F,FV) :-
  ptnVars(A,Q,Q,Q0),
  freeVars(A,Q0,F,F0),
  freeVars(Exp,Q0,F0,F1),
  freeVars(Cond,Q0,F1,FV).

freeVarsList(L,Q,F,Fv) :- varsInList(L,freevars:freeVars,Q,F,Fv).

freeVarsInOther(assertion(_,C),Q,F,Fv) :-
  freeVars(C,Q,F,Fv).
freeVarsInOther(ignore(_,C),Q,F,Fv) :-
  freeVars(C,Q,F,Fv).

ptnVars(v(Lc,Lb),F,Q,Qx) :- \+is_member(v(_,Lb),F), add_mem(v(Lc,Lb),Q,Qx).
ptnVars(v(_,_),_,Q,Q).
ptnVars(intLit(_),_,Q,Q).
ptnVars(floatLit(_),_,Q,Q).
ptnVars(stringLit(_),_,Q,Q).
ptnVars(enm(_,_),_,Q,Q).
ptnVars(cns(_,_),_,Q,Q).
ptnVars(where(_,Ptn,C),F,Q,Qx) :- ptnVars(Ptn,F,Q,Q0), ptnGoalVars(C,F,Q0,Qx).
ptnVars(tple(_,Els),F,Q,Qx) :- ptnVarsInList(Els,F,Q,Qx).
ptnVars(apply(_,_,A),F,Q,Qx) :- ptnVars(A,F,Q,Qx).
ptnVars(theta(_,_,Els,_,_,_),F,Q,Qx) :- ptnVarsInDefs(Els,F,Q,Qx).
ptnVars(record(_,_,Els,_,_,_),F,Q,Qx) :- ptnVarsInDefs(Els,F,Q,Qx).

ptnVarsInList([],_,Q,Q).
ptnVarsInList([P|L],F,Q,Qx) :-
  ptnVars(P,F,Q,Q0),
  ptnVarsInList(L,F,Q0,Qx).

ptnVarsInDefs([],_,Q,Q).
ptnVarsInDefs([P|L],F,Q,Qx) :-
  ptnVarsInDef(P,F,Q,Q0),
  ptnVarsInDefs(L,F,Q0,Qx).

ptnVarsInDef(varDef(_,_,_,_,Value),F,Q,Qx) :-
  ptnVars(Value,F,Q,Qx).
ptnVarsInDef(_,_,Q,Q).

ptnGoalVars(conj(_,L,R),F,Q,Qx) :-
  ptnGoalVars(L,F,Q,Q0),
  ptnGoalVars(R,F,Q0,Qx).
ptnGoalVars(disj(_,L,R),F,Q,Qx) :-
  ptnGoalVars(L,F,Q,Q0),
  ptnGoalVars(R,F,Q0,Qx).
ptnGoalVars(match(_,L,G),F,Q,Qx) :-
  ptnVars(L,F,Q,Q0),
  ptnGoalVars(G,F,Q0,Qx).
ptnGoalVars(cond(_,T,L,R),F,Q,Qx) :-
  ptnGoalVars(T,F,Q,Q0),
  ptnGoalVars(L,F,Q0,Q1),
  ptnGoalVars(R,F,Q1,Qx).
ptnGoalVars(_,_,Q,Q).

varsInList([],_,_,F,F).
varsInList([T|L],C,Q,F,Fv) :-
  call(C,T,Q,F,F0),
  varsInList(L,C,Q,F0,Fv).
