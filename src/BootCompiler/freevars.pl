:-module(freevars,[freeVarsInRule/4]).

:- use_module(misc).

freeVarsInRule(equation(_,_,A,Cond,Exp),Q,F,FV) :-
  freeVarsList(A,Q,F,F0),
  freeVars(Exp,Q,F0,F1),
  freeVarsInGoal(Cond,Q,F1,FV).
freeVarsInRule(clause(_,_,A,Cond,Body),Q,F,FV) :-
  freeVarsList(A,Q,F,F0),
  freeVarsInGoal(Cond,Q,F0,F1),
  freeVarsInGoal(Body,Q,F1,FV).
freeVarsInRule(grammarRule(_,_,A,PB,Body),Q,F,FV) :-
  freeVarsList(A,Q,F,F0),
  freeVarsInNT(Body,Q,F0,F1),
  freeVarsInTerms(PB,Q,F1,FV).

freeVarsList([],_,FV,FV).
freeVarsList([T|L],Q,F,FV) :-
  freeVars(T,Q,F,F0),
  freeVarsList(L,Q,F0,FV).

freeVars(v(_,Lb),Q,F,[idnt(Lb)|F]) :- is_member(idnt(Lb),Q),!.
freeVars(v(_,_),_,F,F).
freeVars(intLit(_),_,F,F).
freeVars(floatLit(_),_,F,F).
freeVars(stringLit(_),_,F,F).
freeVars(tuple(_,Els),Q,F,FV) :- freeVarsList(Els,Q,F,FV).
freeVars(apply(Op,A),Q,F,FV) :- freeVars(Op,Q,F,F0), freeVarsList(A,Q,F0,FV).
freeVars(dot(Rc,_),Q,F,FV) :- freeVars(Rc,Q,F,FV).
freeVars(where(T,C),Q,F,FV) :- freeVars(T,Q,F,F0),freeVarsInGoal(C,Q,F0,FV).
freeVars(conditional(_,C,T,E),Q,F,FV) :- freeVars(T,Q,F,F0),freeVarsInGoal(C,Q,F0,F1),freeVars(E,Q,F1,FV).
freeVars(lambda(Rl),Q,F,FV) :- freeVarsInRule(Rl,Q,F,FV).

freeVarsInGoal(true(_),_,F,F).
freeVarsInGoal(false(_),_,F,F).
freeVarsInGoal(conj(L,R),Q,F,FV) :- freeVarsInGoal(L,Q,F,F0),freeVarsInGoal(R,Q,F0,FV).
freeVarsInGoal(disj(L,R),Q,F,FV) :- freeVarsInGoal(L,Q,F,F0),freeVarsInGoal(R,Q,F0,FV).
freeVarsInGoal(conditional(T,L,R),Q,F,FV) :- freeVarsInGoal(L,Q,F,F0),freeVarsInGoal(R,Q,F0,F1),freeVarsInGoal(T,Q,F1,FV).
freeVarsInGoal(one(_,L),Q,F,FV) :- freeVarsInGoal(L,Q,F,FV).
freeVarsInGoal(neg(_,L),Q,F,FV) :- freeVarsInGoal(L,Q,F,FV).
freeVarsInGoal(forall(_,L,R),Q,F,FV) :- freeVarsInGoal(L,Q,F,F0),freeVarsInGoal(R,Q,F0,FV).
freeVarsInGoal(unify(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVarsInGoal(phrase(_,NT,L,R),Q,F,FV) :- freeVarsInNT(NT,Q,F,F0),freeVars(L,Q,F0,F1),freeVars(R,Q,F1,FV).
freeVarsInGoal(call(_,O,A),Q,F,FV) :- freeVars(O,Q,F,F0),freeVarsList(A,Q,F0,FV).
freeVarsInGoal(isTrue(_,T),Q,F,FV) :- freeVars(T,Q,F,FV).

freeVarsInNT(terminals(_,Terms),Q,F,FV) :- freeVarsInTerms(Terms,Q,F,FV).
freeVarsInNT(eof(_,S),Q,F,FV) :- freeVars(S,Q,F,FV).
freeVarsInNT(conj(_,L,R),Q,F,FV) :- freeVarsInNT(L,Q,F,F0),freeVarsInNT(R,Q,F0,FV).
freeVarsInNT(disj(_,L,R),Q,F,FV) :- freeVarsInNT(L,Q,F,F0),freeVarsInNT(R,Q,F0,FV).
freeVarsInNT(conditional(_,T,L,R),Q,F,FV) :- freeVarsInNT(T,Q,F,F0),freeVarsInNT(L,Q,F0,F1),freeVarsInNT(R,Q,F1,FV).
freeVarsInNT(one(_,L),Q,F,FV) :- freeVarsInNT(L,Q,F,FV).
freeVarsInNT(neg(_,L),Q,F,FV) :- freeVarsInNT(L,Q,F,FV).
freeVarsInNT(ahead(_,L),Q,F,FV) :- freeVarsInNT(L,Q,F,FV).
freeVarsInNT(guard(_,L,T),Q,F,FV) :- freeVarsInNT(L,Q,F,F0),freeVarsInGoal(T,Q,F0,FV).
freeVarsInNT(goal(_,T),Q,F,FV) :- freeVarsInGoal(T,Q,F,FV).
freeVarsInNT(call(_,O,A),Q,F,FV) :- freeVars(O,Q,F,F0),freeVarsList(A,Q,F0,FV).

freeVarsInTerms([],_,F,F).
freeVarsInTerms([term(_,_,T)|L],Q,F,FV) :-
  freeVars(T,Q,F,F0),
  freeVarsInTerms(L,Q,F0,FV).
