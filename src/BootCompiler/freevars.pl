:-module(freevars,[freeVarsInRule/4]).

:- use_module(misc).

freeVarsInRule(equation(_,_,A,Cond,Exp),Q,F,FV) :-
  freeVarsList(A,Q,F,F0),
  freeVars(Exp,Q,F0,F1),
  freeVars(Cond,Q,F1,FV).
freeVarsInRule(grammarRule(_,_,A,Cond,Body),Q,F,FV) :-
  freeVarsList(A,Q,F,F0),
  freeVarsInNT(Body,Q,F0,F1),
  freeVars(Cond,Q,F1,FV).

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
freeVars(where(T,C),Q,F,FV) :- freeVars(T,Q,F,F0),freeVars(C,Q,F0,FV).
freeVars(cond(_,C,T,E),Q,F,FV) :- freeVars(T,Q,F,F0),freeVars(C,Q,F0,F1),freeVars(E,Q,F1,FV).
freeVars(lambda(Rl),Q,F,FV) :- freeVarsInRule(Rl,Q,F,FV).
freeVars(conj(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVars(disj(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVars(one(_,L),Q,F,FV) :- freeVars(L,Q,F,FV).
freeVars(neg(_,L),Q,F,FV) :- freeVars(L,Q,F,FV).
freeVars(match(_,L,R),Q,F,FV) :- freeVars(L,Q,F,F0),freeVars(R,Q,F0,FV).
freeVars(phrase(_,NT,L,R),Q,F,FV) :- freeVarsInNT(NT,Q,F,F0),freeVars(L,Q,F0,F1),freeVars(R,Q,F1,FV).
freeVars(phrase(_,NT,L),Q,F,FV) :- freeVarsInNT(NT,Q,F,F0),freeVars(L,Q,F0,FV).

freeVarsInNT(terminals(_,Terms),Q,F,FV) :- freeVarsInTerms(Terms,Q,F,FV).
freeVarsInNT(eof(_,S),Q,F,FV) :- freeVars(S,Q,F,FV).
freeVarsInNT(conj(_,L,R),Q,F,FV) :- freeVarsInNT(L,Q,F,F0),freeVarsInNT(R,Q,F0,FV).
freeVarsInNT(disj(_,L,R),Q,F,FV) :- freeVarsInNT(L,Q,F,F0),freeVarsInNT(R,Q,F0,FV).
freeVarsInNT(cond(_,T,L,R),Q,F,FV) :- freeVarsInNT(T,Q,F,F0),freeVarsInNT(L,Q,F0,F1),freeVarsInNT(R,Q,F1,FV).
freeVarsInNT(one(_,L),Q,F,FV) :- freeVarsInNT(L,Q,F,FV).
freeVarsInNT(neg(_,L),Q,F,FV) :- freeVarsInNT(L,Q,F,FV).
freeVarsInNT(ahead(_,L),Q,F,FV) :- freeVarsInNT(L,Q,F,FV).
freeVarsInNT(guard(_,L,T),Q,F,FV) :- freeVarsInNT(L,Q,F,F0),freeVarsInGoal(T,Q,F0,FV).
freeVarsInNT(goal(_,T),Q,F,FV) :- freeVars(T,Q,F,FV).
freeVarsInNT(call(_,O,A),Q,F,FV) :- freeVars(O,Q,F,F0),freeVarsList(A,Q,F0,FV).

freeVarsInTerms([],_,F,F).
freeVarsInTerms([term(_,_,T)|L],Q,F,FV) :-
  freeVars(T,Q,F,F0),
  freeVarsInTerms(L,Q,F0,FV).
