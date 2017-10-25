:- module(vartypes,[typeOfVar/7]).

:- use_module(freshen).
:- use_module(unify).
:- use_module(dict).
:- use_module(errors).

typeOfVar(Lc,Nm,Tp,vr(_,_,VT),Env,Ev,Exp) :-
  freshen(VT,Env,_,VrTp),
  manageConstraints(VrTp,[],Lc,v(Lc,Nm),MTp,Exp,Env,Ev),
  checkType(Lc,MTp,Tp,Env).
typeOfVar(Lc,Nm,Tp,mtd(_,_,MTp),Env,Ev,Exp) :-
  freshen(MTp,Env,_,VrTp),
  manageConstraints(VrTp,[],Lc,mtd(Lc,Nm),MtTp,Exp,Env,Ev),
  checkType(Lc,MtTp,Tp,Env).
typeOfVar(Lc,_,Tp,enum(Nm,_,VT),Env,Ev,Exp) :-
  freshen(VT,Env,_,VrTp),
  manageConstraints(VrTp,[],Lc,enm(Lc,Nm),MTp,Exp,Env,Ev),
  checkType(Lc,MTp,Tp,Env).
typeOfVar(Lc,_,Tp,cons(Nm,_,VT),Env,Ev,Exp) :-
  freshen(VT,Env,_,VrTp),
  manageConstraints(VrTp,[],Lc,cns(Lc,Nm),MTp,Exp,Env,Ev),
  checkType(Lc,MTp,Tp,Env).


manageConstraints(constrained(Tp,implementsFace(TV,Fc)),Cons,Lc,V,MTp,Exp,Env,Ev) :- !,
  declareConstraint(implementsFace(TV,Fc),Env,E0),
  manageConstraints(Tp,Cons,Lc,V,MTp,Exp,E0,Ev).
manageConstraints(constrained(Tp,Con),Cons,Lc,V,MTp,Exp,Env,Ev) :- !,
  manageConstraints(Tp,[Con|Cons],Lc,V,MTp,Exp,Env,Ev).
manageConstraints(Tp,[],_,V,Tp,V,Env,Env) :- !.
manageConstraints(Tp,RCons,Lc,V,Tp,over(Lc,V,Cons),Env,Env) :- reverse(RCons,Cons).

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Lc,S,T,_) :-
  reportError("%s not consistent with expected type %s",[S,T],Lc).
