:- module(vartypes,[typeOfVar/6,
		    declareMtd/5,
		    verifyType/4]).

:- use_module(abstract).
:- use_module(freshen).
:- use_module(unify).
:- use_module(dict).
:- use_module(errors).
:- use_module(types).

typeOfVar(Lc,Tp,vrEntry(_,MkTerm,VTp),Env,Ev,Term) :-
  freshen(VTp,Env,_,VrTp),
  call(MkTerm,Lc,Tp,Exp),
  manageConstraints(VrTp,[],Lc,Exp,MTp,Env,Ev,Term),
  verifyType(Lc,MTp,Tp,Env).

manageConstraints(constrained(Tp,implementsFace(TV,Fc)),Cons,Lc,
		  V,MTp,Env,Ev,overaccess(Exp,TV,Fc)) :- !,
  manageConstraints(Tp,Cons,Lc,V,MTp,Env,Ev,Exp).
manageConstraints(constrained(Tp,Con),Cons,Lc,V,MTp,Env,Ev,Exp) :- !,
  manageConstraints(Tp,[Con|Cons],Lc,V,MTp,Env,Ev,Exp).
manageConstraints(Tp,[],_,V,Tp,Env,Env,V) :- !.
manageConstraints(T,RCons,Lc,V,Tp,Env,Env,over(Lc,V,Tp,Cons)) :-
  reverse(RCons,Cons),
  simplifyType(T,Env,_,[],Tp). % no constraints possible here

declareMtd(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkMtd(Nm),Tp),Env,Ev).

mkMtd(Nm,Lc,Tp,mtd(Lc,Nm,Tp)).

gtType(Tp,Env,Type) :-
  freshen(Tp,Env,_,Type).
faceTp(Tp,Env,Face) :-
  faceOfType(Tp,Env,Face).

verifyType(Lc,Actual,Expected,Env) :-
  sameType(Actual,Expected,Lc,Env),!.
verifyType(Lc,S,T,_) :-
  reportError("type %s not consistent with expected type %s",[tpe(S),tpe(T)],Lc).

