:- module(vartypes,[typeOfVar/7,declareVr/5,declareVr/6,declareEnum/5,
		    declareCns/5,declareMtd/5]).

:- use_module(abstract).
:- use_module(freshen).
:- use_module(unify).
:- use_module(dict).
:- use_module(errors).
:- use_module(types).

typeOfVar(Lc,Vr,Tp,vrEntry(_,MkTerm,VTp,_),Env,Ev,Term) :-
  freshen(VTp,Env,_,VrTp),
  call(MkTerm,Lc,Tp,Exp),
  manageConstraints(VrTp,[],Lc,Exp,MTp,Env,Ev,Term),
  checkType(Vr,MTp,Tp,Env).

manageConstraints(constrained(Tp,implementsFace(TV,Fc)),Cons,Lc,V,MTp,Env,Ev,Exp) :- !,
  declareConstraint(implementsFace(TV,Fc),Env,E0),
  manageConstraints(Tp,Cons,Lc,V,MTp,E0,Ev,Exp).
manageConstraints(constrained(Tp,Con),Cons,Lc,V,MTp,Env,Ev,Exp) :- !,
  manageConstraints(Tp,[Con|Cons],Lc,V,MTp,Env,Ev,Exp).
manageConstraints(Tp,[],_,V,Tp,Env,Env,V) :- !.
manageConstraints(T,RCons,Lc,V,Tp,Env,Env,over(Lc,V,Tp,Cons)) :-
  reverse(RCons,Cons),
  simplifyType(T,Env,_,[],Tp). % no constraints possible here

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Ast,S,T,_) :-
  locOfAst(Ast,Lc),
  reportError("%s:%s not consistent with expected type %s",[Ast,S,T],Lc).

declareVr(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,dict:mkVr(Nm),Tp,vartypes:faceTp(Tp)),Env,Ev).
declareVr(Lc,Nm,Tp,Face,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,dict:mkVr(Nm),Tp,vartypes:gtType(Face)),Env,Ev).
declareMtd(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkMtd(Nm),Tp,vartypes:faceTp(Tp)),Env,Ev).
declareEnum(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkEnum(Nm),Tp,vartypes:faceTp(Tp)),Env,Ev).
declareCns(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkCns(Nm),Tp,vartypes:faceTp(Tp)),Env,Ev).

mkMtd(Nm,Lc,Tp,mtd(Lc,Nm,Tp)).
mkCns(Nm,Lc,Tp,cons(Lc,Nm,Tp)).
mkEnum(Nm,Lc,Tp,enm(Lc,Nm,Tp)).

gtType(Tp,Env,Type) :-
  freshen(Tp,Env,_,Type).
faceTp(Tp,Env,Face) :-
  faceOfType(Tp,Env,Face).
