:- module(vartypes,[typeOfVar/7,declareVr/5,declareVr/6,declareEnum/6,declareCns/6,declareMtd/5]).

:- use_module(freshen).
:- use_module(unify).
:- use_module(dict).
:- use_module(errors).

typeOfVar(Lc,_,Tp,entry(_,MkTerm,VTp,_),Env,Ev,Term) :-
  freshen(VTp,Env,_,VrTp),
  checkType(Lc,VrTp,Tp,Env),
  call(MkTerm,Lc,Exp),
  manageConstraints(VrTp,[],Lc,Exp,MTp,Env,Ev,Term),
  checkType(Lc,MTp,Tp,Env).

manageConstraints(constrained(Tp,implementsFace(TV,Fc)),Cons,Lc,V,MTp,Env,Ev,Exp) :- !,
  declareConstraint(implementsFace(TV,Fc),Env,E0),
  manageConstraints(Tp,Cons,Lc,V,MTp,E0,Ev,Exp).
manageConstraints(constrained(Tp,Con),Cons,Lc,V,MTp,Env,Ev,Exp) :- !,
  manageConstraints(Tp,[Con|Cons],Lc,V,MTp,Env,Ev,Exp).
manageConstraints(Tp,[],_,V,Tp,Env,Env,V) :- !.
manageConstraints(Tp,RCons,Lc,V,Tp,Env,Env,over(Lc,V,Cons)) :- reverse(RCons,Cons).

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Lc,S,T,_) :-
  reportError("%s not consistent with expected type %s",[S,T],Lc).

declareVr(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,entry(Lc,vartypes:mkVr(Nm),Tp,vartypes:faceTp(Tp)),Env,Ev).
declareVr(Lc,Nm,Tp,Face,Env,Ev) :-
  declareVar(Nm,entry(Lc,vartypes:mkVr(Nm),Tp,vartypes:gtType(Face)),Env,Ev).
declareMtd(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,entry(Lc,vartypes:mkMtd(Nm,Tp),Tp,vartypes:faceTp(Tp)),Env,Ev).
declareEnum(Lc,Nm,EnNm,Tp,Env,Ev) :-
  declareVar(Nm,entry(Lc,vartypes:mkEnum(EnNm),Tp,vartypes:faceTp(Tp)),Env,Ev).
declareCns(Lc,Nm,CnNm,Tp,Env,Ev) :-
  declareVar(Nm,entry(Lc,vartypes:mkCns(CnNm),Tp,vartypes:faceTp(Tp)),Env,Ev).

mkVr(Nm,Lc,v(Lc,Nm)).
mkMtd(Nm,Tp,Lc,mtd(Nm,Lc,Tp)).
mkCns(Nm,Lc,cns(Lc,Nm)).
mkEnum(Nm,Lc,enm(Lc,Nm)).

gtType(Tp,Env,Type) :-
  freshen(Tp,Env,_,Type).
faceTp(Tp,Env,Face) :-
  faceOfType(Tp,Env,Face).
