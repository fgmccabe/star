:- module(vartypes,[typeOfVar/7,
		    declareEnum/6,
		    declareCns/6,declareMtd/5]).

:- use_module(abstract).
:- use_module(freshen).
:- use_module(unify).
:- use_module(dict).
:- use_module(errors).
:- use_module(types).

typeOfVar(Lc,Vr,Tp,vrEntry(_,MkTerm,VTp),Env,Ev,Term) :-
  freshen(VTp,Env,_,VrTp),
  call(MkTerm,Lc,Tp,Exp),
  manageConstraints(VrTp,[],Lc,Exp,MTp,Env,Ev,Term),
  checkType(name(Lc,Vr),MTp,Tp,Env).

manageConstraints(constrained(Tp,implementsFace(TV,Fc)),Cons,Lc,
		  V,MTp,Env,Ev,overaccess(Exp,TV,Fc)) :- !,
  manageConstraints(Tp,Cons,Lc,V,MTp,Env,Ev,Exp).
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

declareMtd(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkMtd(Nm),Tp),Env,Ev).
declareEnum(Lc,Nm,_FullNm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkEnum(Nm),Tp),Env,Ev).
declareCns(Lc,Nm,_FullNm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,vartypes:mkCns(Nm),Tp),Env,Ev).

mkMtd(Nm,Lc,Tp,mtd(Lc,Nm,Tp)).
mkCns(Nm,Lc,Tp,cons(Lc,Nm,Tp)).
mkEnum(Nm,Lc,Tp,enm(Lc,Nm,ETp)) :- netEnumType(Tp,ETp).

gtType(Tp,Env,Type) :-
  freshen(Tp,Env,_,Type).
faceTp(Tp,Env,Face) :-
  faceOfType(Tp,Env,Face).
