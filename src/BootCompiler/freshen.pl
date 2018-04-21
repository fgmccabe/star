:- module(freshen,[freshen/4,freshenConstraint/5,frshnConstraint/4,
  rewriteConstraints/4,
  rewriteType/4,rewriteTypes/4,
  evidence/4]).

:- use_module(misc).
:- use_module(types).
:- use_module(dict).

freshen(Tp,Env,Bx,FTp) :-
  deRef(Tp,T),
  freshQuants(T,[],Bx,T0),
  freshn(T0,Env,Bx,FTp),!.

freshenConstraint(Q,Qx,Con,E,FCon) :-
  boundVars(Q,Qx),
  frshnConstraint(Con,E,Qx,FCon).

rewriteConstraints([],_,_,[]).
rewriteConstraints([Con|Cons],Env,Q,[FCon|Cx]) :-
  frshnConstraint(Con,Env,Q,FCon),
  rewriteConstraints(Cons,Env,Q,Cx).

boundVars([],[]).
boundVars([kVar(V)|L],[(V,TV)|Lx]) :- newTypeVar(V,TV),
  boundVars(L,Lx).
boundVars([kFun(V,_)|L],[(V,TV)|Lx]) :- newTypeVar(V,TV),
  boundVars(L,Lx).

hasQuants(allType(_,_)).

freshQuants(allType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV),deRef(Tp,T),freshQuants(T,[(V,TV)|B],BV,FTp).
freshQuants(allType(kFun(V,_),Tp),B,BV,FTp) :- newTypeVar(V,TV),deRef(Tp,T),freshQuants(T,[(V,TV)|B],BV,FTp).
freshQuants(existType(kVar(V),Tp),B,BV,FTp) :- genSkolemFun(V,B,TV),deRef(Tp,T),freshQuants(T,[(V,TV)|B],BV,FTp).
freshQuants(Tp,B,B,Tp).

genSkolemFun(Nm,[],V) :-
  skolemVar(Nm,V).
genSkolemFun(Nm,Q,typeExp(NN,Args)) :-
  length(Q,Ar),
  skolemFun(Nm,Ar,NN),
  project1(Q,Args).

evidence(Tp,Env,Q,ProgramType) :-
  deRef(Tp,T),
  skolemize(T,[],Q,SkTp),
  freshn(SkTp,Env,Q,ProgramType).

freshn(Tp,Env,Q,FTp) :- deRef(Tp,T),frshn(T,Env,Q,FTp),!.

skolemize(allType(kVar(V),Tp),B,BV,FTp) :- skolemVar(V,TV),deRef(Tp,T),skolemize(T,[(V,TV)|B],BV,FTp).
skolemize(allType(kFun(V,Ar),Tp),B,BV,FTp) :- skolemFun(V,Ar,TV),deRef(Tp,T),skolemize(T,[(V,TV)|B],BV,FTp).
skolemize(existType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV), deRef(Tp,T),skolemize(T,[(V,TV)|B],BV,FTp).
skolemize(Tp,B,B,Tp).

rewriteType(T,E,Q,WTp) :-
  deRef(T,Tp),
  frshn(Tp,E,Q,WTp).

frshn(anonType,_,_,anonType).
frshn(voidType,_,_,voidType) :- !.
frshn(thisType,E,_,Tp) :- isType("this",E,Tp),!.
frshn(kVar(TV),E,B,Tp) :- (
  is_member((TV,Tp),B),! ;
  isTypeVar(TV,E,Tp),!;
  Tp=kVar(TV)).
frshn(kFun(TV,Ar),E,B,Tp) :- (
  is_member((TV,Tp),B),! ;
  isTypeVar(TV,E,Tp), Tp=kFun(_,Ar),!;
  Tp=kFun(TV,Ar)).
frshn(refType(T),E,B,refType(FTp)) :- frshn(T,E,B,FTp).
frshn(V,_,_,V) :- isUnbound(V),!.
frshn(type(Nm),_,_,type(Nm)).
frshn(tpFun(Nm,Ar),_,_,tpFun(Nm,Ar)).
frshn(funType(A,R),E,B,funType(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(ptnType(A,R),E,B,ptnType(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(consType(A,R),E,B,consType(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(grType(A,R),E,B,grType(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(tupleType(L),E,B,tupleType(FL)) :- rewriteTypes(L,E,B,FL).
frshn(typeExp(O,A),E,B,typeExp(FO,FA)) :- rewriteType(O,E,B,FO),rewriteTypes(A,E,B,FA).
frshn(allType(kVar(V),Tp),E,B,allType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,E,B0,FTp).
frshn(allType(kFun(V,Ar),Tp),E,B,allType(kFun(V,Ar),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,E,B0,FTp).
frshn(existType(kVar(V),Tp),E,B,existType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,E,B0,FTp).
frshn(existType(kFun(V,Ar),Tp),E,B,existType(kFun(V,Ar),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,E,B0,FTp).
frshn(constrained(Tp,Con),E,B,constrained(FTp,FCon)) :-
  rewriteType(Tp,E,B,FTp),
  frshnConstraint(Con,E,B,FCon).
frshn(faceType(L,T),E,B,faceType(FL,FT)) :-
  frshnFields(L,E,B,FL),
  frshnFields(T,E,B,FT).
frshn(typeExists(A,R),E,B,typeExists(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(typeLambda(A,R),E,B,typeLambda(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(contractExists(Con,Tp),E,B,contractExists(FCon,FTp)) :-
  rewriteType(Tp,E,B,FTp),
  frshnConstraint(Con,E,B,FCon).

frshnConstraint(Con,_,[],Con) :- !.
frshnConstraint(conTract(Nm,Args,Deps),E,B,conTract(Nm,FArgs,FDeps)) :-
  rewriteTypes(Args,E,B,FArgs),
  rewriteTypes(Deps,E,B,FDeps).
frshnConstraint(implementsFace(Tp,Face),E,B,implementsFace(FTp,FFace)) :-
  rewriteType(Tp,E,B,FTp),
  rewriteType(Face,E,B,FFace).
frshnConstraint(constrained(C1,C2),E,B,constrained(FC1,FC2)) :-
  frshnConstraint(C1,E,B,FC1),
  frshnConstraint(C2,E,B,FC2).

frshnFields([],_,_,[]).
frshnFields([(Nm,A)|L],E,B,[(Nm,FA)|FL]) :- !, rewriteType(A,E,B,FA), frshnFields(L,E,B,FL).

rewriteTypes([],_,_,[]).
rewriteTypes([A|L],E,B,[FA|FL]) :- rewriteType(A,E,B,FA), rewriteTypes(L,E,B,FL).
