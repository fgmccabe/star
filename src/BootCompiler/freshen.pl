:- module(freshen,[freshen/5,freshenConstraint/5,frshnConstraint/4,
  bindConstraint/1,rewriteType/4,
  evidence/4,contractEvidence/4,freezeType/3]).

:- use_module(misc).
:- use_module(types).
:- use_module(dict).

freshen(Tp,Env,B,Bx,FTp) :-
  deQuant(Tp,B,Bx,T0),
  freshn(T0,Env,Bx,FTp),!.

freshenConstraint(Q,Qx,Con,E,FCon) :-
  boundVars(Q,Qx),
  frshnConstraint(Con,E,Qx,FCon).

boundVars([],[]).
boundVars([kVar(V)|L],[(V,TV)|Lx]) :- newTypeVar(V,TV),
  boundVars(L,Lx).
boundVars([kFun(V,_)|L],[(V,TV)|Lx]) :- newTypeVar(V,TV),
  boundVars(L,Lx).

hasQuants(univType(_,_)).

deQuant(univType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(univType(kFun(V,_),Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(Tp,B,B,Tp).

evidence(Tp,Env,Q,ProgramType) :-
  skolemize(Tp,[],Q,SkTp),
  freshn(SkTp,Env,Q,ProgramType).

freshn(Tp,Env,Q,FTp) :- deRef(Tp,T),frshn(T,Env,Q,FTp),!.

skolemize(univType(kVar(V),Tp),B,BV,FTp) :- readOnlyTypeVar(V,TV),skolemize(Tp,[(V,TV)|B],BV,FTp).
skolemize(univType(kFun(V,Ar),Tp),B,BV,FTp) :- skolemFun(V,Ar,TV),skolemize(Tp,[(V,TV)|B],BV,FTp).
skolemize(Tp,B,B,Tp).

rewriteType(T,E,Q,WTp) :-
  deRef(T,Tp),
  frshn(Tp,E,Q,WTp).

frshn(anonType,_,_,anonType).
frshn(voidType,_,_,voidType) :- !.
frshn(thisType,E,_,Tp) :- isType("this",E,Tp),!.
frshn(kVar(TV),E,_,Tp) :- isTypeVar(TV,E,Tp),!.
frshn(kVar(TV),_,B,Tp) :- (is_member((TV,Tp),B),! ; Tp=kVar(TV)).
frshn(kFun(TV,Ar),E,_,Tp) :- isTypeVar(TV,E,Tp), Tp=kFun(_,Ar).
frshn(kFun(TV,Ar),_,B,Tp) :- (is_member((TV,Tp),B),! ; Tp=kFun(TV,Ar)).
frshn(refType(T),E,B,refType(FTp)) :- frshn(T,E,B,FTp).
frshn(V,_,_,V) :- isUnbound(V),!.
frshn(type(Nm),_,_,type(Nm)).
frshn(tpFun(Nm,Ar),_,_,tpFun(Nm,Ar)).
frshn(funType(A,R),E,B,funType(FA,FR)) :-
  frshnTypes(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(ptnType(A,R),E,B,ptnType(FA,FR)) :-
  frshnTypes(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(grammarType(A,R),E,B,grammarType(FA,FR)) :-
  frshnTypes(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(consType(A,R),E,B,consType(FA,FR)) :-
  frshnTypes(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(tupleType(L),E,B,tupleType(FL)) :- frshnTypes(L,E,B,FL).
frshn(typeExp(O,A),E,B,typeExp(FO,FA)) :- frshn(O,E,B,FO),frshnTypes(A,E,B,FA).
frshn(univType(kVar(V),Tp),E,B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,E,B0,FTp).
frshn(univType(kFun(V,Ar),Tp),E,B,univType(kFun(V,Ar),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,E,B0,FTp).
frshn(constrained(Tp,Con),E,B,constrained(FTp,FCon)) :-
  rewriteType(Tp,E,B,FTp),
  frshnConstraint(Con,E,B,FCon).
frshn(faceType(L),E,B,faceType(FL)) :-
  frshnFields(L,E,B,FL).
frshn(typeExists(A,R),E,B,typeExists(FA,FR)) :-
  rewriteType(A,E,B,FA),
  rewriteType(R,E,B,FR).
frshn(contractExists(Con,Tp),E,B,contractExists(FCon,FTp)) :-
  rewriteType(Tp,E,B,FTp),
  frshnConstraint(Con,E,B,FCon).

frshnConstraint(Con,_,[],Con) :- !.
frshnConstraint(conTract(Nm,Args,Deps),E,B,conTract(Nm,FArgs,FDeps)) :-
  frshnTypes(Args,E,B,FArgs),
  frshnTypes(Deps,E,B,FDeps).
frshnConstraint(implementsFace(Tp,Els),E,B,implementsFace(FTp,FL)) :-
  frshn(Tp,E,B,FTp),
  frshnFields(Els,E,B,FL).
frshnConstraint(constrained(C1,C2),E,B,constrained(FC1,FC2)) :-
  frshnConstraint(C1,E,B,FC1),
  frshnConstraint(C2,E,B,FC2).

frshnContract(conTract(Nm,Args,Deps),E,B,conTract(Nm,FArgs,FDeps)) :-
  frshnTypes(Args,E,B,FArgs),
  frshnTypes(Deps,E,B,FDeps).
frshnContract(constrained(Con,Other),E,B,constrained(FCon,FOther)) :-
  frshnConstraint(Other,E,B,FOther),
  frshnContract(Con,E,B,FCon).

contractEvidence(Tp,E,Q,Con) :-
  skolemize(Tp,[],Q,SkTp),
  frshnContract(SkTp,E,Q,Con).

bindConstraint(typeExp(Nm,Args)) :-
  bindContract(Args,typeExp(Nm,Args)).
bindConstraint(implementsFace(Tp,Els)) :- deRef(Tp,V), isUnbound(V),!,
  setConstraint(V,implementsFace(Tp,Els)).
bindConstraint(implementsFace(_,_)).

bindContract([],_).
bindContract([E|R],C) :- deRef(E,V), isUnbound(V),
  setConstraint(V,C),
  bindContract(R,C).
bindContract([_|R],C) :-
  bindContract(R,C).

frshnFields([],_,_,[]).
frshnFields([(Nm,A)|L],E,B,[(Nm,FA)|FL]) :- !, deRef(A,DA),frshn(DA,E,B,FA), frshnFields(L,E,B,FL).

frshnTypes([],_,_,[]).
frshnTypes([A|L],E,B,[FA|FL]) :- deRef(A,DA),frshn(DA,E,B,FA), frshnTypes(L,E,B,FL).

freezeType(Tp,B,FrZ) :-
  freeze(Tp,B,FT),
  reQuant(B,B,FT,FrZ).

freeze(T,B,Frzn) :-
  deRef(T,Tp),
  frze(Tp,B,Frzn),!.

frze(voidType,_,voidType).
frze(anonType,_,anonType).
frze(thisType,_,thisType).
frze(kVar(TV),_,kVar(TV)).
frze(kFun(T,A),_,kFun(T,A)).
frze(V,B,kVar(TV)) :- isUnbound(V),is_member((TV,VV),B), deRef(VV,VVV), isIdenticalVar(tVar(V),VVV), !.
frze(V,_,V) :- isUnbound(V),!.
frze(type(Nm),_,type(Nm)).
frze(tpFun(Nm,Ar),_,tpFun(Nm,Ar)).
frze(funType(A,R),B,funType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
frze(ptnType(A,R),B,ptnType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
frze(grammarType(A,R),B,grammarType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
frze(consType(A,R),B,consType(FA,FR)) :-
  freezeTypes(A,B,FA),
  freeze(R,B,FR).
frze(tupleType(L),B,tupleType(FL)) :- freezeTypes(L,B,FL).
frze(typeExp(O,A),B,typeExp(FO,FA)) :- freeze(O,B,FO),freezeTypes(A,B,FA).
frze(univType(kVar(V),Tp),B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  freeze(Tp,B0,FTp).
frze(faceType(L),B,faceType(FL)) :-
  freezeFields(L,B,FL).
frze(implementsFace(V,L),B,implementsFace(FV,FL)) :-
  freezeType(V,B,FV),
  freezeFields(L,B,FL).
frze(typeExists(A,R),B,typeExists(FA,FR)) :-
  freeze(A,B,FA),
  freeze(R,B,FR).
frze(constrained(Tp,Con),B,constrained(FTp,FCon)) :-
  freeze(Tp,B,FTp),
  freezeConstraint(Con,B,FCon).

freezeFields([],_,[]).
freezeFields([(Nm,A)|L],B,[(Nm,FA)|FL]) :- !, freeze(A,B,FA), freezeFields(L,B,FL).
freezeFields([A|L],B,[FA|FL]) :- freeze(A,B,FA), freezeFields(L,B,FL).

freezeTypes([],_,[]).
freezeTypes([A|L],B,[FA|FL]) :- freeze(A,B,FA), freezeTypes(L,B,FL).

reQuant([],_,T,T).
reQuant([(_,Tp)|R],BB,T,FZT) :-
  deRef(Tp,V), isUnbound(V),!,
  constraints(V,C),
  freezeConstraints(C,BB,T,FT),
  reQuant(R,BB,FT,FZT).
reQuant([_|B],BB,T,FT) :- reQuant(B,BB,T,FT).

freezeConstraints(C,_,T,T) :- var(C),!.
freezeConstraints([C|_],_,T,T) :- var(C),!.
freezeConstraints([C|R],BB,T,FT) :-
  freezeConstraint(C,BB,FC),
  freezeConstraints(R,BB,constrained(T,FC),FT).

freezeConstraint(conTract(Nm,A,D),B,conTract(Nm,FA,FD)) :-
  freezeTypes(A,B,FA),
  freezeTypes(D,B,FD).
freezeConstraint(implementsFace(T,F),B,implementsFace(FT,FF)) :-
  freeze(T,B,FT),
  freezeFields(F,B,FF).
