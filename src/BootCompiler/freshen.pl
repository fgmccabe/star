:- module(freshen,[freshn/3,freshen/5,freshenConstraint/2,frshnConstraint/3,freshenContract/3,freshenContract/4,
  bindConstraint/1,contractedType/2,rewriteType/3,addThisType/3,
  evidence/2,evidence/4,contractEvidence/4,freezeType/3]).

:- use_module(misc).
:- use_module(types).

freshen(Tp,ThisType,B,Bx,FTp) :-
  addThisType(ThisType,B,B0),
  deQuant(Tp,B0,Bx,T0),
  freshn(T0,B,FTp),!.

freshenConstraint(Con,FCon) :-
  deQuant(Con,[],B,C0),
  frshnConstraint(C0,B,FCon).

addThisType(voidType,B,B).
addThisType(Tp,B,[(thisType,Tp)|B]).

hasQuants(univType(_,_)).

deQuant(univType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(univType(kFun(V,_),Tp),B,BV,FTp) :- newTypeVar(V,TV),deQuant(Tp,[(V,TV)|B],BV,FTp).
deQuant(Tp,B,B,Tp).

evidence(Tp,FTp) :- skolemize(Tp,[],B,T0), freshn(T0,B,FTp).

evidence(Tp,ThisType,Q,ProgramType) :-
  addThisType(ThisType,[],B0),
  skolemize(Tp,B0,Q,SkTp),
  frshn(SkTp,Q,ProgramType).

freshn(Tp,[],Tp) :- !.
freshn(Tp,Binding,FTp) :- deRef(Tp,T),frshn(T,Binding,FTp),!.

skolemize(univType(kVar(V),Tp),B,BV,FTp) :- readOnlyTypeVar(V,TV),skolemize(Tp,[(V,TV)|B],BV,FTp).
skolemize(univType(kFun(V,Ar),Tp),B,BV,FTp) :- skolemFun(V,Ar,TV),skolemize(Tp,[(V,TV)|B],BV,FTp).
skolemize(Tp,B,B,Tp).

rewriteType(T,Q,WTp) :-
  deRef(T,Tp),
  frshn(Tp,Q,WTp).

frshn(anonType,_,anonType).
frshn(voidType,_,voidType) :- !.
frshn(thisType,B,Tp) :- (is_member((thisType,Tp),B),! ; Tp=thisType).
frshn(kVar(TV),B,Tp) :- (is_member((TV,Tp),B),! ; Tp=kVar(TV)).
frshn(kFun(TV,Ar),B,Tp) :- (is_member((TV,Tp),B),! ; Tp=kFun(TV,Ar)).
frshn(refType(T),B,refType(FTp)) :- frshn(T,B,FTp).
frshn(V,_,V) :- isUnbound(V),!.
frshn(type(Nm),_,type(Nm)).
frshn(tpFun(Nm,Ar),_,tpFun(Nm,Ar)).
frshn(funType(A,R),B,funType(FA,FR)) :-
  frshnTypes(A,B,FA),
  rewriteType(R,B,FR).
frshn(ptnType(A,R),B,ptnType(FA,FR)) :-
  frshnTypes(A,B,FA),
  rewriteType(R,B,FR).
frshn(grammarType(A,R),B,grammarType(FA,FR)) :-
  frshnTypes(A,B,FA),
  rewriteType(R,B,FR).
frshn(consType(A,R),B,consType(FA,FR)) :-
  frshnTypes(A,B,FA),
  rewriteType(R,B,FR).
frshn(tupleType(L),B,tupleType(FL)) :- frshnTypes(L,B,FL).
frshn(typeExp(O,A),B,typeExp(FO,FA)) :- frshn(O,B,FO),frshnTypes(A,B,FA).
frshn(univType(kVar(V),Tp),B,univType(kVar(V),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,B0,FTp).
frshn(univType(kFun(V,Ar),Tp),B,univType(kFun(V,Ar),FTp)) :-
  subtract((V,_),B,B0),
  rewriteType(Tp,B0,FTp).
frshn(constrained(Tp,Con),B,constrained(FTp,FCon)) :-
  rewriteType(Tp,B,FTp),
  frshnConstraint(Con,B,FCon).
frshn(faceType(L),B,faceType(FL)) :-
  frshnFields(L,B,FL).
frshn(typeExists(A,R),B,typeExists(FA,FR)) :-
  rewriteType(A,B,FA),
  rewriteType(R,B,FR).

frshnConstraint(Con,[],Con) :- !.
frshnConstraint(conTract(Nm,Args,Deps),B,conTract(Nm,FArgs,FDeps)) :-
  frshnTypes(Args,B,FArgs),
  frshnTypes(Deps,B,FDeps).
frshnConstraint(implementsFace(Tp,Els),B,implementsFace(FTp,FL)) :-
  frshn(Tp,B,FTp),
  frshnFields(Els,B,FL).
frshnConstraint(constrained(C1,C2),B,constrained(FC1,FC2)) :-
  frshnConstraint(C1,B,FC1),
  frshnConstraint(C2,B,FC2).

frshnContract(conTract(Nm,Args,Deps),B,conTract(Nm,FArgs,FDeps)) :-
  frshnTypes(Args,B,FArgs),
  frshnTypes(Deps,B,FDeps).
frshnContract(constrained(Con,Other),B,constrained(FCon,FOther)) :-
  frshnConstraint(Other,B,FOther),
  frshnContract(Con,B,FCon).

freshenContract(Con,ThisType,Q,FCon) :-
  addThisType(ThisType,[],B0),
  deQuant(Con,B0,Q,CC),
  frshnContract(CC,Q,FCon),!.

freshenContract(Con,Q,FCon) :-
  deQuant(Con,[],Q,CC),
  frshnContract(CC,Q,FCon),!.

contractEvidence(Tp,ThisType,Q,Con) :-
  addThisType(ThisType,[],B0),
  skolemize(Tp,B0,Q,SkTp),
  frshnContract(SkTp,Q,Con).

contractedType(constrained(Tp,Con),contracted([Con|Contracts],ConTp)) :-
  contracts(Tp,Contracts,ConTp).

contracts(constrained(Tp,Con),[Con|M],ConTp) :-
  Con = typeExp(_,_),!,
  contracts(Tp,M,ConTp).
contracts(constrained(Tp,Con),Cons,ConTp) :-
  bindConstraint(Con),!,
  contracts(Tp,Cons,ConTp).
contracts(Tp,[],Tp).

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

frshnFields([],_,[]).
frshnFields([(Nm,A)|L],B,[(Nm,FA)|FL]) :- !, deRef(A,DA),frshn(DA,B,FA), frshnFields(L,B,FL).

frshnTypes([],_,[]).
frshnTypes([A|L],B,[FA|FL]) :- deRef(A,DA),frshn(DA,B,FA), frshnTypes(L,B,FL).

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
freshenConstraint(implementsFace(T,F),B,implementsFace(FT,FF)) :-
  freeze(T,B,FT),
  freezeFields(F,B,FF).
