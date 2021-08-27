:- module(freshen,[freshen/4,
		   rewriteType/5,
		   evidence/4]).

:- use_module(misc).
:- use_module(types).
:- use_module(dict).

freshen(Tp,Env,Bx,FTp) :-
  deRef(Tp,T),
  freshQuants(T,[],Bx,T0),
  freshn(T0,Env,Bx,[],FTp),!.

boundVars([],[]).
boundVars([kVar(V)|L],[(V,TV)|Lx]) :- newTypeVar(V,TV),
  boundVars(L,Lx).
boundVars([kFun(V,Ar)|L],[(V,TV)|Lx]) :- newTypeFun(V,Ar,TV),
  boundVars(L,Lx).

hasQuants(allType(_,_)).

freshQuants(allType(kVar(V),Tp),B,BV,FTp) :-
  newTypeVar(V,TV),
  deRef(Tp,T),
  freshQuants(T,[(V,TV)|B],BV,FTp).
freshQuants(allType(kFun(V,Ar),Tp),B,BV,FTp) :-
  newTypeFun(V,Ar,TV),
  deRef(Tp,T),
  freshQuants(T,[(V,TV)|B],BV,FTp).
freshQuants(existType(kVar(V),Tp),B,BV,FTp) :-
  genSkolemFun(V,B,TV),
  deRef(Tp,T),
  freshQuants(T,[(V,TV)|B],BV,FTp).
freshQuants(constrained(Tp,Con),B,BV,constrained(FTp,Con)) :-
  freshQuants(Tp,B,BV,FTp).
freshQuants(Tp,B,B,Tp).

genSkolemFun(Nm,[],V) :-
  skolemVar(Nm,V).
genSkolemFun(Nm,Q,Tp) :-
  length(Q,Ar),
  skolemFun(Nm,Ar,NN),
  rfold(Q,freshen:skExp,NN,Tp).

skExp((_,V),T,tpExp(T,V)).

evidence(Tp,Env,Q,ProgramType) :-
  deRef(Tp,T),
  skolemize(T,[],Q,SkTp),
  freshn(SkTp,Env,Q,[],ProgramType).

freshn(Tp,Env,Q,Ex,FTp) :- deRef(Tp,T),frshn(T,Env,Q,Ex,FTp),!.

skolemize(allType(kVar(V),Tp),B,BV,FTp) :- skolemVar(V,TV),deRef(Tp,T),skolemize(T,[(V,TV)|B],BV,FTp).
skolemize(allType(kFun(V,Ar),Tp),B,BV,FTp) :- skolemFun(V,Ar,TV),deRef(Tp,T),skolemize(T,[(V,TV)|B],BV,FTp).
skolemize(existType(kVar(V),Tp),B,BV,FTp) :- newTypeVar(V,TV), deRef(Tp,T),skolemize(T,[(V,TV)|B],BV,FTp).
skolemize(Tp,B,B,Tp).

rewriteType(T,E,Q,Ex,WTp) :-
  deRef(T,Tp),
  frshn(Tp,E,Q,Ex,WTp).

frshn(anonType,_,_,_,anonType).
frshn(voidType,_,_,_,voidType) :- !.
frshn(kVar(TV),E,B,Ex,Tp) :-
  is_member(kVar(TV),Ex) ->
    Tp=kVar(TV);
    ( is_member((TV,Tp),B),! ;
      isTypeVar(TV,E,Tp),!;
      Tp=kVar(TV)
    ).
frshn(kFun(TV,Ar),E,B,Ex,Tp) :-
  is_member(kFun(TV,Ar),Ex) ->
    Tp=kFun(TV,Ar) ;
    ( is_member((TV,Tp),B),! ;
      isTypeVar(TV,E,Tp), Tp=kFun(_,Ar),!;
      Tp=kFun(TV,Ar)).
frshn(refType(T),E,B,Ex,refType(FTp)) :- rewriteType(T,E,B,Ex,FTp).
frshn(V,_,_,_,V) :- isUnbound(V),!.
frshn(type(Nm),_,_,_,type(Nm)).
frshn(tpFun(Nm,Ar),_,_,_,tpFun(Nm,Ar)).
frshn(funType(A,R),E,B,Ex,funType(FA,FR)) :-
  rewriteType(A,E,B,Ex,FA),
  rewriteType(R,E,B,Ex,FR).
frshn(consType(A,R),E,B,Ex,consType(FA,FR)) :-
  rewriteType(A,E,B,Ex,FA),
  rewriteType(R,E,B,Ex,FR).
frshn(contType(A,R),E,B,Ex,contType(FA,FR)) :-
  rewriteType(A,E,B,Ex,FA),
  rewriteType(R,E,B,Ex,FR).
frshn(tplType(L),E,B,Ex,tplType(FL)) :- rewriteTypes(L,E,B,Ex,FL).
frshn(tpExp(O,A),E,B,Ex,tpExp(FO,FA)) :-
  rewriteType(O,E,B,Ex,FO),
  rewriteType(A,E,B,Ex,FA).
frshn(allType(V,Tp),E,B,Ex,allType(V,FTp)) :-
  rewriteType(Tp,E,B,[V|Ex],FTp).
frshn(existType(V,Tp),E,B,Ex,existType(V,FTp)) :-
  rewriteType(Tp,E,B,[V|Ex],FTp).
frshn(constrained(Tp,Con),E,B,Ex,constrained(FTp,FCon)) :-
  rewriteType(Tp,E,B,Ex,FTp),
  frshnConstraint(Con,bind,E,B,Ex,FCon).
frshn(faceType(L,T),E,B,Ex,faceType(FL,FT)) :-
  frshnFields(L,E,B,Ex,FL),
  frshnFields(T,E,B,Ex,FT).
frshn(typeExists(A,R),E,B,Ex,typeExists(FA,FR)) :-
  rewriteType(A,E,B,Ex,FA),
  rewriteType(R,E,B,Ex,FR).
frshn(typeLambda(A,R),E,B,Ex,typeLambda(FA,FR)) :-
  rewriteType(A,E,B,Ex,FA),
  rewriteType(R,E,B,Ex,FR).
frshn(contractExists(Con,Tp),E,B,Ex,contractExists(FCon,FTp)) :-
  rewriteType(Tp,E,B,Ex,FTp),
  frshnConstraint(Con,nobind,E,B,Ex,FCon).

frshnConstraint(Con,_,_,[],_,Con) :- !.
frshnConstraint(conTract(Nm,Args,Deps),Bnd,E,B,Ex,Con) :-
  rewriteTypes(Args,E,B,Ex,FArgs),
  rewriteTypes(Deps,E,B,Ex,FDeps),
  Con = conTract(Nm,FArgs,FDeps),
  (Bnd=nobind ;
    bindConstraints(FArgs,Con),
    bindConstraints(FDeps,Con)).
frshnConstraint(implementsFace(Tp,Face),Bnd,E,B,Ex,implementsFace(FTp,FFace)) :-
  rewriteType(Tp,E,B,Ex,FTp),
  rewriteType(Face,E,B,Ex,FFace),
  (Bnd=nobind ; bindConstraints([FTp],implementsFace(FTp,FFace))).
frshnConstraint(constrained(C1,C2),Bnd,E,B,Ex,constrained(FC1,FC2)) :-
  frshnConstraint(C1,Bnd,E,B,Ex,FC1),
  frshnConstraint(C2,Bnd,E,B,Ex,FC2).

bindConstraints([],_) :-!.
bindConstraints([V|A],C) :- isUnbound(V),!,
  addConstraint(V,C),
  bindConstraints(A,C).
bindConstraints([_|A],C) :-
  bindConstraints(A,C).

frshnFields([],_,_,_,[]).
frshnFields([(Nm,A)|L],E,B,Ex,[(Nm,FA)|FL]) :-
  rewriteType(A,E,B,Ex,FA),
  frshnFields(L,E,B,Ex,FL).

rewriteTypes([],_,_,_,[]).
rewriteTypes([A|L],E,B,Ex,[FA|FL]) :- rewriteType(A,E,B,Ex,FA), rewriteTypes(L,E,B,Ex,FL).

