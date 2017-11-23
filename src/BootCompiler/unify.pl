:- module(unify,[sameType/3,smList/3,faceOfType/3,sameContract/3,subFace/3]).

:- use_module(misc).
:- use_module(dict).
:- use_module(types).
:- use_module(freshen).

sameType(T1,T2,Env) :- deRef(T1,Tp1), deRef(T2,Tp2), sm(Tp1,Tp2,Env), !.

sm(_,anonType,_).
sm(anonType,_,_).
sm(voidType,voidType,_).
sm(thisType,thisType,_) :-!.
sm(thisType,T2,Env) :- isVar("this",Env,vrEntry(_,_,T1,_)),!,
  sameType(T1,T2,Env).
sm(T1,thisType,Env) :- isVar("this",Env,vrEntry(_,_,T2,_)),!,
  sameType(T1,T2,Env).
sm(kVar(Nm),kVar(Nm),_).
sm(kFun(Nm,Ar),kFun(Nm,Ar),_).
sm(V1,V2,Env) :- isUnbound(V1), isUnbound(V2), varBinding(V1,V2,Env).
sm(V1,T2,Env) :- isUnbound(V1), checkBinding(V1,T2,Env).
sm(T1,V2,Env) :- isUnbound(V2), checkBinding(V2,T1,Env).
sm(type(Nm),type(Nm),_).
sm(tpFun(Nm,Ar),tpFun(Nm,Ar),_).
sm(typeExp(O1,A1),typeExp(O2,A2),Env) :- sameType(O1,O2,Env),smList(A1,A2,Env).
sm(refType(A1),refType(A2),Env) :- sameType(A1,A2,Env).
sm(tupleType(A1),tupleType(A2),Env) :- smList(A1,A2,Env).
sm(funType(A1,R1),funType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
sm(ptnType(A1,R1),ptnType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
sm(consType(A1,R1),consType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A1,A2,Env).
sm(faceType(E1,T1),faceType(E2,T2),Env) :- sameLength(E1,E2),
    sameLength(T1,T2),
    smFields(E1,E2,Env),
    smFields(T1,T2,Env).
sm(existType(K,T1),existType(K,T2),Env) :-
  sameType(T1,T2,Env).
sm(existType(kVar(K1),T1),existType(kVar(K2),T2),Env) :-
  rewriteType(T2,Env,[(K2,K1)],TT2),
  sameType(T1,TT2,Env).
sm(allType(K,T1),allType(K,T2),Env) :-!,
sameType(T1,T2,Env).
sm(allType(kVar(K1),T1),allType(kVar(K2),T2),Env) :-
rewriteType(T2,Env,[(K2,K1)],TT2),
sameType(T1,TT2,Env).

varBinding(T1,T2,_) :- isIdenticalVar(T1,T2),!.
varBinding(T1,T2,Env) :-
  constraints(T1,C1),
  constraints(T2,C2),
  mergeConstraints(C2,C1,Env),
  bind(T1,T2).

sameLength(L1,L2) :- length(L1,L), length(L2,L).

mergeConstraints(C2,C1,Env) :- var(C2),!, copyConstraints(C2,C1,Env).
mergeConstraints([C|R],C1,Env) :- var(C),!, copyConstraints([C|R],C1,Env).
mergeConstraints([_,R],C1,Env) :- mergeConstraints(R,C1,Env).

copyConstraints(_,C,_) :- var(C),!.
copyConstraints(_,[C|_],_) :- var(C),!.
copyConstraints([C|M],[C|R],Env) :- copyConstraints(M,R,Env).

checkBinding(V,Tp,Env) :-
  constraints(V,C),
  bind(V,Tp),
  checkConstraints(C,Env).

checkConstraints(C,_) :- var(C),!.
checkConstraints([C|_],_) :- var(C),!.
checkConstraints([C|M],Env) :- checkConstraint(C,Env),!,
  checkConstraints(M,Env).

checkConstraint(conTract(Nm,Args,Deps),Env) :-
  (surfaceBound(Args) -> checkForImpl(conTract(Nm,Args,Deps),Env) ; true).
checkConstraint(implementsFace(Tp,Face),Env) :-
  faceOfType(Tp,Env,faceType(TpFace,_)),
  checkFace(Face,TpFace,Env).

surfaceBound([]) :-!.
surfaceBound([E|M]) :- deRef(E,EE), \+isUnbound(EE), surfaceBound(M).

checkForImpl(conTract(Nm,Args,Deps),Env) :-
  getImplementations(Nm,Env,Impls),
  is_member(implDef(_,_,_,ITp),Impls),
  freshen(ITp,Env,_,FTp),
  sameType(FTp,conTract(Nm,Args,Deps),Env),!.

sameContract(conTract(Nm,A1,D1),conTract(Nm,A2,D2),Env) :-
  smList(A1,A2,Env),
  smList(D1,D2,Env).

checkFace([],_,_) :-!.
checkFace([(Nm,ElTp)|R],TpFace,Env) :-
  is_member((Nm,XTp), TpFace),
  sameType(ElTp,XTp,Env),
  checkFace(R,TpFace,Env).

smList([],[],_).
smList([E1|L1],[E2|L2],Env) :- sameType(E1,E2,Env), smList(L1,L2,Env).

smFields(_,[],_).
smFields(L1,[(F2,E2)|L2],Env) :- is_member((F2,E1),L1), sameType(E1,E2,Env), smFields(L1,L2,Env).

subFace(faceType(F1,T1),Env,faceType(F2,T2)) :-
  forall(is_member((Nm,Tp1),F1),(is_member((Nm,Tp2),F2),sameType(Tp1,Tp2,Env))),
  forall(is_member((Nm,Tp1),T1),(is_member((Nm,Tp2),T2),sameType(Tp1,Tp2,Env))).

faceOfType(T,Env,Face) :-
  deRef(T,Tp),
  getFace(Tp,Env,Face).

getFace(type(Nm),Env,Face) :- !,
  isType(Nm,Env,tpDef(_,_,FaceRule)),
  freshen(FaceRule,Env,_,typeExists(Lhs,FTp)),
  sameType(type(Nm),Lhs,Env),!,
  getFace(FTp,Env,Face).
getFace(typeExp(Op,Args),Env,Face) :- deRef(Op,tpFun(Nm,Ar)), length(Args,Ar),!,
  isType(Nm,Env,tpDef(_,_,FaceRule)),
  freshen(FaceRule,Env,_,Rl),
  moveConstraints(Rl,_,typeExists(Lhs,FTp)),
  sameType(Lhs,typeExp(Op,Args),Env),!,
  getFace(FTp,Env,Face).
getFace(T,Env,faceType(Face,[])) :- isUnbound(T), !, % fix me - implement types
  constraints(T,C),
  collectImplements(C,Env,[],Face).
getFace(T,Env,faceType(Face,[])) :- isKvar(T),!,
  collectImplementsConstraints(T,Env,Face).
getFace(faceType(Face,Tps),_,faceType(Face,Tps)) :- !.
getFace(existType(V,T),Env,existType(V,F)) :-
  faceOfType(T,Env,F).
getFace(allType(V,T),Env,allType(V,F)) :-
  faceOfType(T,Env,F).

isKvar(V) :- deRef(V,kVar(_)).

collectImplementsConstraints(T,Env,Face) :-
  allImplements(T,Env,Face).

collectImplements(C,_,Face,Face) :- var(C),!.
collectImplements([C|_],_,Face,Face) :- var(C),!.
collectImplements([implementsFace(_,F)|R],Env,SoFar,Face) :-!,
  mergeFace(F,Env,SoFar,SF),
  collectImplements(R,Env,SF,Face).
collectImplements([typeExp(_,_)|R],Env,SoFar,Face) :-!,
  collectImplements(R,Env,SoFar,Face).

mergeFace([],_,Face,Face) :-!.
mergeFace([(Nm,Tp)|R],Env,SoFar,Face) :- is_member((Nm,STp),SoFar),!,
  sameType(Tp,STp,Env),
  mergeFace(R,Env,SoFar,Face).
mergeFace([(Nm,Tp)|R],Env,SoFar,Face) :- mergeFace(R,Env,[(Nm,Tp)|SoFar],Face).
