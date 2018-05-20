:- module(unify,[sameType/3,smList/3,faceOfType/3,sameContract/3,subFace/3,
    applyTypeFun/4,simplifyType/5]).

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
sm(V1,T2,Env) :- isUnbound(V1), varBinding(V1,T2,Env).
sm(T1,V2,Env) :- isUnbound(V2), varBinding(V2,T1,Env).
sm(type(Nm),type(Nm),_).
sm(tpFun(Nm,Ar),tpFun(Nm,Ar),_).
sm(typeExp(O1,A1),T2,Env) :-
  isTypeFun(O1),!,
  freshen(O1,Env,_,OO),
  applyTypeFun(OO,A1,Env,T1),
  sameType(T1,T2,Env).
sm(T1,typeExp(O2,A2),Env) :-
  isTypeFun(O2),!,
  freshen(O2,Env,_,OO),
  applyTypeFun(OO,A2,Env,T2),
  sameType(T1,T2,Env).
sm(typeExp(O1,A1),typeExp(O2,A2),Env) :- sameType(O1,O2,Env),smList(A1,A2,Env).
sm(refType(A1),refType(A2),Env) :- sameType(A1,A2,Env).
sm(tupleType(A1),tupleType(A2),Env) :- smList(A1,A2,Env).
sm(funType(A1,R1),funType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
sm(typeLambda(A1,R1),typeLambda(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
sm(ptnType(A1,R1),ptnType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
sm(consType(A1,R1),consType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A1,A2,Env).
sm(faceType(E1,T1),faceType(E2,T2),Env) :- sameLength(E1,E2),
    sameLength(T1,T2),
    smFields(E1,E2,Env),
    smFields(T1,T2,Env).
sm(existType(K,T1),existType(K,T2),Env) :-
  sameType(T1,T2,Env).
sm(existType(kVar(K1),T1),existType(kVar(K2),T2),Env) :-
  rewriteType(T2,Env,[(K2,kVar(K1))],[],TT2),
  sameType(T1,TT2,Env).
sm(existType(kFun(K1,Ar),T1),existType(kFun(K2,Ar),T2),Env) :-
  rewriteType(T2,Env,[(K2,kFun(K1,Ar))],[],TT2),
  sameType(T1,TT2,Env).
sm(allType(K,T1),allType(K,T2),Env) :-!,
  sameType(T1,T2,Env).
sm(allType(kVar(K1),T1),allType(kVar(K2),T2),Env) :-
  rewriteType(T2,Env,[(K2,kVar(K1))],[],TT2),
  sameType(T1,TT2,Env).
sm(allType(kFun(K1,Ar),T1),allType(kFun(K2,Ar),T2),Env) :-
  rewriteType(T2,Env,[(K2,kFun(K1,Ar))],[],TT2),
  sameType(T1,TT2,Env).

varBinding(T1,T2,_) :- isIdenticalVar(T1,T2),!.
varBinding(T1,T2,_) :-
  bind(T1,T2).

sameLength(L1,L2) :- length(L1,L), length(L2,L).

mergeConstraints(C2,C1,Env) :- var(C2),!, copyConstraints(C2,C1,Env).
mergeConstraints([C|R],C1,Env) :- var(C),!, copyConstraints([C|R],C1,Env).
mergeConstraints([_,R],C1,Env) :- mergeConstraints(R,C1,Env).

copyConstraints(_,C,_) :- var(C),!.
copyConstraints(_,[C|_],_) :- var(C),!.
copyConstraints([C|M],[C|R],Env) :- copyConstraints(M,R,Env).

sameContract(conTract(Nm,A1,D1),conTract(Nm,A2,D2),Env) :-
  smList(A1,A2,Env),
  smList(D1,D2,Env).

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
getFace(T,Env,Face) :- deRef(T,TT),isUnbound(TT), !, % fix me - implement types
  collectImplements(TT,Env,Face).
getFace(T,Env,Face) :- isKvar(T),!,
  collectImplements(T,Env,Face).
getFace(faceType(Face,Tps),_,faceType(Face,Tps)) :- !.
getFace(Tp,_,faceType([],[])) :-
  isProgramType(Tp),!.
getFace(existType(V,T),Env,existType(V,F)) :-
  faceOfType(T,Env,F).
getFace(allType(V,T),Env,allType(V,F)) :-
  faceOfType(T,Env,F).

isKvar(V) :- deRef(V,kVar(_)).

collectImplements(V,Env,faceType(Fields,Types)) :-
  allConstraints(Env,Cons),
  pickupImplements(V,Cons,Env,[],Fields,[],Types).

pickupImplements(_,[],_,F,F,T,T).
pickupImplements(V,[implementsFace(TV,faceType(Fv,Tv))|Cons],Env,F,Fx,T,Tx) :-
  isIdenticalVar(V,TV),!,
  mergeFields(F,Env,Fv,F0),
  mergeFields(T,Env,Tv,T0),
  pickupImplements(V,Cons,Env,F0,Fx,T0,Tx).
pickupImplements(V,[_|Cons],Env,F,Fx,T,Tx) :-
  pickupImplements(V,Cons,Env,F,Fx,T,Tx).

mergeFields([],_,Face,Face) :-!.
mergeFields([(Nm,Tp)|R],Env,SoFar,Face) :- is_member((Nm,STp),SoFar),!,
  sameType(Tp,STp,Env),
  mergeFields(R,Env,SoFar,Face).
mergeFields([(Nm,Tp)|R],Env,SoFar,Face) :- mergeFields(R,Env,[(Nm,Tp)|SoFar],Face).

applyTypeFun(typeLambda(LA,Tp),Args,Env,Tp) :-
  sameType(tupleType(Args),LA,Env),!.

simplifyType(T,Env,C,Cx,Tp) :-
  deRef(T,TT),!,
  smpTp(TT,Env,C,Cx,Tp).

smpTp(anonType,_,C,C,anonType).
smpTp(voidType,_,C,C,voidType).
smpTp(thisType,_,C,C,thisType).
smpTp(type(Nm),_,C,C,type(Nm)).
smpTp(typeExp(O,A),Env,C,Cx,Tp) :-
  isTypeFun(O),!,
  smpTps(A,Env,C,C0,As),
  freshen(O,Env,_,OO),
  applyTypeFun(OO,As,Env,T1),
  simplifyType(T1,Env,C0,Cx,Tp).
smpTp(typeExp(O,A),Env,C,Cx,typeExp(OO,As)) :-
  simplifyType(O,Env,C,C0,OO),
  smpTps(A,Env,C0,Cx,As).
smpTp(kVar(V),_,C,C,kVar(V)).
smpTp(tVar(Vx,Nm,Id),_,Cx,Cx,tVar(Vx,Nm,Id)).
smpTp(tFun(Vx,Nm,Ar,Id),_,Cx,Cx,tFun(Vx,Nm,Ar,Id)).
smpTp(tpFun(Id,Ar),_,Cx,Cx,tpFun(Id,Ar)).
smpTp(refType(T),Env,C,Cx,refType(Tp)) :-
  simplifyType(T,Env,C,Cx,Tp).
smpTp(tupleType(A),Env,C,Cx,tupleType(As)) :-
  smpTps(A,Env,C,Cx,As).
smpTp(funType(L,R),Env,C,Cx,funType(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  simplifyType(R,Env,C0,Cx,Rs).
smpTp(ptnType(L,R),Env,C,Cx,ptnType(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  simplifyType(R,Env,C0,Cx,Rs).
smpTp(consType(L,R),Env,C,Cx,consType(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  simplifyType(R,Env,C0,Cx,Rs).
smpTp(allType(V,T),Env,C,C,allType(V,Tp)) :-
  simplifyType(T,Env,[],Cx,In),
  wrapConstraints(Cx,In,Tp).
smpTp(existType(V,T),Env,Cx,Cx,existType(V,Tp)) :-
  simplifyType(T,Env,[],C,ITp),
  wrapConstraints(C,ITp,Tp).
smpTp(faceType(F,T),Env,C,Cx,faceType(Fs,Ts)) :-
  smpFldTps(F,Env,C,C0,Fs),
  smpFldTps(T,Env,C0,Cx,Ts).
smpTp(typeLambda(L,R),Env,C,Cx,typeLambda(L,Rs)) :-
  simplifyType(R,Env,C,Cx,Rs).
smpTp(constrained(T,Cn),Env,C,Cx,Tp) :-
  smpCon(Cn,Env,C,C0,Cs),
  simplifyType(T,Env,[Cs|C0],Cx,Tp).

smpTps([],_,Cx,Cx,[]).
smpTps([T|Tps],Env,C,Cx,[TT|TTps]) :-
  simplifyType(T,Env,C,C0,TT),
  smpTps(Tps,Env,C0,Cx,TTps).

smpFldTps([],_,C,C,[]).
smpFldTps([(F,T)|Flds],Env,C,Cx,[(F,Tp)|Fs]) :-
  simplifyType(T,Env,C,C0,Tp),
  smpFldTps(Flds,Env,C0,Cx,Fs).

wrapConstraints([],Tp,Tp).
wrapConstraints([Con|C],Tp,WTp) :-
  wrapConstraints(C,constrained(Tp,Con),WTp).

smpCon(conTract(Nm,L,R),Env,C,Cx,conTract(Nm,Ls,Rs)) :-
  smpTps(L,Env,C,C0,Ls),
  smpTps(R,Env,C0,Cx,Rs).
smpCon(implementsFace(L,R),Env,C,Cx,implementsFace(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  smpFldTps(R,Env,C0,Cx,Rs).
