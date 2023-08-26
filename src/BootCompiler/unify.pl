:- module(unify,[sameType/4,faceOfType/4,
		 simplifyType/6,applyTypeFun/5]).

:- use_module(misc).
:- use_module(canon).
:- use_module(dict).
:- use_module(types).
:- use_module(freshen).

sameType(T1,T2,Lc,Env) :- deRef(T1,Tp1), deRef(T2,Tp2), sm(Tp1,Tp2,Lc,Env), !.

sm(_,anonType,_,_).
sm(anonType,_,_,_).
sm(voidType,voidType,_,_).
sm(kVar(Nm),kVar(Nm),_,_).
sm(kFun(Nm,Ar),kFun(Nm,Ar),_,_).
sm(V1,V2,Lc,_Env) :- isUnbound(V1), isUnbound(V2),!, varBinding(V1,V2,Lc).
sm(V1,T2,Lc,_Env) :- isUnbound(V1), !, varBinding(V1,T2,Lc).
sm(T1,V2,Lc,_Env) :- isUnbound(V2), !, varBinding(V2,T1,Lc).
sm(type(Nm),type(Nm),_,_).
sm(tpFun(Nm,Ar),tpFun(Nm,Ar),_,_).
sm(tpExp(O1,A1),tpExp(O2,A2),Lc,Env) :- sameType(O1,O2,Lc,Env), sameType(A1,A2,Lc,Env).
sm(valType(A1),valType(A2),Lc,Env) :- sameType(A1,A2,Lc,Env).
sm(tplType(A1),tplType(A2),Lc,Env) :- smList(A1,A2,Lc,Env).
sm(funType(A1,R1),funType(A2,R2),Lc,Env) :- sameType(R1,R2,Lc,Env), sameType(A2,A1,Lc,Env).
sm(typeLambda(A1,R1),typeLambda(A2,R2),Lc,Env) :- sameType(R1,R2,Lc,Env), sameType(A2,A1,Lc,Env).
sm(consType(A1,R1),consType(A2,R2),Lc,Env) :- sameType(R1,R2,Lc,Env), sameType(A1,A2,Lc,Env).
sm(continType(A1,R1),continType(A2,R2),Lc,Env) :-
  sameType(R1,R2,Lc,Env),
  sameType(A1,A2,Lc,Env).
sm(faceType(E1,T1),faceType(E2,T2),Lc,Env) :- sameLength(E1,E2),
    sameLength(T1,T2),
    smFields(E1,E2,Lc,Env),
    smFields(T1,T2,Lc,Env).
sm(existType(K,T1),existType(K,T2),Lc,Env) :-
  sameType(T1,T2,Lc,Env).
sm(existType(kVar(K1),T1),existType(kVar(K2),T2),Lc,Env) :-
  rewriteType(T2,Lc,Env,[(K2,kVar(K1))],TT2),
  sameType(T1,TT2,Lc,Env).
sm(existType(kFun(K1,Ar),T1),existType(kFun(K2,Ar),T2),Lc,Env) :-
  rewriteType(T2,Lc,Env,[(K2,kFun(K1,Ar))],TT2),
  sameType(T1,TT2,Lc,Env).
sm(allType(K,T1),allType(K,T2),Lc,Env) :-!,
  sameType(T1,T2,Lc,Env).
sm(allType(kVar(K1),T1),allType(kVar(K2),T2),Lc,Env) :-
  rewriteType(T2,Lc,Env,[(K2,kVar(K1))],TT2),
  sameType(T1,TT2,Lc,Env).
sm(allType(kFun(K1,Ar),T1),allType(kFun(K2,Ar),T2),Lc,Env) :-
  rewriteType(T2,Lc,Env,[(K2,kFun(K1,Ar))],TT2),
  sameType(T1,TT2,Lc,Env).
sm(constrained(T1,C1),constrained(T2,C2),Lc,Env) :-
  sameType(T1,T2,Lc,Env),
  sameConstraint(C1,C2,Lc,Env).

varBinding(T1,T2,_) :- isIdenticalVar(T1,T2),!.
varBinding(T1,T2,Lc) :-
  bind(T1,T2,Lc).

sameLength(L1,L2) :- length(L1,L), length(L2,L).

sameConstraint(C1,C2,Lc,Env) :-
  sameContract(C1,C2,Lc,Env),!.
sameConstraint(C1,C2,Lc,Env) :-
  sameImplements(C1,C2,Lc,Env).
sameConstraint(raises(T1),raises(T2),Lc,Env) :-
  sameType(T1,T2,Lc,Env).
sameConstraint(implicit(Nm,T1),implicit(Nm,T2),Lc,Env) :-
  sameType(T1,T2,Lc,Env).


sameContract(conTract(Nm,A1,D1),conTract(Nm,A2,D2),Lc,Env) :-
  smpTps(A1,Lc,Env,[],_,AA1),
  smpTps(A2,Lc,Env,[],_,AA2),
  smList(AA1,AA2,Lc,Env),
  smpTps(D1,Lc,Env,[],_,DD1),
  smpTps(D2,Lc,Env,[],_,DD2),
  smList(DD1,DD2,Lc,Env).

smList([],[],_,_).
smList([E1|L1],[E2|L2],Lc,Env) :- sameType(E1,E2,Lc,Env), smList(L1,L2,Lc,Env).

smFields(_,[],_,_).
smFields(L1,[(F2,E2)|L2],Lc,Env) :- is_member((F2,E1),L1), sameType(E1,E2,Lc,Env), smFields(L1,L2,Lc,Env).

subFace(Tp1,Tp2,Lc,Env) :-
  deRef(Tp1,faceType(F1,T1)),
  deRef(Tp2,faceType(F2,T2)),
  check_implies(is_member((Nm,Tp1),F1),(is_member((Nm,Tp2),F2),sameType(Tp1,Tp2,Lc,Env))),
  check_implies(is_member((Nm,Tp1),T1),(is_member((Nm,Tp2),T2),sameType(Tp1,Tp2,Lc,Env))).

isTypeFun(type(Nm),[],_Lc,Env,Tp) :-
  isType(Nm,Env,tpDef(_,_,Rule)),
  isTypeLam(Rule),!,
  freshen(Rule,Env,_,Tp).
isTypeFun(tpExp(Nm,A),[A|Args],Lc,Env,Tp) :-!,
  isTypeFun(Nm,Args,Lc,Env,Tp).
isTypeFun(tpFun(Nm,_),[],_Lc,Env,Tp) :-
  isType(Nm,Env,tpDef(_,_,Rule)),!,
  isTypeLam(Rule),!,
  freshen(Rule,Env,_,Tp).

faceOfType(T,Lc,Env,Face) :-
  simplifyType(T,Lc,Env,_,[],Tp),
  getFace(Tp,Lc,Env,Face).

getFace(type(Nm),Lc,Env,Face) :- 
  (isType(Nm,Env,tpDef(_,_,FaceRule)) ->
   freshen(FaceRule,Env,_,typeExists(Lhs,FTp)),
   sameType(type(Nm),Lhs,Lc,Env),!,
   getFace(FTp,Lc,Env,Face) ;
   Face=faceType([],[])).
getFace(tpExp(Op,Arg),Lc,Env,Face) :-
  isTypeExp(tpExp(Op,Arg),tpFun(Nm,_),_),!,
  isType(Nm,Env,tpDef(_,_,FaceRule)),
  freshen(FaceRule,Env,_,Rl),
  getConstraints(Rl,_,typeExists(Lhs,FTp)),
  sameType(Lhs,tpExp(Op,Arg),Lc,Env),!,
  getFace(FTp,Lc,Env,Face).
getFace(T,Lc,Env,Face) :- deRef(T,TT),isUnbound(TT), !, % fix me - implement types
  collectImplements(TT,Lc,Env,Face).
getFace(T,Lc,Env,Face) :- isKvar(T),!,
  collectImplements(T,Lc,Env,Face).
getFace(faceType(Face,Tps),_,_,faceType(Face,Tps)) :- !.
getFace(Tp,_,_,faceType([],[])) :-
  isProgramType(Tp),!.
getFace(existType(V,T),Lc,Env,existType(V,F)) :-
  faceOfType(T,Lc,Env,F).
getFace(allType(V,T),Lc,Env,allType(V,F)) :-
  faceOfType(T,Lc,Env,F).
getFace(tplType(_),_,_,faceType([],[])).
getFace(valType(T),Lc,Env,Face) :- getFace(T,Lc,Env,Face).

isKvar(V) :- deRef(V,kVar(_)).

collectImplements(V,Lc,Env,faceType(Fields,Types)) :-
  allConstraints(Env,Cons),
  pickupImplements(V,Cons,Lc,Env,[],Fields,[],Types).

pickupImplements(_,[],_,F,F,T,T).
pickupImplements(V,[implementsFace(TV,faceType(Fv,Tv))|Cons],Lc,Env,F,Fx,T,Tx) :-
  isIdenticalVar(V,TV),!,
  mergeFields(F,Lc,Env,Fv,F0),
  mergeFields(T,Lc,Env,Tv,T0),
  pickupImplements(V,Cons,Lc,Env,F0,Fx,T0,Tx).
pickupImplements(V,[_|Cons],Lc,Env,F,Fx,T,Tx) :-
  pickupImplements(V,Cons,Lc,Env,F,Fx,T,Tx).

mergeFields([],_,Face,Face) :-!.
mergeFields([(Nm,Tp)|R],Lc,Env,SoFar,Face) :- is_member((Nm,STp),SoFar),!,
  sameType(Tp,STp,Lc,Env),
  mergeFields(R,Lc,Env,SoFar,Face).
mergeFields([(Nm,Tp)|R],Lc,Env,SoFar,Face) :- mergeFields(R,Lc,Env,[(Nm,Tp)|SoFar],Face).

sameImplements(implementsFace(T1,F1),implementsFace(T2,F2),Lc,Env) :-
  sameType(T1,T2,Lc,Env),
  sameType(F1,F2,Lc,Env).

simplifyType(T,Lc,Env,C,Cx,Tp) :-
  deRef(T,TT),!,
  smpTp(TT,Lc,Env,C,Cx,Tp).

smpTp(anonType,_,_,C,C,anonType).
smpTp(voidType,_,_,C,C,voidType).
smpTp(type(Nm),_,_,C,C,type(Nm)).
smpTp(tpExp(O,A),Lc,Env,C,Cx,Tp) :-
  isTypeFun(O,Args,Lc,Env,OO),!,
  applyTypeFun(OO,[A|Args],Lc,Env,C,C0,TT),
  simplifyType(TT,Lc,Env,C0,Cx,Tp).
smpTp(tpExp(O,A),Lc,Env,C,Cx,tpExp(OO,As)) :-
  simplifyType(O,Lc,Env,C,C0,OO),
  simplifyType(A,Lc,Env,C0,Cx,As).
smpTp(kVar(V),_,_,C,C,kVar(V)).
smpTp(kFun(V,Ar),_,_,C,C,kFun(V,Ar)).
smpTp(V,_,_,Cx,Cx,V) :- isUnbound(V),!.
smpTp(tpFun(Id,Ar),_,_,Cx,Cx,tpFun(Id,Ar)).
smpTp(valType(T),Lc,Env,C,Cx,valType(Tp)) :-
  simplifyType(T,Lc,Env,C,Cx,Tp).
smpTp(tplType(A),Lc,Env,C,Cx,tplType(As)) :-
  smpTps(A,Lc,Env,C,Cx,As).
smpTp(funType(L,R),Lc,Env,C,Cx,funType(Ls,Rs)) :-
  simplifyType(L,Lc,Env,C,C0,Ls),
  simplifyType(R,Lc,Env,C0,Cx,Rs).
smpTp(consType(L,R),Lc,Env,C,Cx,consType(Ls,Rs)) :-
  simplifyType(L,Lc,Env,C,C0,Ls),
  simplifyType(R,Lc,Env,C0,Cx,Rs).
smpTp(continType(L,R),Lc,Env,C,Cx,continType(Ls,Rs)) :-
  simplifyType(L,Lc,Env,C,C0,Ls),
  simplifyType(R,Lc,Env,C0,Cx,Rs).
smpTp(allType(V,typeLambda(V,tpExp(Op,V))),_,_,C,C,Op).
smpTp(typeLambda(V,tpExp(Op,V)),_,_,C,C,Op).
smpTp(allType(V,T),Lc,Env,C,C,allType(V,Tp)) :-
  simplifyType(T,Lc,Env,Cx,[],In),
  wrapConstraints(Cx,In,Tp).
smpTp(existType(V,T),Lc,Env,Cx,Cx,existType(V,Tp)) :-
  simplifyType(T,Lc,Env,C,[],ITp),
  wrapConstraints(C,ITp,Tp).
smpTp(faceType(F,T),Lc,Env,C,Cx,faceType(Fs,Ts)) :-
  smpFldTps(F,Lc,Env,C,C0,Fs),
  smpFldTps(T,Lc,Env,C0,Cx,Ts).
smpTp(typeLambda(L,R),Lc,Env,C,Cx,typeLambda(L,Rs)) :-
  simplifyType(R,Lc,Env,C,Cx,Rs).
smpTp(constrained(T,Cn),Lc,Env,[Cs|C],Cx,Tp) :-
  smpCon(Cn,Lc,Env,C,C0,Cs),
  simplifyType(T,Lc,Env,C0,Cx,Tp).

smpTps([],_,_,Cx,Cx,[]).
smpTps([T|Tps],Lc,Env,C,Cx,[TT|TTps]) :-
  simplifyType(T,Lc,Env,C,C0,TT),
  smpTps(Tps,Lc,Env,C0,Cx,TTps).

smpFldTps([],_,_,C,C,[]).
smpFldTps([(F,T)|Flds],Lc,Env,C,Cx,[(F,Tp)|Fs]) :-
  simplifyType(T,Lc,Env,C,C0,Tp),
  smpFldTps(Flds,Lc,Env,C0,Cx,Fs).

wrapConstraints([],Tp,Tp).
wrapConstraints([Con|C],Tp,WTp) :-
  wrapConstraints(C,constrained(Tp,Con),WTp).

smpCon(conTract(Nm,L,R),Lc,Env,C,Cx,conTract(Nm,Ls,Rs)) :-
  smpTps(L,Lc,Env,C,C0,Ls),
  smpTps(R,Lc,Env,C0,Cx,Rs).
smpCon(implementsFace(L,R),Lc,Env,C,Cx,implementsFace(Ls,Rs)) :-
  simplifyType(L,Lc,Env,C,C0,Ls),
  simplifyType(R,Lc,Env,C0,Cx,Rs).

bind(tVar(Curr,Con,VLc,Nm,Id),Tp,Lc) :- !,
  \+varIsIn(tVar(Curr,Con,_,Nm,Id),Tp),
  Curr=Tp,
  VLc=Lc.
bind(tFun(Curr,Con,VLc,Nm,Ar,Id),Tp,Lc) :-
  \+varIsIn(tFun(Curr,Con,_,Nm,Ar,Id),Tp),
  Curr=Tp,
  VLc=Lc.

mergeConstraints(Cx,Cy,_Env) :- var(Cx),!, Cx=Cy.
mergeConstraints([Cx|Xs],Y,Lc,Env) :- mergeConstraint(Cx,Y,Lc,Env), mergeConstraints(Xs,Y,Lc,Env).

mergeConstraint(C,Y,_Env) :- var(Y),!,Y=[C|_].
mergeConstraint(conTract(Nm,X,XDps),[conTract(Nm,Y,YDps)|_],Lc,Env) :-
  idList(X,Y,Lc,Env), % only identical types merge
  smList(XDps,YDps,Lc,Env),!.
mergeConstraint(Cx,[_|Y],Lc,Env) :- !, % TODO: handle merging implementsFace more gracefully
  mergeConstraint(Cx,Y,Lc,Env).

varIsIn(TV,Tp) :- deRef(Tp,DTp),
  \+ isIdenticalVar(TV,DTp),
  (TV = tVar(_,_,_,_,Id) -> occIn(Id,DTp); TV=tFun(_,_,_,_,_,Id), occIn(Id,DTp)),!.

occIn(Id,tVar(_,_,_,_,Id)) :-!.
occIn(Id,tVar(Curr,_,_,_,_)) :- nonvar(Curr), !, occIn(Id,Curr).
occIn(Id,tFun(_,_,_,_,_,Id)) :-!.
occIn(Id,tFun(Curr,_,_,_,_,_)) :- nonvar(Curr), !, occIn(Id,Curr).
occIn(Id,tpExp(O,_)) :- occIn(Id,O),!.
occIn(Id,tpExp(_,A)) :- occIn(Id,A),!.
occIn(Id,valType(I)) :- occIn(Id,I).
occIn(Id,tplType(L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(A,_)) :- occIn(Id,A).
occIn(Id,funType(_,R)) :- occIn(Id,R).
occIn(Id,consType(L,_)) :- occIn(Id,L).
occIn(Id,consType(_,R)) :- occIn(Id,R).
occIn(Id,continType(L,_)) :- occIn(Id,L).
occIn(Id,continType(_,R)) :- occIn(Id,R).
occIn(Id,constrained(Tp,Con)) :- occIn(Id,Con) ; occIn(Id,Tp).
occIn(Id,typeLambda(A,_)) :- occIn(Id,A).
occIn(Id,typeLambda(_,R)) :- occIn(Id,R).
occIn(Id,existType(V,Tp)) :- V\=kVar(Id),occIn(Id,Tp).
occIn(Id,allType(V,Tp)) :- V\=kVar(Id),occIn(Id,Tp).
occIn(Id,faceType(L,_)) :- is_member((_,A),L), occIn(Id,A),!.
occIn(Id,faceType(_,T)) :- is_member((_,A),T), occIn(Id,A),!.

applyTypeFun(Fn,Args,Lc,Env,Tp) :-
  applyTypeFun(Fn,Args,Lc,Env,[],[],Tp).

applyTypeFun(Lam,Args,Lc,Env,C,Cx,Tp) :-
  freshen(Lam,Env,_,Lm),
  applyTypeFn(Lm,Args,Lc,Env,C,Cx,Tp).

applyTypeFn(kFun(T,Ar),Args,_,_,Cx,Cx,Tp) :-
  length(Args,Ar),!,
  mkTypeExp(kFun(T,Ar),Args,Tp).
applyTypeFn(tFun(T,C,Lc,N,Ar,Id),Args,_,_,Cx,Cx,Tp) :-
  length(Args,AAr),AAr=<Ar,!,
  mkTypeExp(tFun(T,C,Lc,N,Ar,Id),Args,Tp).
applyTypeFn(tpFun(T,Ar),Args,_,_,Cx,Cx,Tp) :-
  length(Args,AAr),AAr=<Ar,!,
  mkTypeExp(tpFun(T,Ar),Args,Tp).
applyTypeFn(constrained(Tp,Ct),ArgTps,Lc,Env,C,Cx,ATp) :-
  applyTypeFn(Tp,ArgTps,Lc,Env,[Ct|C],Cx,ATp),!.
applyTypeFn(typeLambda(L,Tp),[A|ArgTps],Lc,Env,C,Cx,RTp) :-
  sameType(L,A,Lc,Env),!,
  applyTypeFn(Tp,ArgTps,Lc,Env,C,Cx,RTp).
applyTypeFn(Tp,[],_,_,C,C,Tp).
