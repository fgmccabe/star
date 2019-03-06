:- module(unify,[sameType/3,idenType/3,faceOfType/3,sameContract/3,
    simplifyType/5,checkConstraint/2]).

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
sm(V1,V2,Env) :- isUnbound(V1), isUnbound(V2),!, varBinding(V1,V2,Env).
sm(V1,T2,Env) :- isUnbound(V1), !, varBinding(V1,T2,Env).
sm(T1,V2,Env) :- isUnbound(V2), !, varBinding(V2,T1,Env).
sm(type(Nm),type(Nm),_).
sm(tpFun(Nm,Ar),tpFun(Nm,Ar),_).
sm(tpExp(O1,A1),T2,Env) :-
  isTypeFun(O1,Args,Env,OO),!,
  applyTypeFn(OO,[A1|Args],Env,[],_,T1),
  sameType(T1,T2,Env).
sm(T1,tpExp(O2,A2),Env) :-
  isTypeFun(O2,Args,Env,OO),!,
  applyTypeFn(OO,[A2|Args],Env,[],_,T2),
  sameType(T1,T2,Env).
sm(tpExp(O1,A1),tpExp(O2,A2),Env) :- sameType(O1,O2,Env), sameType(A1,A2,Env).
sm(refType(A1),refType(A2),Env) :- sameType(A1,A2,Env).
sm(tupleType(A1),tupleType(A2),Env) :- smList(A1,A2,Env).
sm(funType(A1,R1),funType(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
sm(typeLambda(A1,R1),typeLambda(A2,R2),Env) :- sameType(R1,R2,Env), sameType(A2,A1,Env).
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
sm(constrained(T1,C1),constrained(T2,C2),Env) :-
  sameType(T1,T2,Env),
  sameConstraint(C1,C2,Env).

varBinding(T1,T2,_) :- isIdenticalVar(T1,T2),!.
varBinding(T1,T2,Env) :-
  bind(T1,T2,Env).

sameLength(L1,L2) :- length(L1,L), length(L2,L).

sameConstraint(C1,C2,Env) :-
  sameContract(C1,C2,Env),!.
sameConstraint(C1,C2,Env) :-
  sameImplements(C1,C2,Env).

sameContract(conTract(Nm,A1,D1),conTract(Nm,A2,D2),Env) :-
  smpTps(A1,Env,[],_,AA1),
  smpTps(A2,Env,[],_,AA2),
  smList(AA1,AA2,Env),
  smpTps(D1,Env,[],_,DD1),
  smpTps(D2,Env,[],_,DD2),
  smList(DD1,DD2,Env).

smList([],[],_).
smList([E1|L1],[E2|L2],Env) :- sameType(E1,E2,Env), smList(L1,L2,Env).

smFields(_,[],_).
smFields(L1,[(F2,E2)|L2],Env) :- is_member((F2,E1),L1), sameType(E1,E2,Env), smFields(L1,L2,Env).

subFace(Tp1,Tp2,Env) :-
  deRef(Tp1,faceType(F1,T1)),
  deRef(Tp2,faceType(F2,T2)),
  forall(is_member((Nm,Tp1),F1),(is_member((Nm,Tp2),F2),sameType(Tp1,Tp2,Env))),
  forall(is_member((Nm,Tp1),T1),(is_member((Nm,Tp2),T2),sameType(Tp1,Tp2,Env))).

isTypeFun(type(Nm),[],Env,Tp) :-
  isType(Nm,Env,tpDef(_,_,Rule)),
  isTypeLam(Rule),!,
  freshen(Rule,Env,_,Tp).
isTypeFun(tpExp(Nm,A),[A|Args],Env,Tp) :-!,
  isTypeFun(Nm,Args,Env,Tp).
isTypeFun(tpFun(Nm,_),[],Env,Tp) :-
  isType(Nm,Env,tpDef(_,_,Rule)),!,
  isTypeLam(Rule),!,
  freshen(Rule,Env,_,Tp).

faceOfType(T,Env,Face) :-
  simplifyType(T,Env,_,[],Tp),
  getFace(Tp,Env,Face).

getFace(type(Nm),Env,Face) :- !,
  isType(Nm,Env,tpDef(_,_,FaceRule)),
  freshen(FaceRule,Env,_,typeExists(Lhs,FTp)),
  sameType(type(Nm),Lhs,Env),!,
  getFace(FTp,Env,Face).
getFace(tpExp(Op,Arg),Env,Face) :-
  isTypeExp(tpExp(Op,Arg),tpFun(Nm,_),_),!,
  isType(Nm,Env,tpDef(_,_,FaceRule)),
  freshen(FaceRule,Env,_,Rl),
  moveConstraints(Rl,_,typeExists(Lhs,FTp)),
  sameType(Lhs,tpExp(Op,Arg),Env),!,
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
getFace(tupleType(_),_,faceType([],[])).
getFace(refType(_),_,faceType([],[])).

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

sameImplements(implementsFace(T1,F1),implementsFace(T2,F2),Env) :-
  sameType(T1,T2,Env),
  sameType(F1,F2,Env).

idenType(T1,T2,Env) :- deRef(T1,Tp1), deRef(T2,Tp2), id(Tp1,Tp2,Env), !.

id(_,anonType,_).
id(anonType,_,_).
id(voidType,voidType,_).
id(thisType,thisType,_) :-!.
id(thisType,T2,Env) :- isVar("this",Env,vrEntry(_,_,T1,_)),!,
  idenType(T1,T2,Env).
id(T1,thisType,Env) :- isVar("this",Env,vrEntry(_,_,T2,_)),!,
  idenType(T1,T2,Env).
id(kVar(Nm),kVar(Nm),_).
id(kFun(Nm,Ar),kFun(Nm,Ar),_).
id(V1,V2,_) :- isUnbound(V1), isUnbound(V2), isIdenticalVar(V1,V2).
id(type(Nm),type(Nm),_).
id(tpFun(Nm,Ar),tpFun(Nm,Ar),_).
id(tpExp(O1,A1),T2,Env) :-
  isTypeFun(O1,Args,Env,OO),!,
  applyTypeFn(OO,[A1|Args],Env,[],_,T1),
  idenType(T1,T2,Env).
id(T1,tpExp(O2,A2),Env) :-
  isTypeFun(O2,Args,Env,OO),!,
  applyTypeFn(OO,[A2|Args],Env,[],_,T2),
  idenType(T1,T2,Env).
id(tpExp(O1,A1),tpExp(O2,A2),Env) :- idenType(O1,O2,Env),idenType(A1,A2,Env).
id(refType(A1),refType(A2),Env) :- idenType(A1,A2,Env).
id(tupleType(A1),tupleType(A2),Env) :- idList(A1,A2,Env).
id(funType(A1,R1),funType(A2,R2),Env) :- idenType(R1,R2,Env), idenType(A2,A1,Env).
id(typeLambda(A1,R1),typeLambda(A2,R2),Env) :- idenType(R1,R2,Env), idenType(A2,A1,Env).
id(consType(A1,R1),consType(A2,R2),Env) :- idenType(R1,R2,Env), idenType(A1,A2,Env).
id(faceType(E1,T1),faceType(E2,T2),Env) :- sameLength(E1,E2),
    sameLength(T1,T2),
    idFields(E1,E2,Env),
    idFields(T1,T2,Env).
id(existType(K,T1),existType(K,T2),Env) :-
  idenType(T1,T2,Env).
id(existType(kFun(K1,Ar),T1),existType(kFun(K2,Ar),T2),Env) :-
  rewriteType(T2,Env,[(K2,kFun(K1,Ar))],[],TT2),
  idenType(T1,TT2,Env).
id(allType(K,T1),allType(K,T2),Env) :-!,
  idenType(T1,T2,Env).
id(allType(kVar(K1),T1),allType(kVar(K2),T2),Env) :-
  rewriteType(T2,Env,[(K2,kVar(K1))],[],TT2),
  idenType(T1,TT2,Env).

idList([],[],_).
idList([E1|L1],[E2|L2],Env) :- idenType(E1,E2,Env), idList(L1,L2,Env).

idFields(_,[],_).
idFields(L1,[(F2,E2)|L2],Env) :- is_member((F2,E1),L1), idenType(E1,E2,Env), idFields(L1,L2,Env).

simplifyType(T,Env,C,Cx,Tp) :-
  deRef(T,TT),!,
  smpTp(TT,Env,C,Cx,Tp).

smpTp(anonType,_,C,C,anonType).
smpTp(voidType,_,C,C,voidType).
smpTp(thisType,_,C,C,thisType).
smpTp(type(Nm),_,C,C,type(Nm)).
smpTp(tpExp(O,A),Env,C,Cx,Tp) :-
  isTypeFun(O,Args,Env,OO),!,
  applyTypeFn(OO,[A|Args],Env,C,C0,TT),
  simplifyType(TT,Env,C0,Cx,Tp).
smpTp(tpExp(O,A),Env,C,Cx,tpExp(OO,As)) :-
  simplifyType(O,Env,C,C0,OO),
  simplifyType(A,Env,C0,Cx,As).
smpTp(kVar(V),_,C,C,kVar(V)).
smpTp(kFun(V,Ar),_,C,C,kFun(V,Ar)).
smpTp(V,_,Cx,Cx,V) :- isUnbound(V),!.
smpTp(tpFun(Id,Ar),_,Cx,Cx,tpFun(Id,Ar)).
smpTp(refType(T),Env,C,Cx,refType(Tp)) :-
  simplifyType(T,Env,C,Cx,Tp).
smpTp(tupleType(A),Env,C,Cx,tupleType(As)) :-
  smpTps(A,Env,C,Cx,As).
smpTp(funType(L,R),Env,C,Cx,funType(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  simplifyType(R,Env,C0,Cx,Rs).
smpTp(consType(L,R),Env,C,Cx,consType(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  simplifyType(R,Env,C0,Cx,Rs).
smpTp(allType(V,typeLambda(V,tpExp(Op,V))),_,C,C,Op).
smpTp(typeLambda(V,tpExp(Op,V)),_,C,C,Op).
smpTp(allType(V,T),Env,C,C,allType(V,Tp)) :-
  simplifyType(T,Env,Cx,[],In),
  wrapConstraints(Cx,In,Tp).
smpTp(existType(V,T),Env,Cx,Cx,existType(V,Tp)) :-
  simplifyType(T,Env,C,[],ITp),
  wrapConstraints(C,ITp,Tp).
smpTp(faceType(F,T),Env,C,Cx,faceType(Fs,Ts)) :-
  smpFldTps(F,Env,C,C0,Fs),
  smpFldTps(T,Env,C0,Cx,Ts).
smpTp(typeLambda(L,R),Env,C,Cx,typeLambda(L,Rs)) :-
  simplifyType(R,Env,C,Cx,Rs).
smpTp(constrained(T,Cn),Env,[Cs|C],Cx,Tp) :-
  smpCon(Cn,Env,C,C0,Cs),
  simplifyType(T,Env,C0,Cx,Tp).

smpTps([],_,Cx,Cx,[]).
smpTps([T|Tps],Env,C,Cx,[TT|TTps]) :-
  simplifyType(T,Env,C,C0,TT),
  smpTps(Tps,Env,C0,Cx,TTps).

smpFldTps([],_,C,C,[]).
smpFldTps([(F,T)|Flds],Env,C,Cx,[(F,Tp)|Fs]) :-
  simplifyType(T,Env,C,C0,Tp),
  smpFldTps(Flds,Env,C0,Cx,Fs).

applyTypeFn(kFun(T,Ar),Args,_,Cx,Cx,Tp) :-
  length(Args,Ar),!,
  mkTypeExp(kFun(T,Ar),Args,Tp).
applyTypeFn(tFun(T,B,Ar,Id),Args,_,Cx,Cx,Tp) :-
  length(Args,AAr),AAr=<Ar,!,
  mkTypeExp(tFun(T,B,Ar,Id),Args,Tp).
applyTypeFn(tpFun(T,Ar),Args,_,Cx,Cx,Tp) :-
  length(Args,AAr),AAr=<Ar,!,
  mkTypeExp(tpFun(T,Ar),Args,Tp).
applyTypeFn(constrained(Tp,Ct),ArgTps,Env,C,Cx,ATp) :-
  applyTypeFn(Tp,ArgTps,Env,[Ct|C],Cx,ATp),!.
applyTypeFn(typeLambda(L,Tp),[A|ArgTps],Env,C,Cx,RTp) :-
  sameType(L,A,Env),!,
  applyTypeFn(Tp,ArgTps,Env,C,Cx,RTp).
applyTypeFn(Tp,[],_,C,C,Tp).

wrapConstraints([],Tp,Tp).
wrapConstraints([Con|C],Tp,WTp) :-
  wrapConstraints(C,constrained(Tp,Con),WTp).

smpCon(conTract(Nm,L,R),Env,C,Cx,conTract(Nm,Ls,Rs)) :-
  smpTps(L,Env,C,C0,Ls),
  smpTps(R,Env,C0,Cx,Rs).
smpCon(implementsFace(L,R),Env,C,Cx,implementsFace(Ls,Rs)) :-
  simplifyType(L,Env,C,C0,Ls),
  simplifyType(R,Env,C0,Cx,Rs).

bind(tVar(Curr,Con,Nm,Id),Tp,Env) :- !,
  \+occursIn(tVar(Curr,Con,Nm,Id),Tp),
  Curr=Tp,
  (varConstraints(Tp,Env,Cx) ->
    mergeConstraints(Con,Cx,Env) ;
    checkConstraints(Con,Env)).
bind(tFun(Curr,Con,Nm,Ar,Id),Tp,Env) :-
  \+occursIn(tFun(Curr,Con,Nm,Ar,Id),Tp),
  Curr=Tp,
  (varConstraints(Tp,Env,Cx) ->
    mergeConstraints(Con,Cx,Env) ;
    checkConstraints(Con,Env)).

mergeConstraints(Cx,Cy,_Env) :- var(Cx),!, Cx=Cy.
mergeConstraints([Cx|Xs],Y,Env) :- mergeConstraint(Cx,Y,Env), mergeConstraints(Xs,Y,Env).

mergeConstraint(C,Y,_Env) :- var(Y),!,Y=[C|_].
mergeConstraint(conTract(Nm,X,XDps),[conTract(Nm,Y,YDps)|_],Env) :-!,
  sameContract(conTract(Nm,X,XDps),conTract(Nm,Y,YDps),Env).
mergeConstraint(Cx,[_|Y],Env) :- !, % TODO: handle merging implementsFace more gracefully
  mergeConstraint(Cx,Y,Env).

checkConstraints(Cx,_Env) :- var(Cx),!.
checkConstraints([C|Cx],Env) :- checkConstraint(C,Env), checkConstraints(Cx,Env).

checkConstraint(conTract(Nm,Args,Deps),Env) :-
  (implementationName(conTract(Nm,Args,Deps),ImplNm) ->
    (getImplementation(Nm,ImplNm,Env,Impl) ->
      freshen(Impl,Env,_,ImplCon),
      getConstrainedContract(ImplCon,Con),
      sameContract(Con,conTract(Nm,Args,Deps),Env);
      true);
    true).

checkConstraint(implementsFace(Tp,NdFace),Env) :-
  faceOfType(Tp,Env,Face),
  subFace(NdFace,Face,Env).

getConstrainedContract(constrained(C,_),Con) :- getConstrainedContract(C,Con).
getConstrainedContract(contractExists(Con,_),Con).

occursIn(TV,Tp) :- deRef(Tp,DTp),
  \+ isIdenticalVar(TV,DTp),
  (TV = tVar(_,_,_,Id) -> occIn(Id,DTp); TV=tFun(_,_,_,_,Id), occIn(Id,DTp)),!.

occIn(Id,tVar(_,_,_,Id)) :-!.
occIn(Id,tVar(Curr,_,_,_)) :- nonvar(Curr), !, occIn(Id,Curr).
occIn(Id,tFun(_,_,_,_,Id)) :-!.
occIn(Id,tFun(Curr,_,_,_,_)) :- nonvar(Curr), !, occIn(Id,Curr).
occIn(Id,tpExp(O,_)) :- occIn(Id,O),!.
occIn(Id,tpExp(_,A)) :- occIn(Id,A),!.
occIn(Id,refType(I)) :- occIn(Id,I).
occIn(Id,tupleType(L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(A,_)) :- occIn(Id,A).
occIn(Id,funType(_,R)) :- occIn(Id,R).
occIn(Id,consType(L,_)) :- occIn(Id,L).
occIn(Id,consType(_,R)) :- occIn(Id,R).
occIn(Id,constrained(Tp,Con)) :- occIn(Id,Con) ; occIn(Id,Tp).
occIn(Id,typeLambda(A,_)) :- occIn(Id,A).
occIn(Id,typeLambda(_,R)) :- occIn(Id,R).
occIn(Id,existType(V,Tp)) :- V\=kVar(Id),occIn(Id,Tp).
occIn(Id,allType(V,Tp)) :- V\=kVar(Id),occIn(Id,Tp).
occIn(Id,faceType(L,_)) :- is_member((_,A),L), occIn(Id,A),!.
occIn(Id,faceType(_,T)) :- is_member((_,A),T), occIn(Id,A),!.
