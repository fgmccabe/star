:- module(types,[isType/1,isConType/1,isConstraint/1,varConstraints/3,addConstraint/2,
      newTypeVar/2,skolemVar/2,newTypeFun/3,skolemFun/3,deRef/2,mkTpExp/3,
      progTypeArity/2,isTypeLam/1,isTypeLam/2,isTypeExp/3,mkTypeExp/3,typeArity/2,
      isFunctionType/1,isFunctionType/2,isCnsType/2,
      isProgramType/1,
      dispType/1,showType/3,showConstraint/3,contractType/2,contractTypes/2,
      isUnbound/1,isBound/1,isUnboundFVar/2, isIdenticalVar/2,
      moveQuants/3,reQuantTps/3,
      moveConstraints/3,moveConstraints/4, implementationName/2,
      stdType/3]).
:- use_module(misc).

isType(anonType).
isType(voidType).
isType(thisType).
isType(kVar(_)).
isType(tVar(_,_,_,_)).
isType(tFun(_,_,_,_,_)).
isType(type(_)).
isType(tpExp(_,_)).
isType(refType(_)).
isType(tupleType(_)).
isType(funType(_,_)).
isType(consType(_,_)).
isType(allType(_,_)).
isType(existType(_,_)).
isType(faceType(_,_)).
isType(typeLambda(_,_)).
isType(constrained(_,_)).

isConstraint(conTract(_,_,_)).
isConstraint(implementsFace(_,_)).

isConType(consType(_,_)).
isConType(allType(_,T)) :- isConType(T).
isConType(existType(_,T)) :- isConType(T).
isConType(constrained(T,_)) :- isConType(T).

newTypeVar(Nm,tVar(_,_,Nm,Id)) :- gensym("_#",Id).
newTypeFun(Nm,Ar,tFun(_,_,Nm,Ar,Id)) :- gensym(Nm,Id).

varConstraints(tVar(_,Con,_,_),_,Con) :-!.
varConstraints(tFun(_,Con,_,_,_),_,Con) :- !.
varConstraints(kVar(Id),Env,Con) :- !,
  getEnvConstraints(Env,types:isKCon(kVar(Id)),Con,_).
varConstraints(kFun(Id,Ar),Env,Con) :- !,
  getEnvConstraints(Env,types:isKCon(kFun(Id,Ar)),Con,_).

isKCon(K,conTract(_,A,_)) :- is_member(K,A).


getEnvConstraints([],_,Cx,Cx).
getEnvConstraints([scope(_,_,Cns,_,_)|Ev],T,C,Cx) :-
  collectConstraints(Cns,T,C,C0),
  getEnvConstraints(Ev,T,C0,Cx).

collectConstraints([],_,Cx,Cx).
collectConstraints([C|Cs],T,[C|C0],Cx) :-
  call(T,C),!,
  collectConstraints(Cs,T,C0,Cx).
collectConstraints([_|Cs],T,C,Cx) :-
  collectConstraints(Cs,T,C,Cx).

addConstraint(tVar(_,Cx,_,_),Con) :- !, safeAdd(Cx,Con).
addConstraint(tFun(_,Cx,_,_,_),Con) :- safeAdd(Cx,Con).

safeAdd(Cx,Con) :- var(Cx),!,Cx=[Con|_].
safeAdd([_|Cx],Con) :- safeAdd(Cx,Con).

skolemVar(Nm,kVar(Id)) :- genstr(Nm,Id).

skolemFun(Nm,0,kVar(Id)) :- !,genstr(Nm,Id).
skolemFun(Nm,Ar,kFun(Id,Ar)) :- genstr(Nm,Id).

deRef(tVar(Curr,_,_,_),Tp) :- nonvar(Curr), !, deRef(Curr,Tp),!.
deRef(tFun(Curr,_,_,_,_),Tp) :- nonvar(Curr), !, deRef(Curr,Tp),!.
deRef(T,T).

isIdenticalVar(tVar(_,_,_,Id),tVar(_,_,_,Id)).
isIdenticalVar(tFun(_,_,_,Ar,Id),tFun(_,_,_,Ar,Id)).
isIdenticalVar(kVar(Id),kVar(Id)).
isIdenticalVar(kFun(Id,Ar),kFun(Id,Ar)).

isUnbound(T) :- deRef(T,Tp), (Tp=tVar(Curr,_,_,_),!,var(Curr) ; Tp=tFun(Curr,_,_,_,_),!,var(Curr)).

isUnboundFVar(T,Ar) :- deRef(T,tFun(_,_,_,Ar,_)).

isBound(T) :- deRef(T,TV), TV\=tVar(_,_,_,_),TV\=tFun(_,_,_,_,_).

moveQuants(allType(B,Tp),[B|Q],Tmpl) :- !,
  moveQuants(Tp,Q,Tmpl).
moveQuants(Tp,[],Tp).

reQuantTps(Tp,[],Tp).
reQuantTps(Tp,[(_,Vt)|Q],allType(Vtt,QTp)) :-
  deRef(Vt,Vtt),
  (Vtt=kVar(_) ; Vtt=kFun(_,_)),!,
  reQuantTps(Tp,Q,QTp).
reQuantTps(Tp,[(_,Vt)|Q],QTp) :-
  \+ isUnbound(Vt),!,
  reQuantTps(Tp,Q,QTp).
reQuantTps(Tp,[(V,Vr)|Q],allType(kVar(V),QTp)) :-
  isUnbound(Vr),
  reQuantTps(Tp,Q,QTp).

moveConstraints(constrained(Tp,Con),[Con|C],Tmp) :-!,
  moveConstraints(Tp,C,Tmp).
moveConstraints(Tp,[],Tp).

moveConstraints(constrained(Tp,Con),C,Cx,Inner) :-
  moveConstraints(Tp,[Con|C],Cx,Inner).
moveConstraints(Tp,C,C,Tp).

showType(anonType,O,Ox) :- appStr("_",O,Ox).
showType(voidType,O,Ox) :- appStr("void",O,Ox).
showType(thisType,O,Ox) :- appStr("this",O,Ox).
showType(kVar(Nm),O,Ox) :- appStr(Nm,O,Ox).
showType(kFun(Nm,Ar),O,Ox) :- appStr(Nm,O,O1),appStr("/",O1,O2),appInt(Ar,O2,Ox).
showType(tVar(Curr,_,_,_),O,Ox) :- nonvar(Curr),!,showType(Curr,O,Ox).
showType(tVar(_,_Cons,Nm,Id),O,Ox) :- /*showVarConstraints(Cons,O,O0),*/appStr("%",O,O1),appStr(Nm,O1,O2),appSym(Id,O2,Ox).
showType(tFun(Curr,_,_,_,_),O,Ox) :- nonvar(Curr),!,showType(Curr,O,Ox).
showType(tFun(_,_,_Nm,Ar,Id),O,Ox) :- appStr("%",O,O1),appStr(Id,O1,O2),appStr("/",O2,O3),appInt(Ar,O3,Ox).
showType(type(Nm),O,Ox) :- appStr(Nm,O,Ox).
showType(tpFun(Nm,Ar),O,Ox) :- appStr(Nm,O,O1),appStr("%",O1,O2),appInt(Ar,O2,Ox).
showType(tpExp(Nm,A),O,Ox) :- showTypeExp(tpExp(Nm,A),O,Ox).
showType(tupleType(A),O,Ox) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,Ox).
showType(funType(A,R),O,Ox) :- showType(A,O,O1), appStr("=>",O1,O2), showType(R,O2,Ox).
showType(consType(A,R),O,Ox) :- showType(A,O,O1), appStr("<=>",O1,O2), showType(R,O2,Ox).
showType(refType(R),O,Ox) :- appStr("ref",O,O4), showType(R,O4,Ox).
showType(allType(V,Tp),O,Ox) :- appStr("all ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,showType,O2,Ox).
showType(existType(V,Tp),O,Ox) :- appStr("exist ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,showType,O2,Ox).
showType(faceType(Els,Tps),O,Ox) :- appStr("{ ",O,O1),
    showFieldTypes(Els,"",Sp,O1,O2),
    showTypeFields(Tps,Sp,_,O2,O3),
    appStr("}",O3,Ox).
showType(typeExists(Hd,Bd),O,Ox) :- showType(Hd,O,O1), appStr("<~",O1,O2),showType(Bd,O2,Ox).
showType(typeLambda(Hd,Bd),O,Ox) :- showType(Hd,O,O1), appStr("~>",O1,O2),showType(Bd,O2,Ox).
showType(contractExists(Spc,Fc),O,Ox) :- showConstraint(Spc,O,O1), appStr("<~",O1,O2), showType(Fc,O2,Ox).
showType(constrained(Tp,Con),O,Ox) :- showConstraint(Con,O,O1), showMoreConstraints(Tp,O1,Ox).

showTypeExp(tpExp(tpFun("{}",1),Arg),O,Ox) :-!,
  showType(Arg,O,O1),
  appStr("{}",O1,Ox).
showTypeExp(T,O,Ox) :-
  deRef(T,Tp),
  showTpExp(Tp,_,O,O1),
  appStr("]",O1,Ox).

showTpExp(tpExp(T,A),",",O,Ox) :-
  deRef(T,Op),
  showTpExp(Op,Sep,O,O1),
  appStr(Sep,O1,O2),
  showType(A,O2,Ox).
showTpExp(Tp,"[",O,Ox) :-
  showType(Tp,O,Ox).

showMoreConstraints(constrained(Tp,Con),O,Ox) :-
  appStr(",",O,O1), showConstraint(Con,O1,O2), showMoreConstraints(Tp,O2,Ox).
showMoreConstraints(Tp,O,Ox) :- appStr("|:",O,O1),showType(Tp,O1,Ox).

showConstraint(allType(V,B),O,Ox) :-
  appStr("all ",O,O1),
  showBound(V,O1,O2),
  showMoreQuantified(B,showConstraint,O2,Ox).
showConstraint(conTract(Nm,Els,[]),O,Ox) :-!,
  appStr(Nm,O,O1), appStr("[",O1,O2),showTypeEls(Els,O2,O3),appStr("]",O3,Ox).
showConstraint(conTract(Nm,Els,Deps),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("[",O1,O2),
  showTypeEls(Els,O2,O3),
  appStr("->>",O3,O4),
  showTypeEls(Deps,O4,O5),
  appStr("]",O5,Ox).
showConstraint(implementsFace(Tp,Face),O,Ox) :-
  showType(Tp,O,O1),
  appStr("<~",O1,O2),
  showType(Face,O2,Ox).
showConstraint(constrained(Con,Extra),O,Ox) :-
  showConstraint(Extra,O,O1),
  appStr("|:",O1,O2),
  showConstraint(Con,O2,Ox).

showVarConstraints(C,O,O) :- var(C),!.
showVarConstraints([C|Cx],O,Ox) :-
  showConstraint(C,O,O1),
  appStr(",:",O1,O2),
  showVarConstraints(Cx,O2,Ox).

showBound(Nm,O,Ox) :- showType(Nm,O,Ox).

showTypeEls([],O,O).
showTypeEls([Tp|More],O,E) :- showType(Tp,O,O1), showMoreTypeEls(More,O1,E).

showMoreTypeEls([],O,O).
showMoreTypeEls([Tp|More],O,E) :- appStr(", ",O,O1),showType(Tp,O1,O2), showMoreTypeEls(More,O2,E).

showMoreQuantified(allType(Nm,Tp),P,O,E) :- appStr(", ",O,O1), showType(Nm,O1,O2), showMoreQuantified(Tp,P,O2,E).
showMoreQuantified(Tp,P,O,E) :- appStr(" ~~ ",O,O1), call(P,Tp,O1,E).

showFieldTypes([],Sep,Sep,O,O).
showFieldTypes([F|More],Sep,Spx,O,E) :- appStr(Sep,O,O0),showField(F,O0,O1), showFieldTypes(More,". ",Spx,O1,E).

showField((Nm,Tp),O,E) :- appStr(Nm,O,O1), appStr(" : ",O1,O2), showType(Tp,O2,E).

showTypeFields([],Sp,Sp,O,O).
showTypeFields([F|More],Sep,Spx,O,E) :-
  appStr(Sep,O,O0),showTypeField(F,O0,O1), showTypeFields(More,". ",Spx,O1,E).

showTypeField((Nm,Tp),O,E) :- appStr(Nm,O,O1), appStr(" ~> ",O1,O2), showType(Tp,O2,E).

dispType(Tp) :-
  showType(Tp,Chrs,[]),
  string_chars(Text,Chrs),
  writeln(Text).

typeArity(Tp,Ar) :- deRef(Tp,T), tArity(T,Ar).

tArity(type(_),0).
tArity(kFun(_,Ar),Ar).
tArity(kVar(_),0).
tArity(tFun(_,_,_,Ar,_),Ar).
tArity(tpExp(Op,_),Ar) :-
  typeArity(Op,A1),
  Ar is A1-1.

progTypeArity(Tp,Ar) :- deRef(Tp,TTp), tpArity(TTp,Ar).

tpArity(allType(_,Tp),Ar) :- progTypeArity(Tp,Ar).
tpArity(existType(_,Tp),Ar) :- progTypeArity(Tp,Ar).
tpArity(constrained(Tp,_),Ar) :- progTypeArity(Tp,A), Ar is A+1.
tpArity(funType(A,_),Ar) :- progTypeArity(A,Ar).
tpArity(consType(A,_),Ar) :- tpArity(A,Ar).
tpArity(refType(A),Ar) :- progTypeArity(A,Ar).
tpArity(tupleType(A),Ar) :- length(A,Ar).
tpArity(_,0).

isFunctionType(T) :- deRef(T,Tp), isFunctionType(Tp,_).

isFunctionType(allType(_,T),Ar) :- deRef(T,Tp),isFunctionType(Tp,Ar).
isFunctionType(funType(A,_),Ar) :- progTypeArity(A,Ar).

isCnsType(Tp,Ar) :- deRef(Tp,T), isCnsTp(T,Ar).

isCnsTp(allType(_,Tp),Ar) :- isCnsTp(Tp,Ar).
isCnsTp(constrained(Tp,_),Ar) :- isCnsTp(Tp,Ar).

isCnsTp(consType(A,_),Ar) :- progTypeArity(A,Ar).

isProgramType(Tp) :- deRef(Tp,TT), isProgType(TT).

isProgType(allType(_,Tp)) :- !, isProgType(Tp).
isProgType(constrained(Tp,_)) :- isProgType(Tp).
isProgType(Tp) :- isFunctionType(Tp),!.
isProgType(Tp) :- isCnsType(Tp,_),!.

isTypeLam(Tp) :- isTypeLam(Tp,_).

isTypeLam(Tp,Ar) :- deRef(Tp,TT), isTpLam(TT,Ar).

isTpLam(typeLambda(At,_),Ar) :- progTypeArity(At,Ar).
isTpLam(allType(_,T),Ar) :- isTpLam(T,Ar).
isTpLam(existType(_,T),Ar) :- isTpLam(T,Ar).
isTpLam(constrained(T,_),Ar) :- isTpLam(T,Ar).

isTypeExp(Tp,Op,Args) :-
  isTpExp(Tp,Op,Args,[]).

isTpExp(tpExp(O,A),Op,Args,Ax) :-
  isTpExp(O,Op,Args,[A|Ax]).
isTpExp(Op,Op,Args,Args).

mkTypeExp(Op,[],Op).
mkTypeExp(Op,[A|Ar],Tp) :-
  mkTypeExp(tpExp(Op,A),Ar,Tp).

implementationName(conTract(Nm,Args,_),INm) :-
  appStr(Nm,S0,S1),
  marker(over,M),
  surfaceNames(Args,M,S1,[]),
  string_chars(INm,S0).

surfaceNames([],_,S,S).
surfaceNames([T|L],Sep,S0,Sx) :-
  deRef(T,TT),
  surfaceName(TT,SN),
  appStr(Sep,S0,S1),
  appStr(SN,S1,S2),
  surfaceNames(L,Sep,S2,Sx).

surfaceName(type(Nm),Nm).
surfaceName(tpExp(Op,_),Nm) :- deRef(Op,OO), surfaceName(OO,Nm).
surfaceName(kVar(Nm),Nm).
surfaceName(kFun(Nm,_),Nm).
surfaceName(tpFun(Nm,_),Nm).
surfaceName(allType(_,Tp),Nm) :-
  surfaceName(Tp,Nm).
surfaceName(constrained(T,_),Nm) :-
  surfaceName(T,Nm).
surfaceName(typeLambda(_,R),Nm) :-
  surfaceName(R,Nm).
surfaceName(tupleType(Els),Nm) :-
  length(Els,Ar),
  swritef(Nm,"()%d",[Ar]).

contractType(conTract(Nm,A,D),Tp) :-
  concat(A,D,Args),
  length(Args,Ar),
  rfold(Args,types:mkTpExp,tpFun(Nm,Ar),Tp).

mkTpExp(Op,A,tpExp(Op,A)).

contractTypes(CTs,TPs) :-
  map(CTs,types:contractType,TPs).

stdType("int",type("star.core*integer"),typeExists(type("star.core*integer"),faceType([],[]))).
stdType("float",type("star.core*float"),typeExists(type("star.core*float"),faceType([],[]))).
stdType("boolean",type("star.core*boolean"),typeExists(type("star.core*boolean"),faceType([],[]))).
stdType("string",type("star.core*string"),typeExists(type("star.core*string"),faceType([],[]))).
stdType("list",tpFun("star.core*list",1),allType(kVar("e"),typeExists(tpExp(tpFun("star.core*list",1),kVar("e")),faceType([],[])))).
