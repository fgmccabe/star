:- module(types,
	  [isType/1,isConType/2,isFaceType/1,isConstraint/1,varConstraints/3,addConstraint/2,
	   toLtipe/2,mkTplTipe/2,
	   netEnumType/2,
	   newTypeVar/2,skolemVar/2,newTypeFun/3,skolemFun/3,deRef/2,mkTpExp/3,
	   progTypeArity/2,progArgTypes/2,isTypeLam/1,isTypeLam/2,isTypeExp/3,mkTypeExp/3,typeArity/2,
	   isFunctionType/1,isFunctionType/2,isCnsType/2,
	   isProgramType/1,isFixedSizeType/1,
	   dispType/1,dispType/2,showType/4,showConstraint/3,
	   contractType/2,contractTypes/2,
	   isUnbound/1,isBound/1,isUnboundFVar/2, isIdenticalVar/2,
	   moveQuants/3,reQuantTps/3,
	   moveConstraints/3,moveConstraints/4, implementationName/2,
	   stdType/3]).
:- use_module(misc).

isType(anonType).
isType(voidType).
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
isType(valType(_)).

isConstraint(conTract(_,_,_)).
isConstraint(implementsFace(_,_)).

isConType(Tp,A) :-
  deRef(Tp,T),!,isCnType(T,A).

isCnType(consType(H,_),A) :- tpArity(H,A).
isCnType(allType(_,T),A) :- isConType(T,A).
isCnType(existType(_,T),A) :- isConType(T,A).
isCnType(constrained(T,_),A) :- isConType(T,A).

netEnumType(T,T1) :-
  deRef(T,Td),
  ntEnumType(Td,T1).
ntEnumType(allType(V,T),allType(V,T1)) :-
  netEnumType(T,T1).
ntEnumType(existType(V,T),existType(V,T1)):-
  netEnumType(T,T1).
ntEnumType(constrained(T,C),constrained(T1,C)) :-
  netEnumType(T,T1).
ntEnumType(consType(_,T),T).

isFaceType(Tp) :- deRef(Tp,T),!,isFcType(T).

isFcType(faceType(_,_)).
isFcType(allType(_,T)) :- isFaceType(T).
isFcType(existType(_,T)) :- isFaceType(T).
isFcType(constrained(T,_)) :- isFaceType(T).

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

showType(anonType,_,O,Ox) :- appStr("_",O,Ox).
showType(voidType,_,O,Ox) :- appStr("void",O,Ox).
showType(kVar(Nm),_,O,Ox) :- appStr(Nm,O,Ox).
showType(kFun(Nm,Ar),_,O,Ox) :- appStr(Nm,O,O1),appStr("/",O1,O2),appInt(Ar,O2,Ox).
showType(tVar(Curr,_,_,_),ShCon,O,Ox) :- nonvar(Curr),!,showType(Curr,ShCon,O,Ox).
showType(tVar(_,Cons,Nm,Id),ShCon,O,Ox) :- (ShCon=true -> showVarConstraints(Cons,"",O,O0); O=O0), appStr("%",O0,O1),appStr(Nm,O1,O2),appSym(Id,O2,Ox).
showType(tFun(Curr,_,_,_,_),ShCon,O,Ox) :- nonvar(Curr),!,showType(Curr,ShCon,O,Ox).
showType(tFun(_,_,_Nm,Ar,Id),_,O,Ox) :- appStr("%",O,O1),appStr(Id,O1,O2),appStr("/",O2,O3),appInt(Ar,O3,Ox).
showType(type(Nm),_,O,Ox) :- appStr(Nm,O,Ox).
showType(tpFun(Nm,Ar),_,O,Ox) :- appStr(Nm,O,O1),appStr("%",O1,O2),appInt(Ar,O2,Ox).
showType(tpExp(Nm,A),ShCon,O,Ox) :- showTypeExp(tpExp(Nm,A),ShCon,O,Ox).
showType(tupleType(A),ShCon,O,Ox) :- appStr("(",O,O1), showTypeEls(A,ShCon,O1,O2), appStr(")",O2,Ox).
showType(funType(A,R),ShCon,O,Ox) :- showType(A,ShCon,O,O1), appStr("=>",O1,O2), showType(R,ShCon,O2,Ox).
showType(consType(A,R),ShCon,O,Ox) :- showType(A,ShCon,O,O1), appStr("<=>",O1,O2), showType(R,ShCon,O2,Ox).
showType(refType(R),ShCon,O,Ox) :- appStr("ref ",O,O4), showType(R,ShCon,O4,Ox).
showType(valType(R),ShCon,O,Ox) :- appStr("val ",O,O4), showType(R,ShCon,O4,Ox).
showType(allType(V,Tp),ShCon,O,Ox) :- appStr("all ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,ShCon,O2,Ox).
showType(existType(V,Tp),ShCon,O,Ox) :- appStr("exist ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,ShCon,O2,Ox).
showType(faceType(Els,Tps),ShCon,O,Ox) :- appStr("{ ",O,O1),
    showFieldTypes(Els,"",Sp,ShCon,O1,O2),
    showTypeFields(Tps,Sp,_,ShCon,O2,O3),
    appStr("}",O3,Ox).
showType(typeExists(Hd,Bd),ShCon,O,Ox) :- showType(Hd,ShCon,O,O1), appStr("<~",O1,O2),showType(Bd,ShCon,O2,Ox).
showType(typeLambda(Hd,Bd),ShCon,O,Ox) :- showType(Hd,ShCon,O,O1), appStr("~>",O1,O2),showType(Bd,ShCon,O2,Ox).
showType(contractExists(Spc,Fc),ShCon,O,Ox) :- showConstraint(Spc,O,O1), appStr("<~",O1,O2), showType(Fc,ShCon,O2,Ox).
showType(constrained(Tp,Con),_,O,Ox) :- showConstraint(Con,O,O1), showMoreConstraints(Tp,O1,Ox).

showTypeExp(T,ShCon,O,Ox) :-
  deRef(T,Tp),
  showTpExp(Tp,_,ShCon,0,O,O1),
  appStr("]",O1,Ox).

showTpExp(tpExp(T,A),",",ShCon,Ar,O,Ox) :-!,
  deRef(T,Op),
  Ar2 is Ar+1,
  showTpExp(Op,Sep,ShCon,Ar2,O,O1),
  appStr(Sep,O1,O2),
  showType(A,ShCon,O2,Ox).
showTpExp(tpFun(Op,Ar),"[",_ShCon,Ar,O,Ox) :-!,
  appStr(Op,O,Ox).
showTpExp(Tp,"[",ShCon,_,O,Ox) :-
  showType(Tp,ShCon,O,Ox).

showMoreConstraints(constrained(Tp,Con),O,Ox) :-
  appStr(",",O,O1), showConstraint(Con,O1,O2), showMoreConstraints(Tp,O2,Ox).
showMoreConstraints(Tp,O,Ox) :- appStr("|:",O,O1),showType(Tp,false,O1,Ox).

showConstraint(conTract(Nm,Els,[]),O,Ox) :-!,
  appStr(Nm,O,O1), appStr("[",O1,O2),showTypeEls(Els,false,O2,O3),appStr("]",O3,Ox).
showConstraint(conTract(Nm,Els,Deps),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("[",O1,O2),
  showTypeEls(Els,false,O2,O3),
  appStr("->>",O3,O4),
  showTypeEls(Deps,false,O4,O5),
  appStr("]",O5,Ox).
showConstraint(implementsFace(Tp,Face),O,Ox) :-
  showType(Tp,false,O,O1),
  appStr("<~",O1,O2),
  showType(Face,false,O2,Ox).
showConstraint(constrained(Con,Extra),O,Ox) :-
  showConstraint(Extra,O,O1),
  appStr("|:",O1,O2),
  showConstraint(Con,O2,Ox).

showVarConstraints(C,Tl,O,Ox) :- var(C),!,(Tl=","->appStr("|:",O,Ox);O=Ox).
showVarConstraints([C|Cx],Ld,O,Ox) :-
  appStr(Ld,O,O1),
  showConstraint(C,O1,O2),
  showVarConstraints(Cx,",",O2,Ox).

showBound(Nm,O,Ox) :- showType(Nm,false,O,Ox).

showTypeEls([],_,O,O).
showTypeEls([Tp|More],ShCon,O,E) :- showType(Tp,ShCon,O,O1), showMoreTypeEls(More,ShCon,O1,E).

showMoreTypeEls([],_,O,O).
showMoreTypeEls([Tp|More],ShCon,O,E) :- appStr(", ",O,O1),showType(Tp,ShCon,O1,O2), showMoreTypeEls(More,ShCon,O2,E).

showMoreQuantified(allType(Nm,Tp),ShCon,O,E) :- appStr(", ",O,O1), showType(Nm,ShCon,O1,O2), showMoreQuantified(Tp,ShCon,O2,E).
showMoreQuantified(Tp,ShCon,O,E) :- appStr(" ~~ ",O,O1), showType(Tp,ShCon,O1,E).

showFieldTypes([],Sep,Sep,_,O,O).
showFieldTypes([F|More],Sep,Spx,ShCon,O,E) :- appStr(Sep,O,O0),showField(F,ShCon,O0,O1), showFieldTypes(More,". ",Spx,ShCon,O1,E).

showField((Nm,Tp),ShCon,O,E) :- appStr(Nm,O,O1), appStr(" : ",O1,O2), showType(Tp,ShCon,O2,E).

showTypeFields([],Sp,Sp,_,O,O).
showTypeFields([F|More],Sep,Spx,ShCon,O,E) :-
  appStr(Sep,O,O0),showTypeField(F,ShCon,O0,O1), showTypeFields(More,". ",Spx,ShCon,O1,E).

showTypeField((Nm,Tp),ShCon,O,E) :- appStr(Nm,O,O1), appStr(" ~> ",O1,O2), showType(Tp,ShCon,O2,E).

dispType(Tp) :-
  showType(Tp,true,Chrs,[]),
  string_chars(Text,Chrs),
  writeln(Text).

dispType(Msg,Tp) :-
  appStr(Msg,Chrs,C0),
  showType(Tp,true,C0,[]),
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

tpArity(allType(_,Tp),Ar) :- !, progTypeArity(Tp,Ar).
tpArity(existType(_,Tp),Ar) :- !, progTypeArity(Tp,Ar).
tpArity(constrained(Tp,conTract(_,_,_)),Ar) :- !,
  progTypeArity(Tp,A), Ar is A+1.
tpArity(constrained(Tp,_),Ar) :- !,progTypeArity(Tp,Ar).
tpArity(funType(A,_),Ar) :- !,
  progTypeArity(A,Ar).
tpArity(consType(A,_),Ar) :- !,
  tpArity(A,Ar).
tpArity(refType(A),Ar) :- !,
  progTypeArity(A,Ar).
tpArity(tupleType(A),Ar) :- !,length(A,Ar).
tpArity(faceType(A,_),Ar) :- !,length(A,Ar).
tpArity(_,0).

progArgTypes(Tp,ArTps) :- deRef(Tp,TT), tpArgTypes(TT,ArTps).

tpArgTypes(allType(_,Tp),ArTps) :- tpArgTypes(Tp,ArTps).
tpArgTypes(existType(_,Tp),ArTps) :- tpArgTypes(Tp,ArTps).
tpArgTypes(constrained(_,Tp),ArTps) :- tpArgTypes(Tp,ArTps).
tpArgTypes(funType(A,_),ArTps) :- tpArgTypes(A,ArTps).
tpArgTypes(tupleType(ArTps),ArTps).

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
mkTypeExp(Op,[A|Args],Tp) :-
  mkTypeExp(tpExp(Op,A),Args,Tp).

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
surfaceName(tVar(_,_,Nm,_),Nm).
surfaceName(tFun(_,_,Nm,_,_),Nm).
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

stdType("integer",type("star.core*integer"),typeExists(type("star.core*integer"),faceType([],[]))).
stdType("float",type("star.core*float"),typeExists(type("star.core*float"),faceType([],[]))).
stdType("boolean",type("star.core*boolean"),typeExists(type("star.core*boolean"),faceType([],[]))).
stdType("string",type("star.core*string"),typeExists(type("star.core*string"),faceType([],[]))).
stdType("package",type("star.pkg*pkg"),typeExists(type("star.pkg*pkg"),faceType([],[]))).
stdType("version",type("star.pkg*version"),typeExists(type("star.pkg*version"),faceType([],[]))).

isFixedSizeType(Tp) :- deRef(Tp,T),!,isFxTp(T).

isFxTp(type("star.core*integer")).
isFxTp(type("star.core*float")).
isFxTp(type("star.core*boolean")).
isFxTp(tupleType(Els)) :- check_implies(misc:is_member(T,Els),types:isFixedSizeType(T)).
isFxTp(faceType(Flds,_)) :- check_implies(misc:is_member((_,T),Flds),types:isFixedSizeType(T)).

toLtipe(Tp,LTp) :-
  deRef(Tp,DTp),
  toLtp(DTp,LTp).

toLtp(type("star.core*integer"),i64Tipe) :- !.
toLtp(type("star.core*float"),f64Tipe) :- !.
toLtp(type("star.core*boolean"),blTipe) :- !.
toLtp(funType(Args,Res),fnTipe(As,R)) :-
  map(Args,types:toLtipe,As),
  toLtipe(Res,R).
toLtp(tupleType(Args),tplTipe(As)) :-
  map(Args,types:toLtipe,As).
toLtp(_,ptrTipe).

mkTplTipe(Cnt,tplTipe(As)) :-
  mkPtrs(Cnt,As),!.

mkPtrs(0,[]).
mkPtrs(I,[ptrTipe|As]) :-
  I1 is I-1,
  mkPtrs(I1,As).

