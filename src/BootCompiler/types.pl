:- module(types,
	  [isType/1,isConType/2,isFaceType/1,isConstraint/1,varConstraints/3,addConstraint/2,
	   toLtipe/2,mkTplTipe/2,tpName/2,consTpName/2,
	   netEnumType/2,
	   newTypeVar/2,skolemVar/2,newTypeFun/3,skolemFun/3,deRef/2,
	   progTypeArity/2,progArgTypes/2,funResType/2,
	   isTypeLam/1,isTypeLam/2,isTypeExp/3,mkTypeExp/3,typeArity/2,
	   isFunctionType/1,isFunctionType/2,isCnsType/3,
	   isProgramType/1,
	   ssConstraint/4,ssType/4,dispType/1,dispConstraint/1,
	   contractType/2,contractTypes/2,
	   isUnbound/1,isBound/1,isUnboundFVar/2, isIdenticalVar/2,occursIn/2,
	   moveQuants/3,reQuantTps/3,
	   moveXQuants/3,reQuantX/3,
	   getConstraints/3,putConstraints/3,
	   implementationName/2,
	   mkTypeRule/3,
	   stdType/3,contType/2,
	   unitTp/1]).
:- use_module(misc).
:- use_module(display).
:- use_module(location).

isType(anonType).
isType(voidType).
isType(kVar(_)).
isType(tVar(_,_,_,_,_)).
isType(tFun(_,_,_,_,_,_)).
isType(type(_)).
isType(tpExp(_,_)).
isType(refType(_)).
isType(tplType(_)).
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

newTypeVar(Nm,tVar(_,_,_,Nm,Id)) :- genstr("_#",Id).
newTypeFun(Nm,Ar,tFun(_,_,_,Nm,Ar,Id)) :- genstr(Nm,Id).

varConstraints(tVar(_,Con,_,_,_),_,Con) :-!.
varConstraints(tFun(_,Con,_,_,_,_),_,Con) :- !.

addConstraint(tVar(_,Cx,_,_,_),Con) :- !, safeAdd(Cx,Con).
addConstraint(tFun(_,Cx,_,_,_,_),Con) :- safeAdd(Cx,Con).

safeAdd(Cx,Con) :- var(Cx),!,Cx=[Con|_].
safeAdd([_|Cx],Con) :- safeAdd(Cx,Con).

skolemVar(Nm,kVar(Id)) :- genstr(Nm,Id).

skolemFun(Nm,0,kVar(Id)) :- !,genstr(Nm,Id).
skolemFun(Nm,Ar,kFun(Id,Ar)) :- genstr(Nm,Id).

deRef(tVar(Curr,_,_,_,_),Tp) :- nonvar(Curr), !, deRef(Curr,Tp),!.
deRef(tFun(Curr,_,_,_,_,_),Tp) :- nonvar(Curr), !, deRef(Curr,Tp),!.
deRef(T,T).

isIdenticalVar(tVar(_,_,_,_,Id),tVar(_,_,_,_,Id)).
isIdenticalVar(tFun(_,_,_,_,Ar,Id),tFun(_,_,_,_,Ar,Id)).
isIdenticalVar(kVar(Id),kVar(Id)).
isIdenticalVar(kFun(Id,Ar),kFun(Id,Ar)).

isUnbound(T) :- deRef(T,Tp),
  (Tp=tVar(Curr,_,_,_,_),!,var(Curr) ;
   Tp=tFun(Curr,_,_,_,_,_),!,var(Curr)).

isUnboundFVar(T,Ar) :- deRef(T,tFun(_,_,_,_,Ar,_)).

isBound(T) :- deRef(T,TV), TV\=tVar(_,_,_,_,_),TV\=tFun(_,_,_,_,_).

moveQuants(Tp,Q,Tmp) :-
  deRef(Tp,DTp), mvQuants(DTp,Q,Tmp).

mvQuants(allType(B,Tp),[B|Q],Tmpl) :- !,
  deRef(Tp,DTp),
  moveQuants(DTp,Q,Tmpl).
mvQuants(Tp,[],Tp).

moveXQuants(existType(B,InTp),[B|Q],Tp) :-!,
  moveXQuants(InTp,Q,Tp).
moveXQuants(Tp,[],Tp).

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

reQuantX([],Tp,Tp).
reQuantX([(_,Vt)|Q],Tp,existType(Vt,QTp)) :-
  reQuantX(Q,Tp,QTp).

getConstraints(Tp,Cx,Inner) :-
  deRef(Tp,DTp),
  mvConstraints(DTp,Cx,[],Inner).

mvConstraints(constrained(Tp,Con),[Con|C],Cx,Tmp) :-
  deRef(Tp,DTp),
  mvConstraints(DTp,C,Cx,Tmp).
mvConstraints(Tp,Cx,Cx,Tp).

putConstraints([],Tp,Tp).
putConstraints([Con|Cx],In,constrained(Tp,Con)) :-
  putConstraints(Cx,In,Tp).

dispType(Tp) :-
  ssType(Tp,false,0,TT),
  displayln(TT).

ssType(anonType,_,_,ss("_")).
ssType(voidType,_,_,ss("void")).
ssType(kVar(Nm),_,_,id(Nm)).
ssType(kFun(Nm,Ar),_,_,sq([id(Nm),ss("/"),ix(Ar)])).
% ssType(tVar(Curr,_,Lc,_,_),true,Dp,sq([S/*,ss("@"),LL*/])) :-
%   nonvar(Curr),!,
%   deRef(Curr,Cr),
%   ssType(Cr,true,Dp,S),
%   lastBindingLoc(Curr,Lc,VLc),
%   ssLc(VLc,_LL).
ssType(tVar(Curr,_,_,_,_),_,Dp,S) :- nonvar(Curr),!,ssType(Curr,false,Dp,S).
ssType(tVar(_,_,_,_,Id),_,_,ss(Id)).
ssType(tVar(_,Cons,_,Id),true,Dp,sq([sq(Cnx),ss("%"),ss(Id)])) :-
  ssVarConstraints(Cons,Dp,Cnx).
ssType(tFun(Curr,_,_,_,_,_),ShCon,Dp,S) :- nonvar(Curr),!,ssType(Curr,ShCon,Dp,S).
ssType(tFun(_,Cons,_,_,Ar,Id),true,Dp,
       sq([sq(Cnx),ss("%"),ss(Id),ss("/"),ix(Ar)])) :-
  ssVarConstraints(Cons,Dp,Cnx).
ssType(tFun(_,_,_,_,Ar,Id),false,_,sq([ss("%"),ss(Id),ss("/"),ix(Ar)])).
ssType(type(Nm),_,_,id(Id)) :- typeName(Nm,Id).
ssType(tpFun(Nm,Ar),_,_,sq([id(Id),ss("/"),ix(Ar)])) :- typeName(Nm,Id).
ssType(tpExp(Nm,A),ShCon,Dp,S) :- ssTypeExp(tpExp(Nm,A),ShCon,Dp,S).
ssType(tplType(A),ShCon,Dp,sq([lp,iv(ss(","),AA),rp])) :-
  ssTypeEls(A,ShCon,Dp,AA).
ssType(funType(A,R),ShCon,Dp,sq([AA,ss("=>"),RR])) :-
  ssType(A,ShCon,Dp,AA),
  ssType(R,ShCon,Dp,RR).
ssType(consType(A,R),ShCon,Dp,sq([AA,ss("<=>"),RR])) :-
  ssType(A,ShCon,Dp,AA),
  ssType(R,ShCon,Dp,RR).
ssType(throwsType(A,R),ShCon,Dp,sq([AA,ss(" throws "),RR])) :-
  ssType(A,ShCon,Dp,AA),
  ssType(R,ShCon,Dp,RR).
ssType(refType(R),ShCon,Dp,sq([ss("ref "),RR])) :- ssType(R,ShCon,Dp,RR).
ssType(valType(R),ShCon,Dp,sq([ss("val "),RR])) :- ssType(R,ShCon,Dp,RR).
ssType(allType(V,T),ShCon,Dp,sq([ss("all "),iv(ss(","),[types:tvr(V)|VV]),ss("~"),TT])) :-
  deRef(T,T0),
  ssABound(T0,ShCon,Dp,VV,TT).
ssType(existType(V,T),ShCon,Dp,
       sq([ss("exist "),iv(ss(","),[types:tvr(V)|VV]),ss("~"),TT])) :-
  ssEBound(T,ShCon,Dp,VV,TT).
ssType(faceType(Els,Tps),ShCon,Dp,sq([ss("{"),iv(nl(Dp2),FFTT),ss("}")])) :-
  Dp2 is Dp+2,
  map(Els,types:ssField(ShCon,Dp2),FF),
  map(Tps,types:ssTypeField(ShCon,Dp2),TT),
  concat(FF,TT,FFTT).
ssType(typeExists(Hd,Bd),ShCon,Dp,sq([HH,ss("<~"),BB])) :-
  ssType(Hd,ShCon,Dp,HH),
  ssType(Bd,ShCon,Dp,BB).
ssType(typeLambda(Hd,Bd),ShCon,Dp,sq([HH,ss("~>"),BB])) :-
  ssType(Hd,ShCon,Dp,HH),
  ssType(Bd,ShCon,Dp,BB).
ssType(contractExists(Hd,Bd),ShCon,Dp,sq([HH,ss("<~"),BB])) :-
  ssConstraint(ShCon,Dp,Hd,HH),
  ssType(Bd,ShCon,Dp,BB).
ssType(constrained(Tp,Con),_ShCon,Dp,sq([CC,ss("|:"),TT])) :-
  ssConstraint(false,Dp,Con,CC),
  ssType(Tp,false,Dp,TT).

ssABound(allType(V,T),ShCon,Dp,[types:tvr(V)|Vs],TT) :-
  deRef(T,T0),
  ssABound(T0,ShCon,Dp,Vs,TT).
ssABound(T,ShCon,Dp,[],TT) :-
  ssType(T,ShCon,Dp,TT).
ssEBound(existType(V,T),ShCon,Dp,[types:tvr(V)|Vs],TT) :-
  deRef(T,T0),
  ssABound(T0,ShCon,Dp,Vs,TT).
ssEBound(T,ShCon,Dp,[],TT) :-
  ssType(T,ShCon,Dp,TT).

tvr(kVar(X),id(X)).
tvr(kFun(X,A),sq([id(X),ss("/"),ix(A)])).

ssTypeExp(T,ShCon,Dp,sq([Op,ss("["),iv(ss(","),Els),ss("]")])) :-
  deRef(T,Tp),
  ssTpExp(Tp,ShCon,Dp,Op,REls),
  reverse(REls,Els).

ssTpExp(tpExp(T,A),ShCon,Dp,OO,[AA|Els]) :-!,
  deRef(T,Op),
  ssTpExp(Op,ShCon,Dp,OO,Els),
  ssType(A,ShCon,Dp,AA).
ssTpExp(tpFun(Op,_),_ShCon,_Dp,id(Id),[]) :- typeName(Op,Id).
ssTpExp(kFun(Op,_),_ShCon,_Dp,id(Op),[]).
ssTpExp(Tp,ShCon,Dp,TT,[]) :-
  ssType(Tp,ShCon,Dp,TT).

ssField(ShCon,Dp,(Nm,Tp),sq([id(Nm),ss(":"),TT])) :-
  ssType(Tp,ShCon,Dp,TT).
ssTypeField(ShCon,Dp,(Nm,Tp),sq([id(Nm),ss(":"),TT])) :-
  ssType(Tp,ShCon,Dp,TT).

dispConstraint(Tp) :-
  ssConstraint(true,0,Tp,TT),
  displayln(TT).

ssConstraint(ShCon,Dp,conTract(Nm,Els,[]),sq([id(Nm),ss("["),iv(ss(","),EE),ss("]")])) :-!,
  ssTypeEls(Els,ShCon,Dp,EE).
ssConstraint(ShCon,Dp,conTract(Nm,Els,Deps),
	       sq([id(Nm),ss("["),iv(ss(","),EE),ss("->>"),
		   iv(ss(","),DD),ss("]")])) :-
  ssTypeEls(Els,ShCon,Dp,EE),
  ssTypeEls(Deps,ShCon,Dp,DD).
ssConstraint(ShCon,Dp,implementsFace(Tp,Face),sq([TT,ss("<~"),FF])) :-
  ssType(Tp,ShCon,Dp,TT),
  ssType(Face,ShCon,Dp,FF).
ssConstraint(ShCon,Dp,implicit(Nm,Tp),sq([ss("("),ss(Nm),ss(" : "),TT,ss(")")])) :-
  ssType(Tp,ShCon,Dp,TT).

ssVarConstraints(C,_,[]) :- var(C),!.
ssVarConstraints([C1|Cx],Dp,[CC,ss(",")|Cs]) :-
  nonvar(Cx),!,
  ssConstraint(false,Dp,C1,CC),
  ssVarConstraints(Cx,Dp,Cs).
ssVarConstraints([C|_],Dp,[CC,ss("|:")]) :-
  ssConstraint(false,Dp,C,CC).

ssTypeEls(Tps,ShCon,Dp,TT) :-
  map(Tps,types:ssTp(ShCon,Dp),TT).

ssTp(ShCon,Dp,Tp,TT) :-
  ssType(Tp,ShCon,Dp,TT).

typeArity(Tp,Ar) :- deRef(Tp,T), tArity(T,Ar),!.

tArity(type(_),0).
tArity(kFun(_,Ar),Ar).
tArity(kVar(_),0).
tArity(tFun(_,_,_,Ar,_),Ar).
tArity(tpFun(_,Ar),Ar).
tArity(tpExp(Op,_),Ar) :-
  typeArity(Op,A1),
  Ar is A1-1.

progTypeArity(Tp,Ar) :- deRef(Tp,TTp), tpArity(TTp,Ar).

tpArity(allType(_,Tp),Ar) :- !, progTypeArity(Tp,Ar).
tpArity(existType(_,Tp),Ar) :- !, progTypeArity(Tp,Ar).
tpArity(constrained(Tp,_),Ar) :- !,
  progTypeArity(Tp,A), Ar is A+1.
tpArity(funType(A,_),Ar) :- !,
  progTypeArity(A,Ar).
tpArity(consType(A,_),Ar) :- !,
  tpArity(A,Ar).
tpArity(throwsType(A,_),Ar) :- !,
  tpArity(A,Ar).
tpArity(refType(A),Ar) :- !,
  progTypeArity(A,Ar).
tpArity(tplType(A),Ar) :- !,length(A,Ar).
tpArity(faceType(A,_),Ar) :- !,length(A,Ar).
tpArity(_,0).

progArgTypes(Tp,ArTps) :- deRef(Tp,TT), tpArgTypes(TT,ArTps).

tpArgTypes(allType(_,Tp),ArTps) :- tpArgTypes(Tp,ArTps).
tpArgTypes(existType(_,Tp),ArTps) :- tpArgTypes(Tp,ArTps).
tpArgTypes(constrained(_,Tp),ArTps) :- tpArgTypes(Tp,ArTps).
tpArgTypes(funType(A,_),ArTps) :- tpArgTypes(A,ArTps).
tpArgTypes(throwsType(A,_),ArTps) :- tpArgTypes(A,ArTps).
tpArgTypes(tplType(ArTps),ArTps).

funResType(Tp,ResTp) :- deRef(Tp,TT), resType(TT,ResTp).

resType(allType(_,Tp),ResTp) :- resType(Tp,ResTp).
resType(existType(_,Tp),ResTp) :- resType(Tp,ResTp).
resType(constrained(_,Tp),ResTp) :- resType(Tp,ResTp).
resType(funType(_,R),R) :- !.
resType(R,R) :- !.

isFunctionType(T) :- deRef(T,Tp), isFunctionType(Tp,_).

isFunctionType(allType(_,T),Ar) :- deRef(T,Tp),isFunctionType(Tp,Ar).
isFunctionType(funType(A,_),Ar) :- progTypeArity(A,Ar).

isCnsType(Tp,Arg,Rep) :- deRef(Tp,T), isCnsTp(T,Arg,Rep).

isCnsTp(allType(_,Tp),Arg,Rep) :- isCnsTp(Tp,Arg,Rep).
isCnsTp(constrained(Tp,_),Arg,Rep) :- isCnsTp(Tp,Arg,Rep).
isCnsTp(consType(A,R),A,R).

isProgramType(Tp) :- deRef(Tp,TT), isProgType(TT).

isProgType(allType(_,Tp)) :- !, isProgType(Tp).
isProgType(constrained(Tp,_)) :- isProgType(Tp).
isProgType(Tp) :- isFunctionType(Tp),!.
isProgType(Tp) :- isCnsType(Tp,_,_),!.

isTypeLam(Tp) :- isTypeLam(Tp,_).

isTypeLam(Tp,Ar) :- deRef(Tp,TT), isTpLam(TT,Ar).

isTpLam(typeLambda(At,_),Ar) :- progTypeArity(At,Ar).
isTpLam(allType(_,T),Ar) :- isTpLam(T,Ar).
isTpLam(existType(_,T),Ar) :- isTpLam(T,Ar).
isTpLam(constrained(T,_),Ar) :- isTpLam(T,Ar).

mkTypeRule(typeLambda(Args,Tp),Tp,typeLambda(Args,Tp)) :-!.
mkTypeRule(allType(K,T),allType(K,Tp),allType(K,R)) :-
  mkTypeRule(T,Tp,R).
mkTypeRule(existType(K,T),existType(K,Tp),existType(K,R)) :-
  mkTypeRule(T,Tp,R).
mkTypeRule(constrained(T,C),constraint(Tp,C),constrained(R,C)) :-
  mkTypeRule(T,Tp,R).
mkTypeRule(Tp,Tp,Tp).

isTypeExp(Tp,Op,Args) :-
  isTpExp(Tp,Op,Args,[]).

isTpExp(tpExp(O,A),Op,Args,Ax) :-
  isTpExp(O,Op,Args,[A|Ax]).
isTpExp(Op,Op,Args,Args).

mkTypeExp(Op,[],Op).
mkTypeExp(Op,[A|Args],Tp) :-
  mkTypeExp(tpExp(Op,A),Args,Tp).

contType(Arg,Tp) :-
  mkTypeExp(tpFun("star.core*cont",1),[Arg],Tp).

tpName(Tp,Nm) :-
  deRef(Tp,RTp),
  tpNm(RTp,Nm).

consTpName(allType(_,T),Nm) :- consTpName(T,Nm).
consTpName(existType(_,T),Nm) :- consTpName(T,Nm).
consTpName(constrained(T,_),Nm) :- consTpName(T,Nm).
consTpName(consType(_,Tp),Nm) :- tpName(Tp,Nm).

implementationName(conTract(Nm,Args,_),INm) :-
  appStr(Nm,S0,S1),
  marker(over,M),
  surfaceNames(Args,M,S1,[]),
  string_chars(INm,S0).

surfaceNames([],_,S,S).
surfaceNames([T|L],Sep,S0,Sx) :-
  deRef(T,TT),
  tpNm(TT,SN),
  appStr(Sep,S0,S1),
  appStr(SN,S1,S2),
  surfaceNames(L,Sep,S2,Sx).

tpNm(type(Nm),Nm).
tpNm(tpExp(Op,_),Nm) :- deRef(Op,OO), tpNm(OO,Nm).
tpNm(kVar(Nm),Nm).
tpNm(kFun(Nm,_),Nm).
tpNm(tpFun(Nm,_),Nm).
tpNm(tVar(_,_,_,Nm,_),Nm).
tpNm(tFun(_,_,_,Nm,_,_),Nm).
tpNm(allType(_,Tp),Nm) :-
  tpNm(Tp,Nm).
tpNm(constrained(T,_),Nm) :-
  tpNm(T,Nm).
tpNm(typeLambda(_,R),Nm) :-
  tpNm(R,Nm).
tpNm(tplType(Els),Nm) :-
  length(Els,Ar),
  swritef(Nm,"()%d",[Ar]).
tpNm(faceType(Flds,_),Nm) :-
  sort(Flds,types:cmpFld,SFlds),
  project0(SFlds,Fns),
  interleave(Fns,"|",Fs),
  concatStrings(Fs,AllFs),
  stringHash(0,AllFs,Hash),
  swritef(Nm,"{}%d",[Hash]).

cmpFld((F1,_),(F2,_)) :- str_lt(F1,F2).

contractType(conTract(Nm,A,D),Tp) :-
  concat(A,D,Args),
  length(Args,Ar),
  mkTypeExp(tpFun(Nm,Ar),Args,Tp).
contractType(implicit(_,Tp),Tp).
contractType(allType(V,CT),allType(V,T)) :-
  contractType(CT,T).
contractType(constrained(_,Cn),Tp) :-
  contractType(Cn,Tp).

contractTypes(CTs,TPs) :-
  map(CTs,types:contractType,TPs).

stdType("integer",type("star.core*integer"),typeExists(type("star.core*integer"),faceType([],[]))).
stdType("bigint",type("star.core*bigint"),typeExists(type("star.core*bigint"),faceType([],[]))).
stdType("float",type("star.core*float"),typeExists(type("star.core*float"),faceType([],[]))).
stdType("boolean",type("star.core*boolean"),typeExists(type("star.core*boolean"),faceType([],[]))).
stdType("char",type("star.core*char"),typeExists(type("star.core*char"),faceType([],[]))).
stdType("string",type("star.core*string"),typeExists(type("star.core*string"),faceType([],[]))).
stdType("cons",
	tpFun("star.core*cons",1),
	allType(kVar("a"),
		typeExists(tpExp(tpFun("star.core*cons",2),kVar("a")),
			   faceType([],[])))).
stdType("package",type("star.pkg*pkg"),typeExists(type("star.pkg*pkg"),faceType([],[]))).
stdType("version",type("star.pkg*version"),typeExists(type("star.pkg*version"),faceType([],[]))).
stdType("file",type("star.file*fileHandle"),typeExists(type("star.file*fileHandle"),faceType([],[]))).
stdType("fiber",
	tpFun("star.core*fiber",2),
	allType(kVar("a"),
		allType(kVar("e"),
			typeExists(tpExp(tpExp(tpFun("star.core*fiber",2),kVar("a")),
					 kVar("e")),
				   faceType([],[]))))).
stdType("cont",
	tpFun("star.core*cont",1),
	allType(kVar("e"),
		typeExists(tpExp(tpFun("star.core*cont",1),kVar("e")),
			   faceType([],[])))).

toLtipe(Tp,LTp) :-
  deRef(Tp,DTp),
  toLtp(DTp,LTp).

toLtp(type("star.core*integer"),i64Tipe) :- !.
toLtp(type("star.core*float"),f64Tipe) :- !.
toLtp(type("star.core*boolean"),blTipe) :- !.
toLtp(funType(Args,Res),fnTipe(As,R)) :-
  toLtipe(Args,As),
  toLtipe(Res,R).
toLtp(throwsType(Arg,Res),throwsTipe(A,R)) :-
  toLtipe(Arg,A),
  toLtipe(Res,R).
toLtp(tplType(Args),tplTipe(As)) :-
  map(Args,types:toLtipe,As).
toLtp(_,ptrTipe).

mkTplTipe(Cnt,tplTipe(As)) :-
  mkPtrs(Cnt,As),!.

mkPtrs(0,[]) :-!.
mkPtrs(I,[ptrTipe|As]) :-
  I1 is I-1,
  mkPtrs(I1,As).

unitTp(tplType([])).

lastBindingLoc(tVar(Curr,_,Lc,_,_),_,Lc) :-
  nonvar(Curr), \+Curr=tVar(_,_,_,_,_),!.
lastBindingLoc(tVar(Curr,_,VLc,_,_),_,Lc) :-
  nonvar(Curr),!,
  lastBindingLoc(Curr,VLc,Lc).
lastBindingLoc(_,Lc,Lc).

typeName(Nm,Id) :- localName(Nm,type,Id),!.
typeName(Nm,Nm).

occursIn(V,Tp) :-
  deRef(V,DV),
  deRef(Tp,DTp),
  occIn(DV,DTp).

occIn(V,VV) :- isIdenticalVar(V,VV),!.
occIn(V,tpExp(O,_)) :- deRef(O,OO),occIn(V,OO),!.
occIn(V,tpExp(_,A)) :- deRef(A,AA),occIn(V,AA),!.
occIn(V,refType(I)) :- deRef(I,II),occIn(V,II).
occIn(V,tplType(L)) :- is_member(A,L), deRef(A,AA),occIn(V,AA).
occIn(V,funType(A,_)) :- deRef(A,AA),occIn(V,AA).
occIn(V,funType(_,R)) :- deRef(R,RR),occIn(V,RR).
occIn(V,consType(L,_)) :- deRef(L,LL),occIn(V,LL).
occIn(V,consType(_,R)) :- deRef(R,RR),occIn(V,RR).
occIn(V,throwsType(A,_)) :- deRef(A,AA),occIn(V,AA).
occIn(V,throwsType(_,R)) :- deRef(R,RR),occIn(V,RR).
occIn(V,constrained(_,C)) :- deRef(C,CC),occIn(V,CC),!.
occIn(V,constrained(T,_)) :- deRef(T,TT),occIn(V,TT),!.
occIn(V,typeLambda(A,_)) :- deRef(A,AA),occIn(V,AA).
occIn(V,typeLambda(_,R)) :- deRef(R,RR),occIn(V,RR).
occIn(V,existType(VV,T)) :- \+isIdenticalVar(V,VV),deRef(T,TT),occIn(V,TT).
occIn(V,allType(VV,T)) :- \+isIdenticalVar(V,VV),deRef(T,TT),occIn(V,TT).
occIn(V,faceType(L,_)) :- is_member((_,A),L), deRef(A,AA),occIn(V,AA),!.
occIn(V,faceType(_,T)) :- is_member((_,A),T), deRef(A,AA),occIn(V,AA),!.
occIn(V,contractExists(C,_)) :- deRef(C,Con),occIn(V,Con),!.
occIn(V,contractExists(_,T)) :- deRef(T,Tp),occIn(V,Tp),!.
occIn(V,typeExists(C,_)) :- deRef(C,Con),occIn(V,Con),!.
occIn(V,typeExists(_,T)) :- deRef(T,Tp),occIn(V,Tp),!.
occIn(V,conTract(_,T,_)) :- is_member(A,T), deRef(A,AA),occIn(V,AA).
occIn(V,conTract(_,_,D)) :- is_member(A,D), deRef(A,AA),occIn(V,AA).
occIn(V,implementsFace(T,_)) :- deRef(T,TT),occIn(V,TT),!.
occIn(V,implementsFace(_,F)) :- deRef(F,FF),occIn(V,FF),!.

