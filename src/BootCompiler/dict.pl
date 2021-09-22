:- module(dict,[declareType/4,isType/3,
		declareTypeVars/4,isTypeVar/3,
		declareVar/4,declareVr/5,declareField/6,
		declareEnum/6,declareCns/6,
		mkVr/4,isVar/3,getVar/5,
		currentVar/3,restoreVar/4,
		declareContract/4,getContract/3,
		declareImplementation/5,
		declareFieldAccess/6,getFieldAccess/5,
		getImplementation/4,
		declareConstraint/4,declareConstraints/4,allConstraints/2,
		pushScope/2,mergeDict/4,pushFace/4,makeKey/2,stdDict/1,
		dispEnv/1
	       ]).

:- use_module(canon).
:- use_module(display).
:- use_module(misc).
:- use_module(types).
:- use_module(escapes).
:- use_module(intrinsics).
:- use_module(location).
:- use_module(errors).

isType(Nm,Env,Tp) :-
  marker(type,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  typeInD(Key,Env,Tp).

typeInD(Key,[dict(Types,_,_,_,_,_)|_],Tp) :- get_dict(Key,Types,Tp),!.
typeInD(Key,[_|Env],Tp) :- typeInD(Key,Env,Tp).

declareType(Nm,TpDef,[dict(Types,Nms,Cons,Impls,Accs,Contracts)|Outer],
	    [dict(Types1,Nms,Cons,Impls,Accs,Contracts)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Types,TpDef,Types1).

declareTypeVars([],_,Env,Env).
declareTypeVars([(Nm,Tp)|Vars],Lc,Env,Ex) :-
  declareType(Nm,tpDef(Lc,Tp,voidType),Env,E0),
  declareTypeVars(Vars,Lc,E0,Ex).

isTypeVar(Nm,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,voidType)),!.

declareVar(Nm,Vr,[dict(Types,Names,Cns,Impls,Accs,Contracts)|Outer],
	   [dict(Types,Names1,Cns,Impls,Accs,Contracts)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Names,Vr,Names1).

declareVr(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,dict:mkVr(Nm),Tp),Env,Ev).
declareField(Lc,Rc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,dict:mkFld(Nm,Rc),Tp),Env,Ev).

isVar(Nm,_,vrEntry(std,dict:mkVr(Nm),Tp)) :- isIntrinsic(Nm,Tp,_),!.
isVar(Nm,_,vrEntry(std,dict:mkVr(Nm),Tp)) :- escapeType(Nm,Tp),!.
isVar(Nm,Env,Vr) :- makeKey(Nm,Key), isVr(Key,Env,Vr).

getVar(Lc,Nm,Env,Env,Vr) :-
  isVar(Nm,Env,vrEntry(_,MkTrm,VTp)),
  freshen:freshen(VTp,Env,_Q,VrTp),
  getConstraints(VrTp,Cx,ViTp),
  call(MkTrm,Lc,ViTp,VExp),
  manageConstraints(Cx,Lc,VExp,Vr).

declareEnum(Lc,Nm,FullNm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,dict:mkEnum(FullNm),Tp),Env,Ev).

declareCns(Lc,Nm,FullNm,Tp,Env,Ev) :-
  declareVar(Nm,vrEntry(Lc,dict:mkCns(FullNm),Tp),Env,Ev).

mkCns(Nm,Lc,Tp,cons(Lc,Nm,Tp)).
mkEnum(Nm,Lc,Tp,enm(Lc,Nm,ETp)) :- netEnumType(Tp,ETp).

manageConstraints([],_,Exp,Exp).
manageConstraints([implementsFace(TV,Fc)|Cx],Lc,Term,Exp) :-
  manageConstraints(Cx,Lc,overaccess(Term,TV,Fc),Exp).
manageConstraints([Con|Cx],Lc,V,over(Lc,V,Tp,[Con|Cx])) :-
  typeOfCanon(V,Tp).

mkVr(Nm,Lc,Tp,v(Lc,Nm,Tp)).
noFace(_,faceType([],[])).
mkFld(Fld,Rc,Lc,Tp,dot(Lc,Rc,Fld,Tp)).

isVr(Key,[dict(_,Names,_,_,_,_)|_],Vr) :- get_dict(Key,Names,Vr),!.
isVr(Key,[_|Outer],Vr) :- isVr(Key,Outer,Vr).

currentVar(Nm,Env,some(Vr)) :- makeKey(Nm,Key), isVr(Key,Env,Vr),!.
currentVar(_,_,none).

restoreVar(Nm,Env,some(Vr),Ev) :-
  declareVar(Nm,Vr,Env,Ev).
restoreVar(_,Env,none,Env).

declareContract(Nm,Con,[dict(Types,Nms,Cns,Impls,Accs,Contracts)|Outer],
		[dict(Types,Nms,Cns,Impls,Accs,Cons)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Contracts,Con,Cons).

getContract(Nm,Env,Con) :-
  marker(conTract,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  contractInD(Key,Env,Con).

contractInD(Ky,[dict(_,_,_,_,_,Cons)|_],Con) :- get_dict(Ky,Cons,Con),!.
contractInD(Key,[_|Env],Con) :- contractInD(Key,Env,Con).

declareConstraint(Lc,C,E,Ev) :- C=conTract(_,_Args,_Deps),!,
  implementationName(C,ImpNm),
  contractType(C,CTp),
  declareVr(Lc,ImpNm,CTp,E,Ev).
declareConstraint(_,Con,[dict(Types,Nms,Cns,Impl,Accs,Cons)|Outer],
		  [dict(Types,Nms,[Con|Cns],Impl,Accs,Cons)|Outer]).

declareConstraints(_,[],Env,Env).
declareConstraints(Lc,[C|L],E,Ex) :-
  declareConstraint(Lc,C,E,E0),
  declareConstraints(Lc,L,E0,Ex).

allConstraints([],[]).
allConstraints([dict(_,_,Cns,_,_,_)|Env],All) :-
  allConstraints(Env,Outer),
  concat(Cns,Outer,All).

declareImplementation(ImplNm,ImplVrNm,Tp,
		      [dict(Types,Names,Cns,Impls,Accs,Contracts)|Outer],
		      [dict(Types,Names,Cns,Implx,Accs,Contracts)|Outer]) :-
  makeKey(ImplNm,Key),
  put_dict(Key,Impls,implement(ImplNm,ImplVrNm,Tp),Implx).

getImplementation(Env,ImplNm,ImplVrNm,ImplTp) :-
  makeKey(ImplNm,Key),
  implInDct(Env,Key,ImplVrNm,ImplTp).

implInDct([dict(_,_,_,Impls,_,_)|_],Key,ImplVrNm,ImplTp) :-
  get_dict(Key,Impls,implement(_,ImplVrNm,ImplTp)).
implInDct([_|Env],Key,ImplVrNm,ImplTp) :-
  implInDct(Env,Key,ImplVrNm,ImplTp).

declareFieldAccess(Tp,Fld,Fun,FldTp,[dict(Types,Vars,Cns,Impls,Acs,Cons)|Outer],
		   [dict(Types,Vars,Cns,Impls,Acx,Cons)|Outer]) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  (get_dict(Key,Acs,Accors) ->
   put_dict(Key,Acs,[(Fld,Fun,FldTp)|Accors],Acx) ;
   put_dict(Key,Acs,[(Fld,Fun,FldTp)],Acx)).

getFieldAccess(Tp,Fld,Fun,FldTp,Dict) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  accInDct(Key,Fld,Fun,FldTp,Dict).

accInDct(Ky,Fld,Fun,FldTp,[dict(_,_,_,_,Acc,_)|_]) :-
  get_dict(Ky,Acc,A),
  is_member((Fld,Fun,FldTp),A),!.
accInDct(Ky,Fld,Fun,FldTp,[_|Outer]) :-
  accInDct(Ky,Fld,Fun,FldTp,Outer).
  
pushScope(Env,[dict(types{},vars{},[],impls{},accs{},contracts{})|Env]).

mergeDict(D1,D2,Env,D3) :-
  length(D1,L1),
  length(D2,L1),!,
  mergeScopes(D1,D2,Env,D3).

mergeScopes([dict(Ts1,Ns1,Cns,Impls,Accs,Cons)|O],
    [dict(Ts2,Ns2,Cns,Impls,Accs,Cons)|O],Env,
	    [dict(Ts3,N3,Cns,Impls,Accs,Cons)|O]) :-
  dict_pairs(Ts1,T,T1),
  dict_pairs(Ts2,_,T2),
  mergeTDefs(T1,T2,Env,Ts),
  dict_pairs(Ts3,T,Ts),
  dict_pairs(Ns1,_,N1),
  dict_pairs(Ns2,N,N2),
  mergeVDefs(N1,N2,Env,Ns),
  dict_pairs(N3,N,Ns).

mergeTDefs([],_,_,[]).
mergeTDefs(_,[],_,[]).
mergeTDefs([Vr-T1|D1],D2,Env,[Vr-T1|D3]) :-
  is_member(Vr-T2,D2),
  sameTpDef(T1,T2,Env),
  mergeTDefs(D1,D2,Env,D3).
mergeTDefs([_|D1],D2,Env,D3) :-
  mergeTDefs(D1,D2,Env,D3).

mergeVDefs([],_,_,[]).
mergeVDefs(_,[],_,[]).
mergeVDefs([Vr-T1|D1],D2,Env,[Vr-T1|D3]) :-
  is_member(Vr-T2,D2),
  sameDesc(T1,T2,Env),
  mergeVDefs(D1,D2,Env,D3).
mergeVDefs([_|D1],D2,Env,D3) :-
  mergeVDefs(D1,D2,Env,D3).

sameTpDef(tpDef(Lc,T1,R1),tpDef(_,T2,R2),Env) :-
  unify:sameType(T1,T2,Lc,Env),
  unify:sameType(R1,R2,Lc,Env).

sameDesc(vrEntry(Lc,C1,Tp1),vrEntry(_,C1,Tp2),Env) :-
  unify:sameType(Tp1,Tp2,Lc,Env).

pushFace(faceType(Vrs,Tps),Lc,Env,ThEnv) :-
  pushFields(Vrs,Lc,Env,E0),
  pushTypes(Tps,Lc,E0,ThEnv).

pushFields([],_,Env,Env).
pushFields([(Nm,Tp)|Fields],Lc,Env,ThEnv) :-
  declareVar(Nm,vrEntry(Lc,dict:mkVr(Nm),Tp),Env,Env0),
  pushFields(Fields,Lc,Env0,ThEnv).

pushTypes([],_,Env,Env).
pushTypes([(N,Type)|Tps],Lc,Env,ThEnv) :-
  mkTypeRule(Type,Tp,Rl),
  declareType(N,tpDef(Lc,Tp,Rl),Env,E0),
  pushTypes(Tps,Lc,E0,ThEnv).

processDictLvl(Dict,P,Cx,Result) :-
  processDictLvl(Dict,P,Cx,[],Result),!.

processDictLvl(_,_,0,SoFar,SoFar).
processDictLvl([],_,_,SoFar,SoFar).
processDictLvl([dict(_,Names,_,_,_,_)|Outer],P,Cx,SoFar,Result) :-
  dict_pairs(Names,_,Pairs),
  procNames(Pairs,P,SoFar,S0),
  Cx1 is Cx-1,
  processDictLvl(Outer,P,Cx1,S0,Result).

procNames([],_,SoFar,SoFar).
procNames([K-Vr|More],P,SoFar,Result) :-
  call(P,K,Vr,SoFar,S0),
  procNames(More,P,S0,Result).

stdDict(Base) :-
  pushScope([],B),
  stdType("integer",IntTp,ITpEx),
  stdType("chars",StrTp,StpEx),
  stdType("float",FltTp,FtEx),
  stdType("cons",ConsTp,ConsEx),
  stdType("file",FileTp,FileEx),
  stdType("tag",TagTp,TagEx),
  stdType("task",TaskTp,TaskEx),
  declareType("chars",tpDef(std,StrTp,StpEx),B,B0),
  declareType("integer",tpDef(std,IntTp,ITpEx),B0,B1),
  declareType("float",tpDef(std,FltTp,FtEx),B1,B2),
  declareType("file",tpDef(std,FileTp,FileEx),B2,B3),
  declareType("cons",tpDef(std,ConsTp,ConsEx),B3,B4),
  declareType("tag",tpDef(std,TagTp,TagEx),B4,B5),
/*  declareEnum(std,"nil","star.core#nil",
	      allType(kVar("e"),
		      consType(tplType([]),
			       tpExp(tpFun("star.core*cons",1),kVar("e")))),B4,B5),
  declareCns(std,"cons","star.core#cons",
	      allType(kVar("e"),
		      consType(tplType([kVar("e"),
					tpExp(tpFun("star.core*cons",1),kVar("e"))]),
			       tpExp(tpFun("star.core*cons",1),kVar("e")))),B5,B6),
    */
%  declareType("action",tpDef(std,ActionTp,ActionEx),B3,B4),
  declareType("task",tpDef(std,TaskTp,TaskEx),B5,Bx),
  Bx=Base.

dispDictLvl(dict(Types,Nms,_Cns,Impls,Accs,Contracts),Cx,
	    sq([ix(Cx),ss("-"),
		iv(nl(2),[TT,VV,II,AA,CC])])) :-
  dispTypes(Types,TT),
  dispVars(Nms,VV),
  dispImplementations(Impls,II),
  dispAccessors(Accs,AA),
  dispContracts(Contracts,CC).

dispTypes(Tps,sq([ss("Types"),nl(4),iv(nl(4),TT)])) :-
  dict_pairs(Tps,_,Pairs),
  map(Pairs,dict:showTpEntry,TT).

showTpEntry(_-tpDef(_Lc,Tp,Rl),sq([TT,ss("~~"),RR])) :-
  ssType(Tp,false,2,TT),
  ssType(Rl,false,2,RR).

dispContracts(Cons,sq([ss("Contracts"),nl(4),iv(nl(4),CC)])) :-
  dict_pairs(Cons,_,Pairs),
  map(Pairs,dict:showContractEntry,CC).

showContractEntry(_-conDef(Nm,FullNm,Tp,Rl),sq([id(Nm),ss("~"),id(FullNm),ss(":"),
						TT,ss("~~"),RR])) :-
  ssType(Tp,false,2,TT),
  ssType(Rl,false,2,RR).

dispVars(Vrs,sq([ss("Vars"),nl(4),iv(nl(4),VV)])) :-
  dict_pairs(Vrs,_,Pairs),
  map(Pairs,dict:showVrEntry,VV).

showVrEntry(Nm-vrEntry(_,_,Tp),sq([id(Nm),ss(":"),TT])) :-
  ssType(Tp,false,2,TT).

dispAccessors(Acs,sq([ss("Accessors"),nl(4),iv(nl(4),AA)])) :-
  dict_pairs(Acs,_,Pairs),
  map(Pairs,dict:showAccessor,AA).

showAccessor(Nm-L,sq([id(Nm),LL])) :-
  dispFieldAccesses(L,LL).

dispFieldAccesses(List,iv(nl(4),LL)) :-
  map(List,dict:dispFieldAccess,LL).

dispFieldAccess((Fld,Fun,FldTp),sq([ss("."),id(Fld),ss(" -> "),id(Fun),TT])) :-
  ssType(FldTp,false,4,TT).

dispImplementations(Acs,sq([ss("Implementations"),nl(4),iv(nl(4),II)])) :-
  dict_pairs(Acs,_,Pairs),
  map(Pairs,dict:showImplementation,II).

showImplementation(_Nm-implement(ImplNm,ImplVrNm,ImplTp),
		   sq([id(ImplNm),ss("="),id(ImplVrNm),ss(":"),TT])) :-
  ssType(ImplTp,false,4,TT).

dispEnv(Env) :-
  dispEnv(Env,0).

dispEnv([],_).
dispEnv([D|Env],Cx) :-
  dispDictLvl(D,Cx,DD),
  display(DD),
  C1 is Cx+1,
  dispEnv(Env,C1).

