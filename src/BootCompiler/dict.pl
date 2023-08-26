:- module(dict,[declareType/4,isType/3,
		declareTypeVars/4,isTypeVar/3,
		declareVr/6,declareField/6,declareMtd/5,declareDyn/6,
		declareEnum/6,declareCns/6,
		getVar/5,getVarTypeFace/4,varLoc/4,
		currentVar/3,
		declareContract/4,getContract/3,
		declareImplementation/5,
		declareFieldAccess/6,getFieldAccess/5,
		declareFieldUpdater/6,getFieldUpdater/5,
		getImplementation/4,
		manageConstraints/4,
		declareConstraints/4,
		allConstraints/2,
		declareTryScope/5,getTryScope/5,
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

typeInD(Key,[dict(Types,_,_,_,_,_,_,_)|_],Tp) :- get_dict(Key,Types,Tp),!.
typeInD(Key,[_|Env],Tp) :- typeInD(Key,Env,Tp).

declareType(Nm,TpDef,[dict(Types,Nms,Cons,Impls,Accs,Ups,Contracts,Trys)|Outer],
	    [dict(Types1,Nms,Cons,Impls,Accs,Ups,Contracts,Trys)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Types,TpDef,Types1).

declareTypeVars([],_,Env,Env).
declareTypeVars([(Nm,Tp)|Vars],Lc,Env,Ex) :-
  declareType(Nm,tpDef(Lc,Tp,voidType),Env,E0),
  declareTypeVars(Vars,Lc,E0,Ex).

isTypeVar(Nm,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,voidType)),!.

declareVar(Nm,Lc,Tp,Face,Mk,
	   [dict(Types,Names,Cns,Impls,Accs,Ups,Contracts,Trys)|Outer],
	   [dict(Types,Names1,Cns,Impls,Accs,Ups,Contracts,Trys)|Outer]) :-
%  reportMsg("Declare %s:%s",[id(Nm),tpe(Tp)],Lc),
  makeKey(Nm,Key),
  put_dict(Key,Names,vrEntry(Lc,Mk,Face,Tp),Names1).

declareVr(Lc,Nm,Tp,Face,Env,Ev) :-
  declareVar(Nm,Lc,Tp,Face,dict:mkVr(Nm),Env,Ev).

declareField(Lc,Rc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,Lc,Tp,none,dict:mkFld(Nm,Rc),Env,Ev).

declareMtd(Lc,Nm,Tp,Env,Ev) :-
  declareVar(Nm,Lc,Tp,none,dict:mkMtd(Nm),Env,Ev).

declareDyn(Lc,Nm,RlNm,Tp,Env,Ev) :-
  declareVar(Nm,Lc,Tp,none,dict:mkVr(RlNm),Env,Ev).

mkMtd(Nm,Lc,Tp,mtd(Lc,Nm,Tp)).

isVar(Nm,_,vrEntry(std,dict:mkVr(Nm),none,Tp)) :- isIntrinsic(Nm,Tp,_),!.
isVar(Nm,_,vrEntry(std,dict:mkVr(Nm),none,Tp)) :- escapeType(Nm,Tp),!.
isVar(Nm,Env,Vr) :- makeKey(Nm,Key), isVr(Key,Env,Vr).

varLoc(Nm,Env,Tp,Lc) :-
  isVar(Nm,Env,vrEntry(Lc,_,_,Tp)),!.

getVar(Lc,Nm,Env,Vr,ViTp) :-
  isVar(Nm,Env,vrEntry(_,MkTrm,_,VTp)),
%  reportMsg("type of %s is %s",[id(Nm),tpe(VTp)],Lc),
  freshen:freshen(VTp,Env,_Q,VrTp),
%  reportMsg("freshened type of %s is %s",[id(Nm),tpe(VrTp)],Lc),
  getConstraints(VrTp,Cx,ViTp),
  call(MkTrm,Lc,ViTp,VExp),
  manageConstraints(Cx,Lc,VExp,Vr).

getVarTypeFace(_Lc,Nm,Env,ViTp) :-
  isVar(Nm,Env,vrEntry(_,_,some(Fce),_)),
  freshen:freshen(Fce,Env,_Q,VrTp),
  getConstraints(VrTp,_Cx,ViTp).

declareEnum(Lc,Nm,FullNm,Tp,Env,Ev) :-
  declareVar(Nm,Lc,Tp,none,dict:mkEnum(FullNm),Env,Ev).

declareCns(Lc,Nm,FullNm,Tp,Env,Ev) :-
  declareVar(Nm,Lc,Tp,none,dict:mkCns(FullNm),Env,Ev).

mkCns(Nm,Lc,Tp,enm(Lc,Nm,Tp)).
mkEnum(Nm,Lc,Tp,enm(Lc,Nm,ETp)) :- netEnumType(Tp,ETp).

manageConstraints([],_,V,V).
manageConstraints([implementsFace(RTp,faceType(Flds,_))|Cx],Lc,Term,Exp) :-
  manageFieldAccess(Flds,Lc,RTp,Term,Dot),
  manageConstraints(Cx,Lc,Dot,Exp).
manageConstraints([Con|Cx],Lc,V,over(Lc,V,[Con|Cx])).

manageFieldAccess([],_,_,Term,Term) :-!.
manageFieldAccess([(Fld,Ftp)|Flds],Lc,RTp,Term,Exp) :-
  manageFieldAccess(Flds,Lc,RTp,overaccess(Lc,Term,RTp,Fld,Ftp),Exp).

mkVr(Nm,Lc,Tp,v(Lc,Nm,Tp)).
noFace(_,faceType([],[])).
mkFld(Fld,Rc,Lc,Tp,dot(Lc,Rc,Fld,Tp)).

isVr(Key,[dict(_,Names,_,_,_,_,_,_)|_],Vr) :- get_dict(Key,Names,Vr),!.
isVr(Key,[_|Outer],Vr) :- isVr(Key,Outer,Vr).

currentVar(Nm,Env,some(Vr)) :- makeKey(Nm,Key), isVr(Key,Env,Vr),!.
currentVar(_,_,none).

declareContract(Nm,Con,[dict(Types,Nms,Cns,Impls,Accs,Ups,Contracts,Trs)|Outer],
		[dict(Types,Nms,Cns,Impls,Accs,Ups,Cons,Trs)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Contracts,Con,Cons).

getContract(Nm,Env,Con) :-
  marker(conTract,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  contractInD(Key,Env,Con).

contractInD(Ky,[dict(_,_,_,_,_,_,Cons,_)|_],Con) :- get_dict(Ky,Cons,Con),!.
contractInD(Key,[_|Env],Con) :- contractInD(Key,Env,Con).

allConstraints([],[]).
allConstraints([dict(_,_,Cons,_,_,_,_,_)|D],C,Cx) :-
  concat(Cons,C,C0),
  allConstraints(D,C0,Cx).

declareConstraint(Lc,C,E,Ev) :- C=conTract(_,_Args,_Deps),!,
  implementationName(C,ImpNm),
  contractType(C,CTp),
  declareVr(Lc,ImpNm,CTp,none,E,Ev).
declareConstraint(Lc,implicit(Nm,Tp),E,Ev) :-
  declareVr(Lc,Nm,Tp,none,E,Ev).
declareConstraint(Lc,raises(Tp),E,Ev) :-
  declareVr(Lc,"$try",Tp,none,E,Ev).
declareConstraint(_,Con,[dict(Types,Nms,Cns,Impl,Accs,Ups,Cons,Trs)|Outer],
		  [dict(Types,Nms,[Con|Cns],Impl,Accs,Ups,Cons,Trs)|Outer]).

declareConstraints(_,[],Env,Env).
declareConstraints(Lc,[C|L],E,Ex) :-
  declareConstraint(Lc,C,E,E0),
  declareConstraints(Lc,L,E0,Ex).

declareImplementation(ImplNm,ImplVrNm,Tp,
		      [dict(Tps,Nms,Cns,Impls,Acs,Ups,Cons,Trs)|Outer],
		      [dict(Tps,Nms,Cns,Implx,Acs,Ups,Cons,Trs)|Outer]) :-
  makeKey(ImplNm,Key),
  put_dict(Key,Impls,implement(ImplNm,ImplVrNm,Tp),Implx).

getImplementation(Env,ImplNm,ImplVrNm,ImplTp) :-
  makeKey(ImplNm,Key),
  implInDct(Env,Key,ImplVrNm,ImplTp).

implInDct([dict(_,_,_,Impls,_,_,_,_)|_],Key,ImplVrNm,ImplTp) :-
  get_dict(Key,Impls,implement(_,ImplVrNm,ImplTp)).
implInDct([_|Env],Key,ImplVrNm,ImplTp) :-
  implInDct(Env,Key,ImplVrNm,ImplTp).

declareFieldAccess(Tp,Fld,Fun,FldTp,[dict(Tps,Vrs,Cns,Impls,Acs,Ups,Cons,Trs)|Outer],
		   [dict(Tps,Vrs,Cns,Impls,Acx,Ups,Cons,Trs)|Outer]) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  (get_dict(Key,Acs,Accors) ->
   put_dict(Key,Acs,[(Fld,Fun,FldTp)|Accors],Acx) ;
   put_dict(Key,Acs,[(Fld,Fun,FldTp)],Acx)).

getFieldAccess(Tp,Fld,Fun,FldTp,Dict) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  accInDct(Key,Fld,Fun,FldTp,Dict).

accInDct(Ky,Fld,Fun,FldTp,[dict(_,_,_,_,Acc,_,_,_)|_]) :-
  get_dict(Ky,Acc,A),
  is_member((Fld,Fun,FldTp),A),!.
accInDct(Ky,Fld,Fun,FldTp,[_|Outer]) :-
  accInDct(Ky,Fld,Fun,FldTp,Outer).

declareFieldUpdater(Tp,Fld,Fun,FldTp,[dict(Tps,Vrs,Cns,Impls,Acs,Ups,Cons,Trs)|Outer],
		   [dict(Tps,Vrs,Cns,Impls,Acs,Upx,Cons,Trs)|Outer]) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  (get_dict(Key,Ups,Accors) ->
   put_dict(Key,Ups,[(Fld,Fun,FldTp)|Accors],Upx) ;
   put_dict(Key,Ups,[(Fld,Fun,FldTp)],Upx)).

getFieldUpdater(Tp,Fld,Fun,FldTp,Dict) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  upInDct(Key,Fld,Fun,FldTp,Dict).

upInDct(Ky,Fld,Fun,FldTp,[dict(_,_,_,_,_,Acc,_,_)|_]) :-
  get_dict(Ky,Acc,A),
  is_member((Fld,Fun,FldTp),A),!.
upInDct(Ky,Fld,Fun,FldTp,[_|Outer]) :-
  upInDct(Ky,Fld,Fun,FldTp,Outer).

declareTryScope(Lc,Tp,TpVrNm,[dict(Tps,Vrs,Cns,Impls,Acs,Ups,Cons,Trs)|Outer],
		[dict(Tps,Vrs,Cns,Impls,Acs,Ups,Cons,Trx)|Outer]) :-
  tpName(Tp,TpNm),
  makeKey(TpNm,Key),!,
  put_dict(Key,Trs,tryBlk(Lc,TpVrNm,Tp),Trx).

getTryScope(TpNm,Dict,Lc,Nm,ETp) :-
  makeKey(TpNm,Key),!,
  tryInDct(Key,Lc,Nm,ETp,Dict).

tryInDct(Key,Lc,Nm,Tp,[dict(_,_,_,_,_,_,_,Trs)|_]) :-
  get_dict(Key,Trs,tryBlk(Lc,Nm,Tp)),!.
tryInDct(Key,Lc,Nm,Tp,[_|Outer]) :-
  tryInDct(Key,Lc,Nm,Tp,Outer).
  
pushScope(Env,[dict(types{},vars{},[],impls{},accs{},ups{},contracts{},trys{})|Env]).

mergeDict(D1,D2,Env,D3) :-
  length(D1,L1),
  length(D2,L1),!,
  mergeScopes(D1,D2,Env,D3).

mergeScopes([dict(Ts1,Ns1,Cns,Impls,Accs,Ups,Cons,Trs)|O],
    [dict(Ts2,Ns2,Cns,Impls,Accs,Ups,Cons,Trs)|O],Env,
	    [dict(Ts3,N3,Cns,Impls,Accs,Ups,Cons,Trs)|O]) :-
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

sameDesc(vrEntry(Lc,C1,_,Tp1),vrEntry(_,C1,_,Tp2),Env) :-
  unify:sameType(Tp1,Tp2,Lc,Env).

pushFace(faceType(Vrs,Tps),Lc,Env,ThEnv) :-
  pushFields(Vrs,Lc,Env,E0),
  pushTypes(Tps,Lc,E0,ThEnv).

pushFields([],_,Env,Env).
pushFields([(Nm,Tp)|Fields],Lc,Env,ThEnv) :-
  declareVar(Nm,Lc,Tp,none,dict:mkVr(Nm),Env,Env0),
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
processDictLvl([dict(_,Names,_,_,_,_,_,_)|Outer],P,Cx,SoFar,Result) :-
  dict_pairs(Names,_,Pairs),
  procNames(Pairs,P,SoFar,S0),
  Cx1 is Cx-1,
  processDictLvl(Outer,P,Cx1,S0,Result).

procNames([],_,SoFar,SoFar).
procNames([K-Vr|More],P,SoFar,Result) :-
  call(P,K,Vr,SoFar,S0),
  procNames(More,P,S0,Result).

declareStdTypes([],D,D).
declareStdTypes([T|Ts],D,Dx) :-
  stdType(T,Tp,TpEx),
  declareType(T,tpDef(std,Tp,TpEx),D,D1),
  declareStdTypes(Ts,D1,Dx).

stdDict(Base) :-
  pushScope([],B),
  declareStdTypes(["integer","bigint","char","string","float",
		   "cons","file","fiber","cont","tag"],B,Base).

dispDictLvl(dict(Types,Nms,_Cns,Impls,Accs,Ups,Contracts,Trs),Cx,
	    sq([ix(Cx),ss("-"),
		iv(nl(2),[TT,VV,II,AA,UU,CC,BB])])) :-
  dispTypes(Types,TT),
  dispVars(Nms,VV),
  dispImplementations(Impls,II),
  dispAccessors(Accs,AA),
  dispUpdaters(Ups,UU),
  dispContracts(Contracts,CC),
  dispTryBlocks(Trs,BB).

dispTypes(Tps,sq([ss("Types"),nl(4),iv(nl(4),TT)])) :-
  dict_pairs(Tps,_,Pairs),
  map(Pairs,dict:showTpEntry,TT).

showTpEntry(_-tpDef(_Lc,Tp,Rl),sq([TT,ss("~~"),RR])) :-
  ssType(Tp,false,2,TT),
  ssType(Rl,false,2,RR).

dispContracts(Cons,sq([ss("Contracts"),nl(4),iv(nl(4),CC)])) :-
  dict_pairs(Cons,_,Pairs),
  map(Pairs,dict:showContractEntry,CC).

showContractEntry(_-conDef(Nm,FullNm,Rl),sq([id(Nm),ss("~"),id(FullNm),ss(":"),
						RR])) :-
  ssType(Rl,false,2,RR).

dispVars(Vrs,sq([ss("Vars"),nl(4),iv(nl(4),VV)])) :-
  dict_pairs(Vrs,_,Pairs),
  map(Pairs,dict:showVrEntry,VV).

showVrEntry(Nm-vrEntry(_,_,_,Tp),sq([id(Nm),ss(":"),TT])) :-
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

dispUpdaters(Acs,sq([ss("Updaters"),nl(4),iv(nl(4),AA)])) :-
  dict_pairs(Acs,_,Pairs),
  map(Pairs,dict:showUpdater,AA).

showUpdater(Nm-L,sq([id(Nm),LL])) :-
  dispFieldUpdates(L,LL).

dispFieldUpdates(List,iv(nl(4),LL)) :-
  map(List,dict:dispFieldUpdate,LL).

dispFieldUpdate((Fld,Fun,FldTp),sq([ss("."),id(Fld),ss(" -> "),id(Fun),TT])) :-
  ssType(FldTp,false,4,TT).

dispImplementations(Acs,sq([ss("Implementations"),nl(4),iv(nl(4),II)])) :-
  dict_pairs(Acs,_,Pairs),
  map(Pairs,dict:showImplementation,II).

showImplementation(_Nm-implement(ImplNm,ImplVrNm,ImplTp),
		   sq([id(ImplNm),ss("="),id(ImplVrNm),ss(":"),TT])) :-
  ssType(ImplTp,false,4,TT).

dispTryBlocks(Trs,iv(nl(4),TT)) :-
  map(Trs,dict:dispTryBlock,TT).

dispTryBlock(tryBlk(Lc,Vr,Tp),sq([ss("try scope @ "),ss(LL),ss(Vr),ss(":"),TT])) :-
  ssType(Tp,TT),
  ssLoc(Lc,LL).

dispEnv(Env) :-
  dispEnv(Env,0).

dispEnv([],_).
dispEnv([D|Env],Cx) :-
  dispDictLvl(D,Cx,DD),
  display(DD),
  C1 is Cx+1,
  dispEnv(Env,C1).


