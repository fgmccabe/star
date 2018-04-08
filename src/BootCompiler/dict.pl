:- module(dict,[declareType/4,isType/3,
    declareTypeVars/4,isTypeVar/3,
    declareVar/4,mkVr/3,isVar/3,hasType/3,currentVar/3,restoreVar/4,
    declareContract/4,getContract/3,
    declareImplementation/4,getImplementations/3,allImplements/3,
    declareConstraint/3,allConstraints/2,
    processNames/3,processTypes/3,
    pushScope/2,pushFace/4,makeKey/2,stdDict/1]).

:- use_module(misc).
:- use_module(types).
:- use_module(escapes).

isType(Nm,Env,Tp) :-
  marker(type,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  typeInD(Key,Env,Tp).

typeInD(Key,[scope(Types,_,_,_,_)|_],Tp) :- get_dict(Key,Types,Tp),!.
typeInD(Key,[_|Env],Tp) :- typeInD(Key,Env,Tp).

declareType(Nm,TpDef,[scope(Types,Nms,Cons,Impls,Contracts)|Outer],[scope(Types1,Nms,Cons,Impls,Contracts)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Types,TpDef,Types1).

declareTypeVars([],_,Env,Env).
declareTypeVars([(thisType,_)|Vars],Lc,Env,Ex) :- !,
  declareTypeVars(Vars,Lc,Env,Ex).
declareTypeVars([(Nm,Tp)|Vars],Lc,Env,Ex) :-
  declareType(Nm,tpDef(Lc,Tp,voidType),Env,E0),
  declareTypeVars(Vars,Lc,E0,Ex).

isTypeVar(Nm,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,voidType)),!.

declareVar(Nm,Vr,[scope(Types,Names,Cns,Impls,Contracts)|Outer],[scope(Types,Names1,Cns,Impls,Contracts)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Names,Vr,Names1).

isVar(Nm,_,vrEntry(std,dict:mkVr(Nm),Tp,dict:noFace)) :- escapeType(Nm,Tp),!.
isVar(Nm,Env,Vr) :- makeKey(Nm,Key), isVr(Key,Env,Vr).

mkVr(Nm,Lc,v(Lc,Nm)).
noFace(_,faceType([],[])).

isVr(Key,[scope(_,Names,_,_,_)|_],Vr) :- get_dict(Key,Names,Vr),!.
isVr(Key,[_|Outer],Vr) :- isVr(Key,Outer,Vr).

hasType(Nm,Env,Tp) :-
  makeKey(Nm,Ky),
  isVr(Ky,Env,vrEntry(_,_,Tp)),!.

currentVar(Nm,Env,some(Vr)) :- makeKey(Nm,Key), isVr(Key,Env,Vr),!.
currentVar(_,_,none).

restoreVar(Nm,Env,some(Vr),Ev) :-
  declareVar(Nm,Vr,Env,Ev).
restoreVar(_,Env,none,Env).

declareContract(Nm,Con,[scope(Types,Nms,Cns,Impls,Contracts)|Outer],[scope(Types,Nms,Cns,Impls,Cons)|Outer]) :-
  makeKey(Nm,Key),
  put_dict(Key,Contracts,Con,Cons).

getContract(Nm,Env,Con) :-
  marker(conTract,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  contractInD(Key,Env,Con).

contractInD(Ky,[scope(_,_,_,_,Cons)|_],Con) :- get_dict(Ky,Cons,Con),!.
contractInD(Key,[_|Env],Con) :- contractInD(Key,Env,Con).

declareConstraint(Con,[scope(Types,Nms,Cns,Impl,Cons)|Outer],[scope(Types,Nms,[Con|Cns],Impl,Cons)|Outer]).

allConstraints([],[]).
allConstraints([scope(_,_,Cns,_,_)|Env],All) :-
  allConstraints(Env,Outer),
  concat(Cns,Outer,All).

allImplements(T,Env,Face) :-
  allImplements(T,Env,Env,[],Face).

allImplements(T,[scope(_,_,Cns,_,_)|Outer],Env,SoFar,Face) :-
  filteImplements(Cns,T,Env,SoFar,I0),
  allImplements(T,Outer,Env,I0,Face).
allImplements(_,[],_,Face,Face).

filteImplements([],_,_,F,F).
filteImplements([implementsFace(V,Fields)|Cns],VV,Env,F,Face) :-
  deRef(V,VV),
  concat(Fields,F,F0),
  filteImplements(Cns,VV,Env,F0,Face).

declareImplementation(Con,Impl,[scope(Types,Nms,Cns,I,Cons)|Outer],[scope(Types,Nms,Cns,I1,Cons)|Outer]) :-
  makeKey(Con,Key),
  (get_dict(Key,I,Impls) -> put_dict(Key,I,[Impl|Impls],I1); put_dict(Key,I,[Impl],I1)).

getImplementations(Nm,Env,Impls) :-
  marker(conTract,M),
  pathSuffix(Nm,M,Id),
  makeKey(Id,Key),
  implInD(Key,Env,Impls).

implInD(Ky,[scope(_,_,_,Impls,_)|_],I) :- get_dict(Ky,Impls,I),!.
implInD(Key,[_|Env],Con) :- implInD(Key,Env,Con).

pushScope(Env,[scope(types{},vars{},[],impls{},contracts{})|Env]).

pushFace(faceType(Vrs,Tps),Lc,Env,ThEnv) :-
  pushFields(Vrs,Lc,Env,E0),
  pushTypes(Tps,Lc,E0,ThEnv).

pushFields([],_,Env,Env).
pushFields([(Nm,Tp)|Fields],Lc,Env,ThEnv) :-
  declareVar(Nm,vrEntry(Lc,dict:mkVr(Nm),Tp,vartypes:faceTp(Tp)),Env,Env0),
  pushFields(Fields,Lc,Env0,ThEnv).

pushTypes([],_,Env,Env).
pushTypes([(N,Type)|Tps],Lc,Env,ThEnv) :-
  declareType(N,tpDef(Lc,Type,faceType([],[])),Env,E0),
  pushTypes(Tps,Lc,E0,ThEnv).

processNames(Dict,P,Result) :-
  processNames(Dict,P,[],Result).

processNames([],_,SoFar,SoFar).
processNames([scope(_,Names,_,_,_)|Outer],P,SoFar,Result) :-
  dict_pairs(Names,_,Pairs),
  procNames(Pairs,P,SoFar,S0),
  processNames(Outer,P,S0,Result).

procNames([],_,SoFar,SoFar).
procNames([K-vEntry(Vr,_)|More],P,SoFar,Result) :-
  call(P,K,Vr,SoFar,S0),
  procNames(More,P,S0,Result).

processTypes(Dict,P,Result) :-
  processTypes(Dict,P,[],Result).

processTypes([],_,SoFar,SoFar).
processTypes([scope(_,Names,_,_,_)|Outer],P,SoFar,Result) :-
  dict_pairs(Names,_,Pairs),
  procTypes(Pairs,P,SoFar,S0),
  processTypes(Outer,P,S0,Result).

procTypes([],_,SoFar,SoFar).
procTypes([K-V|More],P,SoFar,Result) :-
  call(P,K,V,SoFar,S0),
  procTypes(More,P,S0,Result).

makeKey(Id,Key) :-
  atom_string(Key,Id).

stdDict(Base) :-
  pushScope([],B),
  stdType("int",IntTp,ITpEx),
  stdType("string",StrTp,StpEx),
  stdType("float",FltTp,FtEx),
  declareType("string",tpDef(std,StrTp,StpEx),B,B0),
  declareType("integer",tpDef(std,IntTp,ITpEx),B0,B1),
  declareType("float",tpDef(std,FltTp,FtEx),B1,B2),
  stdType("list",LstTp,LTp),
  declareType("list",tpDef(std,LstTp,LTp),B2,Base).
