:- module(declmgt,[declareType/4,isType/3,
		declareTypeVars/4,isTypeVar/3,
		declareVr/6,
		declareEnum/6,declareCns/6,
		getVar/5,getVarTypeFace/4,varLoc/4,
		currentVar/3,
		declareContract/4,getContract/3,
		declareImplementation/5,
		getImplementation/4,
		manageConstraints/4,
		pushScope/2,mergeDict/4,pushFace/4,makeKey/2,stdDict/1,
		dispEnv/1,declareDecl/4,declareAllDecls/4,defineContract/5
	       ]).

:- use_module(canon).
:- use_module(display).
:- use_module(misc).
:- use_module(types).
:- use_module(dict).
:- use_module(escapes).
:- use_module(location).
:- use_module(errors).
:- use_module(unify).

declareAllDecls([],_,Env,Env).
declareAllDecls([D|More],Lc,Env,Evx) :-
%  reportMsg("declare %s",[dcl(D)]),
  declareDecl(Lc,D,Env,E0),!,
  declareAllDecls(More,Lc,E0,Evx).

declareDecl(Lc,impDec(ImplNm,FullNm,ImplTp),Ev,Evx) :-
  declareVr(Lc,FullNm,ImplTp,none,Ev,Ev0),
  declareImplementation(ImplNm,FullNm,ImplTp,Ev0,Evx).
declareDecl(_,accDec(Tp,Fld,FnNm,AccTp),Ev,Evx) :-
  declareFieldAccess(Tp,Fld,FnNm,AccTp,Ev,Evx).
declareDecl(_,updDec(Tp,Fld,FnNm,AccTp),Ev,Evx) :-
  declareFieldUpdater(Tp,Fld,FnNm,AccTp,Ev,Evx).
declareDecl(Lc,contractDec(Nm,CnNm,Rule),Ev,Evx) :-
  defineContract(Nm,Lc,conDef(Nm,CnNm,Rule),Ev,Evx).
declareDecl(Lc,typeDec(Nm,Tp,Rule),Env,Evx) :-
  declareType(Nm,tpDef(Lc,Tp,Rule),Env,Evx).
declareDecl(Lc,varDec(Nm,_FullNm,Tp),Env,Evx) :-
  declareVr(Lc,Nm,Tp,none,Env,Evx).
declareDecl(Lc,cnsDec(Nm,FullNm,Tp),Env,Evx) :-
  (isConType(Tp,0) ->
   declareEnum(Lc,Nm,FullNm,Tp,Env,Evx) ;
   declareCns(Lc,Nm,FullNm,Tp,Env,Evx)).
declareDecl(Lc,funDec(Nm,_FullNm,Tp),Env,Evx) :-
  declareVr(Lc,Nm,Tp,none,Env,Evx).
declareDecl(Lc,Entry,Env,Env) :-
  reportError("(internal) cannot figure out import entry %s",[Entry],Lc).

defineContract(N,Lc,Contract,E0,Ex) :-
  declareContract(N,Contract,E0,E1),
  declareMethods(Contract,Lc,E1,Ex).

declareMethods(conDef(_,_,ConEx),Lc,Env,Ev) :-
  moveQuants(ConEx,Q,C1),
  getConstraints(C1,Cx,contractExists(CTract,faceType(Methods,[]))),
  formMethods(Methods,Lc,Q,Cx,CTract,Env,Ev).

formMethods([],_,_,_,_,Env,Env).
formMethods([(Nm,Tp)|M],Lc,Q,Cx,Con,Env,Ev) :-
  moveQuants(Tp,FQ,CMTp),
  getConstraints(CMTp,_,QTp),
  merge(FQ,Q,MQ),
  putConstraints(Cx,constrained(QTp,Con),CC),
  moveQuants(MTp,MQ,CC),
  declareMtd(Lc,Nm,MTp,Env,E0),
  formMethods(M,Lc,Q,Cx,Con,E0,Ev).
