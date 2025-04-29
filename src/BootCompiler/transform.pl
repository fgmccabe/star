:- module(transform,[transformProg/4]).

/*
 * Implement a lambda lifting transformation to reduce (slightly) the semantics to
 * a flat language
 */

:- use_module(canon).
:- use_module(transutils).
:- use_module(errors).
:- use_module(types).
:- use_module(matcher).
:- use_module(misc).
:- use_module(escapes).
:- use_module(location).
:- use_module(freevars).
:- use_module(lterms).

/*
  Functions are converted to top-level functions with explicit parameters containing
  free variables. Theta records are converted to structures.

  E.g. add(X) => { a(Y)=>X+Y }.

  is converted to (assuming add is already top-level):

  add(X) => thetaXX(X)

  We add accessor functions to thetaXX:

  thetaXX('.a',Lbl) => thetaXX_A(Lbl)

  and individual functions are augmented with their theta parameter:

  aXX(Lbl,X) where thetaXX(Y) .= Lbl => X+Y

  Calls through variables access the closure form of functions:

  F(3,4) —> ocall(F,3,4,Tp)

  Calling a function from a theta combines:

  A = add(2)
  B = A.a(3)

  becomes
  A = add(2) (= thetaXX(2))

  ocall(ocall(A,'.a'),3,Tp)

*/

transformProg(PkgDecls,prog(pkg(Pkg,Vers),Imports,Decls,LDecls,Defs),
	      Opts,mdule(pkg(Pkg,Vers),Imports,Decls,LDecls,Dfs)) :-
  makePkgMap(PkgDecls,Map),
%  (is_member(traceNormalize,Opts) -> dispMap("Package map: ",0,Map);true),
  transformModuleDefs(Defs,Pkg,Map,Opts,Dfs,[]).

makePkgMap(PkgDecls,[lyr(VarMap,TpMap,ConsMap,void)]) :-
  stdDecl(Std),
  makeConstructorMap(Std,consMap{},StdMap),
  reverse(PkgDecls,Decls),  % declare imports first
  makeConstructorMap(Decls,StdMap,ConsMap),
  declareModuleGlobals(Std,ConsMap,varMap{},SVarMap,typeMap{},STpMap),!,
  declareModuleGlobals(Decls,ConsMap,SVarMap,VarMap,STpMap,TpMap),!.

declareModuleGlobals([Def|Rest],ConsMap,VMap,VMx,TMap,TMx) :-
  declMdlGlobal(Def,ConsMap,VMap,M0,TMap,TM0),
  declareModuleGlobals(Rest,ConsMap,M0,VMx,TM0,TMx).
declareModuleGlobals([],_,Map,Map,TMap,TMap).

declMdlGlobal(funDec(Nm,LclName,Tp),_,VMp,VMx,TMx,TMx) :-
  mangleName(LclName,closure,ClosureName),
  progTypeArity(Tp,Ar),
  declEntry(Nm,moduleFun(LclName,some(ClosureName),Ar,Tp),VMp,VMx).
declMdlGlobal(varDec(Nm,LclName,Tp),_,Mp,Mx,TMx,TMx) :-
  declEntry(Nm,moduleVar(LclName,Tp),Mp,Mx).
declMdlGlobal(cnsDec(_Nm,FullNm,Tp),_,Mp,Mx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  declEntry(FullNm,moduleCons(FullNm,Tp,Ar),Mp,Mx).
declMdlGlobal(typeDec(Nm,Tp,_),ConsMap,VMx,VMx,TMp,TMx) :-
  tpName(Tp,TpNm),
  findConsMap(TpNm,ConsMap,IxMap),!,
  declEntry(Nm,moduleType(TpNm,Tp,IxMap),TMp,TMx).
declMdlGlobal(accDec(_,_,AccName,Tp),_,VMp,VMx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  makeKey(AccName,Key),
  (get_dict(Key,VMp,_),VMx=VMp;
  mangleName(AccName,closure,ClosureName),
   declEntry(AccName,moduleFun(AccName,some(ClosureName),Ar,Tp),VMp,VMx)).
declMdlGlobal(updDec(_,_,AccName,Tp),_,VMp,VMx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  makeKey(AccName,Key),
  (get_dict(Key,VMp,_),VMx=VMp;
   mangleName(AccName,closure,ClosureName),
   declEntry(AccName,moduleFun(AccName,some(ClosureName),Ar,Tp),VMp,VMx)).
declMdlGlobal(_,_,Mx,Mx,TMx,TMx).

makeConstructorMap(Decls,CnMp,ConsMap) :-
  findAllConstructors(Decls,[],Cons),
  indexConstructors(Cons,CnMp,ConsMap).

findAllConstructors([],Cons,Cons) :-!.
findAllConstructors([cnsDec(Nm,FullNm,CnsTp)|Defs],Cons,Cnx) :-
  consTpName(CnsTp,TpNm),
  (concat(L1,[(TpNm,L)|L2],Cons) ->
   concat(L1,[(TpNm,[(Nm,FullNm,CnsTp)|L])|L2],C0) ;
   C0=[(TpNm,[(Nm,FullNm,CnsTp)])|Cons]),
  findAllConstructors(Defs,C0,Cnx).
findAllConstructors([_|Defs],Cons,Cnx) :-
  findAllConstructors(Defs,Cons,Cnx).

collectCons(Nm,TpNm,Cons,Cns) :-
  (concat(L1,[(TpNm,L)|L2],Cons) ->
   concat(L1,[(TpNm,[Nm|L])|L2],Cns) ;
   Cns=[(TpNm,[Nm])|Cons]).

indexConstructors([],Map,Map).
indexConstructors([(TpNm,L)|Cons],ConsMap,CnMx) :-
  map(L,transform:mkConsLbl,L0),
  sort(L0,transform:lblLt,L1),
  index_list(L1,0,SL),
  makeKey(TpNm,Key),
  put_dict(Key,ConsMap,SL,CnMp),
  indexConstructors(Cons,CnMp,CnMx).

mkConsLbl((_,Nm,Tp),lbl(Nm,Ar)) :-
  isConType(Tp,Ar),!.

lblLt(lbl(N1,_),lbl(N2,_)) :- str_lt(N1,N2).

declEntry(Nm,Entry,Map,Mpx) :-
  makeKey(Nm,Key),
  put_dict(Key,Map,Entry,Mpx).

findConsMap(Nm,ConsMap,IxMap) :-
  makeKey(Nm,Key),
  get_dict(Key,ConsMap,IxMap).

findConsIndex(TpNm,Nm,Map,Ix) :-
  makeKey(TpNm,Key),
  get_dict(Key,Map,Cons),!,
  is_member((Nm,Ix),Cons).

transformModuleDefs([],_,_,_,Ex,Ex).
transformModuleDefs([Def|Defs],Pkg,Map,Opts,Ex,Exx) :-
  transformMdlDef(Def,Pkg,Map,Opts,Ex,Ex1),!,
  transformModuleDefs(Defs,Pkg,Map,Opts,Ex1,Exx).

transformMdlDef(funDef(Lc,Nm,ExtNm,H,Tp,[],Eqns),_,Map,Opts,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,H,Tp,[],Eqns,Map,Opts,Dx,Dxx).
transformMdlDef(varDef(Lc,_Nm,ExtNm,[],Tp,Val),_Pkg,Map,Opts,Dx,Dxx) :-
  transformGlobal(Lc,ExtNm,Val,Tp,Map,Opts,Dx,Dxx).
transformMdlDef(typeDef(Lc,_Nm,Tp,Rl),_Pkg,Map,_,D,Dx) :-
  transformTypeDef(Lc,Tp,Rl,Map,D,Dx).
transformMdlDef(cnsDef(Lc,_Nm,enm(_,FullNm,Tp)),_Pkg,Map,_,D,Dx) :-
  transformConsDef(Lc,FullNm,Tp,Map,D,Dx).
transformMdlDef(conDef(_,_,_),_Pkg,_,_,Dxx,Dxx).

transformGlobal(Lc,ExtNm,Val,Tp,Map,Opts,[GlbDef|Dx],Dxx) :-
  (is_member(traceNormalize,Opts) -> dispDef(varDef(Lc,ExtNm,ExtNm,[],Tp,Val));true),
  liftExp(Val,Vl,[],_Q,Map,Opts,Dx,Dxx),
  GlbDef = glbDef(Lc,ExtNm,Tp,Vl),
  (is_member(traceNormalize,Opts) -> dispRuleSet(GlbDef);true).

extraArity(Arity,Vars,ExAr) :-
  length(Vars,E),
  ExAr is E+Arity.

transformTypeDef(Lc,Tp,Rl,Map,[typDef(Lc,Tp,Rl,IxMap)|Dx],Dx) :-
  tpName(Tp,TpNm),
  lookupTypeIndex(Map,TpNm,IxMap),!.
transformTypeDef(_Lc,_Tp,_Rl,_Map,Dx,Dx).

transformConsDef(Lc,Nm,Tp,Map,[lblDef(Lc,lbl(Nm,Ar),Tp,Ix)|Dx],Dx) :-
  consTpName(Tp,TpNm),
  lookupTypeIndex(Map,TpNm,IxMap),
  progTypeArity(Tp,Ar),
  is_member((lbl(Nm,_),Ix),IxMap).

transformFunction(Lc,Nm,LclName,H,Tp,Extra,Eqns,Map,Opts,[Fun|Ex],Exx) :-
  (is_member(traceNormalize,Opts) -> dispFunction(LclName,Tp,Eqns);true),
  progTypeArity(Tp,Arity),
  extraArity(Arity,Extra,Ar),
  extendFunTp(Tp,Extra,ATp),
  transformEquations(Map,Extra,Opts,Eqns,Rules,[],Ex,Ex0),
  (is_member(traceNormalize,Opts) -> dispEquations(Rules);true),
  closureEntry(Map,Lc,Nm,Tp,Extra,Opts,Ex0,Exx),
  functionMatcher(Lc,lbl(LclName,Ar),H,ATp,Rules,Map,Fun),!,
  (is_member(traceNormalize,Opts) -> dispRuleSet(Fun);true).

closureEntry(Map,Lc,Name,Tp,Extra,Opts,[ClEntry|L],L) :-
  lookupVar(Map,Name,Reslt),
  programAccess(Reslt,Prog,Closure),!,
  extendFunTp(Tp,Extra,ATp),
  realArgTypes(ATp,Tps),
  genVars(Tps,Args),
  length(Args,Ar),
  (Extra = [] ->
   genVar("ϕ",tplType([]),FrVr),
   Ar1 is Ar+1,
   ClEntry = fnDef(Lc,lbl(Closure,Ar1),hard,ATp,[FrVr|Args],cll(Lc,lbl(Prog,Ar),Args,ATp)) ;
   ClEntry = fnDef(Lc,lbl(Closure,Ar),hard,ATp,Args,cll(Lc,lbl(Prog,Ar),Args,ATp))),
  (is_member(traceNormalize,Opts) -> dispRuleSet(ClEntry);true).
closureEntry(_,_,_Name,_Tp,_,_,L,L).
%  reportMsg("no closure for %s:%s",[ss(Name),tpe(Tp)]).

extendFunTp(Tp,[],Tp):-!.
extendFunTp(funType(tplType(Els),Rt),Extra,funType(tplType(NEls),Rt)) :-!,
  extendTplTp(Extra,Anons),!,
  concat(Anons,Els,NEls).
extendFunTp(allType(V,T),Extra,allType(V,NT)) :-!,
  extendFunTp(T,Extra,NT).
extendFunTp(existType(V,T),Extra,existType(V,NT)) :-
  extendFunTp(T,Extra,NT).
extendFunTp(constrained(T,C),Extra,constrained(NT,C)) :-
  extendFunTp(T,Extra,NT).

extendTplTp([],[]).
extendTplTp([T|M],[Tp|NEls]) :-
  tipeOf(T,Tp),
  extendTplTp(M,NEls).

transformEquations(_,_,_,[],Rules,Rules,Ex,Ex).
transformEquations(Map,Extra,Opts,[Eqn|Defs],Rules,Rx,Ex,Exx) :-
  transformEqn(Eqn,Map,Extra,Opts,Rules,R0,Ex,Ex0),
  transformEquations(Map,Extra,Opts,Defs,R0,Rx,Ex0,Exx).

transformEqn(rule(Lc,A,G,Value),Map,Extra,Opts,[(Lc,Args,Test,Rep)|Rx],Rx,Ex,Exx) :-
  filterVars(Extra,Q0),
  ptnVars(A,Q0,Q0a),
  declarePtnVars(Q0a,Map,LMap),
  liftArgPtn(A,AA,Q0a,Q1,LMap,Opts,Ex,Ex0), % head args
  liftGuard(G,Test,Q1,Q2,LMap,Opts,Ex0,Ex1),
  concat(Extra,AA,Args),
  liftExp(Value,Rep,Q2,_Q3,LMap,Opts,Ex1,Exx). % replacement expression

liftGuard(none,none,Q,Q,_,_,Ex,Ex) :-!.
liftGuard(some(G),some(LG),Q,Qx,Map,Opts,Ex,Exx) :-
  liftGoal(G,LG,Q,Qx,Map,Opts,Ex,Exx).

transformLetDefs(_,_,_,_,[],Fx,Fx,Dfs,Dfs).
transformLetDefs(Map,OMap,Extra,Opts,[Def|Defs],F,Fx,Ex,Exx) :-
  transformLetDef(Def,Extra,Map,OMap,Opts,F,F1,Ex,Ex1),!,
  transformLetDefs(Map,OMap,Extra,Opts,Defs,F1,Fx,Ex1,Exx).

transformLetDef(funDef(Lc,Nm,ExtNm,H,Tp,_,Eqns),Extra,Map,_OMap,Opts,Fx,Fx,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,H,Tp,Extra,Eqns,Map,Opts,Dx,Dxx).
transformLetDef(varDef(_Lc,Nm,_LclNm,_,_Tp,Exp),_,Map,OMap,Opts,F,[(Nm,Ix,Rep)|F],Dx,Dxx) :-
  lookupVar(Map,Nm,labelArg(_,Ix,_ThVr,_)),
  liftExp(Exp,Rep,[],_Qx,OMap,Opts,Dx,Dxx).
transformLetDef(varDef(Lc,Nm,_LclNm,_,Tp,Exp),Extra,Map,OMap,Opts,F,F,Dx,Dxx) :-
  lookupVar(Map,Nm,thunkArg(ThVr,Lbl,Ix)),
  liftFreeThunk(Lc,Nm,Lbl,Tp,ThVr,Exp,Ix,Extra,OMap,Opts,Dx,Dxx).
transformLetDef(cnsDef(_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformLetDef(typeDef(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformLetDef(conDef(_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformLetDef(accDec(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformLetDef(updDec(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformLetDef(implDef(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).

liftArgPtn(tple(_Lc,Els),A,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(Els,A,Q,Qx,Map,Opts,Ex,Exx).

liftPtns([],[],Q,Q,_,_,Ex,Ex) :-!.
liftPtns([P|More],[A|Args],Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftPtns(More,Args,Q0,Qx,Map,Opts,Ex0,Exx).

liftPtn(v(Lc,Nm,Tp),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,Tp,A,Q,Qx,Map,Opts).
liftPtn(anon(_,Tp),ann(Tp),Q,Q,_,_,Ex,Ex).
liftPtn(enm(Lc,Nm,Tp),Ptn,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,Tp,Ptn,Q,Qx,Map,Opts).
liftPtn(void,voyd,Q,Q,_,_,Ex,Ex):-!.
liftPtn(intLit(_,Ix),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(bigLit(_,Ix),bigx(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(floatLit(_,Dx),float(Dx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(charLit(_,Sx),chr(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(stringLit(_,Sx),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(tple(_,Ptns),PTpl,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(Ptns,Ps,Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(Ps,PTpl).
liftPtn(apply(Lc,v(_,Nm,Tp),tple(_,A),_),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Tp,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(capply(Lc,enm(_,Nm,Tp),tple(_,A),_),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Tp,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(svGet(Lc,P,Tp),savGet(Lc,PP,Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,PP,Q,Qx,Map,Opts,Ex,Exx).
liftPtn(XX,whr(Lc,Vr,mtch(Lc,Vr,Val)),Q,Qx,Map,Opts,Ex,Exx) :-
  locOfCanon(XX,Lc),
  typeOfCanon(XX,Tp),
  genVar("ϕ",Tp,Vr),
  liftExp(XX,Val,Q,Qx,Map,Opts,Ex,Exx).

trVarPtn(_Lc,Nm,_,idnt(Nm,T),Q,Q,_Map,_Opts) :-
  is_member(idnt(Nm,T),Q),!.
trVarPtn(Lc,Nm,Tp,A,Q,Qx,Map,Opts) :-
  lookupVar(Map,Nm,V),!,
  implementVarPtn(V,Nm,Tp,Lc,A,Map,Opts,Q,Qx).

implementVarPtn(moduleVar(Vn,Tp),_,_,Lc,cll(Lc,lbl(Vn,0),[],Tp),_,_,Q,Q) :-
  reportError("not allowed to have globals in patterns: %s",[id(Vn)],Lc). % module variable

implementVarPtn(labelArg(N,Ix,ThVr,_),_,Tp,Lc,
		whr(Lc,N,mtch(Lc,N,nth(Lc,Vr,Ix,Tp))),Map,Opts,Q,Qx) :- !, % argument from label
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Q0),
  merge([N],Q0,Qx).
implementVarPtn(thunkArg(ThVr,Lbl,_),_,Tp,Lc,
		whr(Lc,N,mtch(Lc,N,cll(Lc,Lbl,[Vr],Tp))),Map,Opts,Q,Qx) :- !, % argument from label
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Q0),
  merge([N],Q0,Qx).
implementVarPtn(moduleCons(Enum,_,0),_,_,_,enum(Enum),_,_,Q,Q).
implementVarPtn(notInMap,Nm,Tp,_,idnt(Nm,Tp),_,_,Q,Qx) :-                 % variable local to rule
  merge([idnt(Nm,Tp)],Q,Qx).

trPtnCallOp(Lc,Nm,Tp,Args,whr(Lc,X,mtch(Lc,X,ecll(Lc,Nm,Args,Tp))),Q,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!,
  genVar("_X",Tp,X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Tp,Args,Ptn,Q,Qx,Map,Opts,Ex,Ex) :-
  lookupVar(Map,Nm,V),
  implementPtnCall(V,Lc,Tp,Args,Ptn,Map,Opts,Q,Qx).

implementPtnCall(localFun(Fn,_,Ar,ThVr,Tp),Lc,Tp,Args,
		 whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,A2),XArgs,Tp))),Map,Opts,Q,Qx) :-
  genVar("_X",Tp,X),
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx),
  concat(Args,[Vr],XArgs),
  merge([X],Q,Qx),
  A2 is Ar+1.
implementPtnCall(moduleFun(Fn,_,Ar,Tp),Lc,Tp,Args,
		 whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,Ar),Args,Tp))),_,_,Q,Qx) :-
  genVar("_X",Tp,X),
  merge([X],Q,Qx).
implementPtnCall(moduleCons(Mdl,_,Ar),_,_Tp,Args,ctpl(lbl(Mdl,Ar),Args),_,_,Q,Q).
implementPtnCall(localCons(Nm,_,Ar,ThV),_,_Tp,Args,ctpl(lbl(Nm,Arity),[ThV|Args]),_,_,Q,Q) :-
  Arity is Ar+1.

liftExps([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftExps([P|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftExps(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

liftExp(v(Lc,Nm,Tp),Vr,Q,Qx,Map,Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Tp,Vr,Map,Opts,Q,Qx).
liftExp(enm(Lc,Nm,Tp),Exp,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Tp,Exp,Map,Opts,Q,Qx).
liftExp(intLit(_,Ix),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(bigLit(_,Ix),bigx(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(floatLit(_,Dx),float(Dx),Q,Q,_,_,Ex,Ex) :-!.
liftExp(charLit(_,Cp),chr(Cp),Q,Q,_,_,Ex,Ex) :-!.
liftExp(stringLit(_,Sx),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftExp(tple(_,A),TApl,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,TA,[],Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(TA,TApl).
liftExp(open(_,E,_),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(apply(Lc,Op,tple(_,A),Tp),Call,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,LA,[],Q,Q1,Map,Opts,Ex,Ex1),
  typeOfCanon(Op,OTp),
  traceTrans(Opts,Lc,"implement call to %s:%s",[can(Op),tpe(OTp)]),
  trExpCallOp(Lc,Op,Tp,nothrow,LA,Call,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(capply(Lc,Op,tple(_,A),Tp),Call,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,LA,[],Q,Q1,Map,Opts,Ex,Ex1),
  traceTrans(Opts,Lc,"implement call to %s:%s",[can(Op),tpe(Tp)]),
  trExpCallOp(Lc,Op,Tp,nothrow,LA,Call,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(tdot(Lc,R,Ix,Tp),nth(Lc,Rc,Ix,Tp),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(R,Rc,Q,Qx,Map,Opts,Ex,Exx).
liftExp(case(Lc,Bnd,Cses,_),Result,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(Bnd,Bound,Q,Q0,Map,Opts,Ex,Ex0),
  liftCases(Cses,Cases,Q0,Qx,Map,Opts,transform:liftExp,Ex0,Exx),
  (idnt(_,_)=Bound ->
   caseMatcher(Lc,Bound,Cases,Map,Result) ;
   typeOfCanon(Bnd,Tp),
   genVar("_C",Tp,V),
   caseMatcher(Lc,V,Cases,Map,Res),
   Result = ltt(Lc,V,Bound,Res)).
liftExp(tryCatch(Lc,B,T,H),tryCtch(Lc,BB,TT,E,HH),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(T,TT,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(B,BB,Q0,Q1,Map,Opts,Ex0,Ex1),
  typeOfCanon(T,Tp),
  genVar("_E",Tp,E),
  liftCases(H,Cases,Q1,Qx,Map,Opts,transform:liftExp,Ex1,Exx),
  caseMatcher(Lc,E,Cases,Map,HH).
liftExp(try(Lc,B,ErTp,H),tryX(Lc,BB,E,HH),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(B,BB,Q,Q1,Map,Opts,Ex,Ex1),
  genVar("_E",ErTp,E),
  liftCases(H,Cases,Q1,Qx,Map,Opts,transform:liftExp,Ex1,Exx),
  caseMatcher(Lc,E,Cases,Map,HH).
liftExp(raise(Lc,T,E,_),rais(Lc,TT,EE),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(T,TT,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(E,EE,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(throw(Lc,E,_),thrw(Lc,EE),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftExp(pull(Lc,E,Tp,_),pul(Lc,EE,Tp),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftExp(push(Lc,E,Tp),psh(Lc,EE,Tp),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftExp(cell(Lc,In),cel(Lc,CellV),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(In,CellV,Q,Qx,Map,Opts,Ex,Exx).
liftExp(deref(Lc,In),get(Lc,CellV),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(In,CellV,Q,Qx,Map,Opts,Ex,Exx).
liftExp(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx),
  reportError("Unexpected %s in expression",[can(where(Lc,P,C))],Lc).
liftExp(G,Gl,Q,Qx,Map,Opts,Ex,Exx) :-
  isGoal(G),!,
  liftGoal(G,Gl,Q,Qx,Map,Opts,Ex,Exx).
liftExp(cond(Lc,T,L,R,_),cnd(Lc,LT,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(T,LT,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(L,LL,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(letExp(Lc,Decls,Defs,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
%  (is_member(traceNormalize,Opts) -> dispCanon(letExp(Lc,Decls,Defs,Bnd));true),
  liftLetExp(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(letRec(Lc,Decls,Defs,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetRecExp(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(lambda(Lc,Lbl,[],Rle,Tp),Rslt,Q,Q,Map,Opts,Ex,Exx) :-!,
  liftLambda(lambda(Lc,Lbl,[],Rle,Tp),Rslt,Q,Map,Opts,Ex,Exx).
liftExp(valof(Lc,A,_),vlof(Lc,Rslt),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftAction(A,Rslt,Q,Qx,Map,Opts,Ex,Exx).
liftExp(task(Lc,A,_),tsk(Lc,F),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(A,F,Q,Qx,Map,Opts,Ex,Exx).
liftExp(resume(Lc,T,M,Tp),resme(Lc,TT,MM,Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(T,TT,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(M,MM,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(suspend(Lc,T,M,Tp),susp(Lc,TT,MM,Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(T,TT,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(M,MM,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(retire(Lc,T,M,Tp),rtire(Lc,TT,MM,Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(T,TT,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(M,MM,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(thnkRef(Lc,E,Tp),ocall(Lc,EE,[],Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftExp(newSV(Lc,Tp),sav(Lc,Tp),Qx,Qx,_Map,_Opts,Exx,Exx).
liftExp(svGet(Lc,E,Tp),savGet(Lc,EE,Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftExp(svSet(Lc,E,V),savSet(Lc,EE,VV),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Ex0),
  liftExp(V,VV,Q,Qx,Map,Opts,Ex0,Exx).
liftExp(XX,void,Q,Q,_,_,Ex,Ex) :-
  locOfCanon(XX,Lc),
  reportFatal("internal: cannot transform %s as expression",[XX],Lc).

liftAction(doNop(Lc),nop(Lc),Q,Q,_,_,Ex,Ex) :-!.
liftAction(doSeq(Lc,L,R),seq(Lc,LL,RR),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftAction(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(R,RR,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(doLbld(Lc,Lb,A),lbld(Lc,Lb,AA),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftAction(A,AA,Q,Qx,Map,Opts,Ex,Exx).
liftAction(doBrk(Lc,Lb),brk(Lc,Lb),Qx,Qx,_Map,_Opts,Exx,Exx) :-!.
liftAction(doValis(Lc,E),vls(Lc,EE),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftAction(doDefn(Lc,P,E),defn(Lc,PP,EE),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftPtn(P,PP,Q,Q0,Map,Opts,Ex,Ex0), % simplified pattern
  liftExp(E,EE,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(doMatch(Lc,P,E),mtch(Lc,PP,EE),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftPtn(P,PP,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(E,EE,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(doAssign(Lc,P,E),asgn(Lc,PP,EE),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(P,PP,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(E,EE,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(doIfThenElse(Lc,G,L,R),iftte(Lc,GG,LL,RR),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftGoal(G,GG,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(L,LL,Q0,Qx,Map,Opts,Ex0,Ex1),
  liftAction(R,RR,Q,_,Map,Opts,Ex1,Exx).
liftAction(doWhile(Lc,G,B),whle(Lc,GG,BB),Q,Q,Map,Opts,Ex,Exx) :-!,
  liftGoal(G,GG,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(B,BB,Q0,_,Map,Opts,Ex0,Exx).
liftAction(doLet(Lc,Decls,Defs,B),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
%  (is_member(traceNormalize,Opts) -> dispAction(doLet(Lc,Decls,Defs,B));true),
  genVar("_ThAL",Tp,ThVr),
  letActionMap(Lc,Decls,Defs,B,ThVr,Q,Map,Opts,ThMap,RMap,FreeTerm),
  tipeOf(FreeTerm,Tp),
  transformLetDefs(ThMap,RMap,[ThVr],Opts,Defs,[],Fx,Ex,Ex1),
  liftAction(B,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeActionLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp).
%  (is_member(traceNormalize,Opts) -> dispAct(Exp);true).
liftAction(doLetRec(Lc,Decls,Defs,B),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
%  (is_member(traceNormalize,Opts) -> dispAction(doLetRec(Lc,Decls,Defs,B));true),
  genVar("_ThRA",Tp,ThVr),
  letRecActionMap(Lc,Decls,Defs,B,ThVr,Q,Map,Opts,ThMap,FreeTerm),
  tipeOf(FreeTerm,Tp),
  transformLetDefs(ThMap,ThMap,[ThVr],Opts,Defs,[],Fx,Ex,Ex1),
  liftAction(B,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeActionLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp).
%  (is_member(traceNormalize,Opts) -> dispAct(Exp);true).
liftAction(doCase(Lc,B,Cs,_),Reslt,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(B,BB,Q,Q0,Map,Opts,Ex,Ex0),
  liftCases(Cs,Cases,Q0,Qx,Map,Opts,transform:liftAction,Ex0,Exx),
  (idnt(_,_)=BB ->
   actionCaseMatcher(Lc,BB,Cases,Map,Reslt);
   typeOfCanon(B,Tp),
   genVar("_C",Tp,V),
   actionCaseMatcher(Lc,V,Cases,Map,Rslt),
   Reslt = seq(Lc,defn(Lc,V,BB),Rslt)).
liftAction(doTryCatch(Lc,B,T,H),doTryCtch(Lc,BB,TT,E,HH),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(T,TT,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(B,BB,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftCases(H,Cases,Q1,Qx,Map,Opts,transform:liftAction,Ex1,Exx),
  typeOfCanon(T,Tp),
  genVar("_E",Tp,E),
  actionCaseMatcher(Lc,E,Cases,Map,HH).
liftAction(doTry(Lc,B,ErTp,H),aTry(Lc,BB,E,HH),Q,Qx,Map,Opts,Ex,Exx) :-
  liftAction(B,BB,Q,Q1,Map,Opts,Ex,Ex1),
  liftCases(H,Cases,Q1,Qx,Map,Opts,transform:liftAction,Ex1,Exx),
  genVar("_E",ErTp,E),
  actionCaseMatcher(Lc,E,Cases,Map,HH).
liftAction(doThrow(Lc,E),aThrow(Lc,EE),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftAction(doPull(Lc,E,_,_),aPull(Lc,EE),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftAction(doPush(Lc,E,_),aPush(Lc,EE),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,EE,Q,Qx,Map,Opts,Ex,Exx).
liftAction(doExp(Lc,E),perf(Lc,Exp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftAction(XX,nop(Lc),Q,Q,_,_,Ex,Ex) :-!,
  locOfCanon(XX,Lc),
  reportFatal("internal: cannot transform %s as action",[cnact(XX)],Lc).
  
liftLetExp(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftLet(Lc,Decls,Defs,Bnd,transform:liftExp,Exp,Q,Qx,Map,Opts,Ex,Exx).

liftLet(Lc,Decls,Defs,Bnd,Cll,Exp,Q,Q,Map,Opts,Ex,Exx) :-
  genVar("_ThL",Tp,ThVr),
  letMap(Lc,Decls,Defs,Bnd,ThVr,Q,QL,Map,Opts,ThMap,RMap,FreeTerm),
  (is_member(traceNormalize,Opts) ->
     reportMsg("Free term %s",[ltrm(FreeTerm)]),
     dispMap("Let map: ",1,ThMap);true),
  tipeOf(FreeTerm,Tp),
  transformLetDefs(ThMap,RMap,[ThVr],Opts,Defs,[],Fx,Ex,Ex1),
  call(Cll,Bnd,BExpr,QL,_Qx,ThMap,Opts,Ex1,Exx),
  mkFreeLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp),
  (is_member(traceNormalize,Opts) -> dispTerm(Exp);true).

liftLetRecExp(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetRec(Lc,Decls,Defs,transform:liftExp,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).

liftLetRec(Lc,Decls,Defs,Cll,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  genVar("_ThV",Tp,ThVr),
  letRecMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,ThMap,FreeTerm,_CellVars),
  tipeOf(FreeTerm,Tp),
  (is_member(traceNormalize,Opts) ->
     reportMsg("Free term %s",[ltrm(FreeTerm)],Lc),
     dispMap("Letrec map: ",1,ThMap);true),
  transformLetDefs(ThMap,ThMap,[ThVr],Opts,Defs,[],Fx,Ex,Ex1),
  call(Cll,Bnd,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp),
  (is_member(traceNormalize,Opts) ->
   reportMsg("lifted let rec %s",[ltrm(Exp)],Lc);true).

mkFreeLet(Lc,Vr,Fr,Ups,Exp,AExp) :-
  computeFreeVect(Lc,Vr,Fr,Ups,transform:combineUpdate,Exp,AExp).

mkFreeActionLet(Lc,Vr,Fr,Ups,Exp,AExp) :-
  computeFreeVect(Lc,Vr,Fr,Ups,transform:combineActionUpdate,Exp,AExp).

combineUpdate(Lc,Vr,Ix,Term,SoFar,seqD(Lc,setix(Lc,Vr,Ix,Term),SoFar)).

combineActionUpdate(Lc,Vr,Ix,Term,SoFar,seq(Lc,setix(Lc,Vr,Ix,Term),SoFar)).

computeFreeVect(Lc,Vr,Fr,[],_,Exp,ltt(Lc,Vr,Fr,Exp)).
computeFreeVect(Lc,Vr,ctpl(Lbl,Args),[(_Nm,Ix,Term)|Ups],Update,Exp,Reslt) :-
  \+idInTerm(Vr,Term),!,
  split_list(Ix,Args,F,[_|R]),
  concat(F,[Term|R],NArgs),
  computeFreeVect(Lc,Vr,ctpl(Lbl,NArgs),Ups,Update,Exp,Reslt).
computeFreeVect(Lc,Vr,Fr,[(_,Ix,Term)|Ups],Update,Exp,Reslt) :-
  call(Update,Lc,Vr,Ix,Term,Exp,SoFar),
  computeFreeVect(Lc,Vr,Fr,Ups,Update,SoFar,Reslt).
  
trVarExp(Lc,Nm,Tp,Exp,Map,Opts,Q,Qx) :-
  lookupVar(Map,Nm,V),!,
  implementVarExp(V,Lc,Nm,Tp,Exp,Map,Opts,Q,Qx),!.
trVarExp(Lc,Nm,Tp,ann(Tp),_,_,Q,Q) :-
  reportError("%s not defined",[id(Nm)],Lc).

liftVar(_,Vr,Vr,Map,_Opts,Q,Qx):-
  thisVar(Map,Vr),!,
  merge([Vr],Q,Qx).
liftVar(Lc,idnt(Nm,Tp),Vr,Map,Opts,Q,Qx) :-
  trVarExp(Lc,Nm,Tp,Vr,Map,Opts,Q,Qx).

implementVarExp(moduleVar(V,_),_Lc,_,Tp,idnt(V,Tp),_,_,Qx,Qx).
implementVarExp(labelArg(_N,Ix,ThVr,_),Lc,_,Tp,nth(Lc,ThV,Ix,Tp),Map,Opts,Q,Qx) :-
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementVarExp(thunkArg(ThVr,Lbl,_),Lc,_,Tp,cll(Lc,Lbl,[ThV],Tp),Map,Opts,Q,Qx) :-
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementVarExp(moduleCons(Enum,_,0),_,_,_,enum(Enum),_,_,Q,Q).
implementVarExp(moduleCons(C,_,Ar),_,_,_,Cns,_,_,Q,Q) :-
  trCons(C,Ar,Cns).
implementVarExp(notInMap,_,Nm,Tp,idnt(Nm,Tp),_,_,Q,Qx) :-
  merge([idnt(Nm,Tp)],Q,Qx).
implementVarExp(moduleFun(_,some(Closure),Ar,Tp),_,_,_,clos(Closure,Ar1,Unit,Tp),_,_,Q,Q) :-
  Closure\==void,
  Ar1 is Ar+1,
  mkTpl([],Unit).
implementVarExp(localFun(_Fn,Closure,Ar,ThVr,Tp),Lc,_,_,clos(Closure,Ar1,Vr,Tp),Map,Opts,Q,Qx) :-
  Ar1 is Ar+1,
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx).
implementVarExp(_Other,Lc,Nm,Tp,idnt(Nm,Tp),_,_,Q,Q) :-
  reportError("cannot handle %s in expression",[id(Nm)],Lc).

trExpCallOp(Lc,throwing(_,Op,ErTp),Tp,nothrow,Args,Call,Q,Qx,Map,Opts,Ex,Exx) :-
  trExpCallOp(Lc,Op,Tp,throw(ErTp),Args,Call,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,v(_,Nm,_),Tp,nothrow,Args,ecll(Lc,Nm,Args,Tp),Qx,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!.
trExpCallOp(Lc,v(_,Nm,_),Tp,throw(ErTp),Args,xecll(Lc,Nm,Args,Tp,ErTp),Qx,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!.
trExpCallOp(Lc,v(_,Nm,_),Tp,Throwing,Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  lookupVar(Map,Nm,Reslt),
  Reslt\=notInMap,!,
  implementFunCall(Lc,Reslt,Nm,Args,Tp,Throwing,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(_Lc,enm(Lc,Nm,_Tp),_,_,Args,Exp,Qx,Qx,Map,_Opts,Exx,Exx) :-
  lookupVar(Map,Nm,Reslt),
  (Reslt = moduleCons(Mdl,_,Ar) ->
   Exp = ctpl(lbl(Mdl,Ar),Args);
   Reslt = localCons(_,_,Ar,ThV) ->
   Arity is Ar+1,
   Exp = ctpl(lbl(Nm,Arity),[ThV|Args]);
   reportError("cannot handle constructor %s",[id(Nm)],Lc)).
trExpCallOp(Lc,Op,Tp,nothrow,A,ocall(Lc,Rc,A,Tp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Op,Rc,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,Op,Tp,throw(ErTp),A,xocall(Lc,Rc,A,Tp,ErTp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Op,Rc,Q,Qx,Map,Opts,Ex,Exx).

implementFunCall(Lc,localFun(Fn,_,Ar,ThVr,_),_,Args,Tp,nothrow,cll(Lc,lbl(Fn,Ar2),XArgs,Tp),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx),
  concat([Vr],Args,XArgs),
  Ar2 is Ar+1.
implementFunCall(Lc,localFun(Fn,_,Ar,ThVr,_),_,Args,Tp,throw(ErTp),xcll(Lc,lbl(Fn,Ar2),XArgs,Tp,ErTp),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx),
  concat([Vr],Args,XArgs),
  Ar2 is Ar+1.
implementFunCall(Lc,moduleFun(Fn,_,Ar,Tp),_,Args,_Tp,nothrow,
		 cll(Lc,lbl(Fn,Ar),Args,Tp),Qx,Qx,_,_,Ex,Ex).
implementFunCall(Lc,moduleFun(Fn,_,Ar,Tp),_,Args,_Tp,throw(ErTp),
		 xcll(Lc,lbl(Fn,Ar),Args,Tp,ErTp),Qx,Qx,_,_,Ex,Ex).
implementFunCall(Lc,moduleVar(Fn,Tp),_,Args,RTp,nothrow,
		 ocall(Lc,idnt(Fn,Tp),Args,RTp),Qx,Qx,_,_,Ex,Ex).
implementFunCall(Lc,moduleVar(Fn,Tp),_,Args,RTp,throw(ErTp),
		 xocall(Lc,idnt(Fn,Tp),Args,RTp,ErTp),Qx,Qx,_,_,Ex,Ex).
implementFunCall(Lc,thunkArg(ThVr,Lbl,_),_,Args,Tp,nothrow,
		 ocall(Lc,cll(Lc,Lbl,[ThV],OTp),Args,Tp),Q,Qx,Map,Opts,Ex,Ex) :-
  tipeOf(ThVr,ATp),
  OTp = funType(tplType([ATp]),Tp),
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementFunCall(Lc,thunkArg(ThVr,Lbl,_),_,Args,Tp,throw(ErTp),
		 xocall(Lc,cll(Lc,Lbl,[ThV],OTp),Args,Tp,ErTp),Q,Qx,Map,Opts,Ex,Ex):-
  tipeOf(ThVr,ATp),
  OTp = funType(tplType([ATp]),Tp),
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementFunCall(Lc,labelArg(_,Ix,ThVr,OTp),_,Args,Tp,nothrow,
		 ocall(Lc,nth(Lc,ThV,Ix,OTp),Args,Tp),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementFunCall(Lc,labelArg(_,Ix,ThVr,OTp),_,Args,Tp,throw(ErTp),
		 xocall(Lc,nth(Lc,ThV,Ix,OTp),Args,Tp,ErTp),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementFunCall(Lc,notInMap,Nm,Args,Tp,_,ocall(Lc,idnt(Nm,Tp),Args,Tp),Q,Q,_Map,_Opts,Ex,Ex) :-
  reportError("cannot compile unknown function %s",[id(Nm)],Lc).

liftCases([],[],Qx,Qx,_Map,_Opts,_,Exx,Exx) :- !.
liftCases([C|Cses],[Case|Cases],Q,Qx,Map,Opts,Lifter,Ex,Exx) :-
  liftCase(C,Case,Q,Q0,Map,Opts,Lifter,Ex,Ex0),
  liftCases(Cses,Cases,Q0,Qx,Map,Opts,Lifter,Ex0,Exx).

liftCase(rule(Lc,P,G,Value),(Lc,[Ptn],Test,Rep),Q,Qx,Map,Opts,Lifter,Ex,Exx) :-
  liftPtn(P,Ptn,Q,Q0,Map,Opts,Ex,Ex0),
  liftGuard(G,Test,Q0,Q1,Map,Opts,Ex0,Ex1), % condition goals
  call(Lifter,Value,Rep,Q1,Qx,Map,Opts,Ex1,Exx).

liftLambda(lambda(Lc,LamLbl,Cx,Eqn,Tp),clos(LamLbl,Ar,FreeTerm,Tp),Q,Map,Opts,[LamFun|Ex],Exx) :-
  progTypeArity(Tp,Ar0),
  Ar is Ar0+1,
  (is_member(traceNormalize,Opts) -> dispCanon(lambda(Lc,LamLbl,Cx,Eqn,Tp));true),
  lambdaMap(lambda(Lc,LamLbl,Cx,Eqn,Tp),ThVr,Q,Map,Opts,FreeTerm,LMap),
  (is_member(traceNormalize,Opts) ->
   reportMsg("Free term %s",[ltrm(FreeTerm)]),
   dispMap("Lambda map: ",1,LMap);true),
  transformEqn(Eqn,LMap,[ThVr],Opts,Rls,[],Ex,Exx),
  functionMatcher(Lc,lbl(LamLbl,Ar),hard,Tp,Rls,Map,LamFun),
  (is_member(traceNormalize,Opts) ->
   reportMsg("Lifted Lambda: %s",[ldef(LamFun)],Lc);
   true).

liftGoal(Cond,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftGl(Cond,Exp,Q,Qx,Map,Opts,Ex,Exx).

liftGl(conj(Lc,L,R),cnj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGl(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftGl(R,LR,Q0,Qx,Map,Opts,Ex0,Exx).
liftGl(disj(Lc,L,R),dsj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGl(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftGl(R,LR,Q0,Qx,Map,Opts,Ex0,Exx).
liftGl(cond(Lc,T,L,R,_),cnd(Lc,LT,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGl(T,LT,Q,Q0,Map,Opts,Ex,Ex0),
  liftGl(L,LL,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftGl(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftGl(implies(Lc,G,T),Gl,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftGl(neg(Lc,conj(Lc,G,neg(Lc,T))),Gl,Q,Qx,Map,Opts,Ex,Exx).
liftGl(match(Lc,L,R),mtch(Lc,Lx,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftPtn(L,Lx,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(R,Rx,Q0,Qx,Map,Opts,Ex0,Exx).
liftGl(neg(Lc,R),ng(Lc,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGl(R,Rx,Q,Qx,Map,Opts,Ex,Exx).
liftGl(G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(G,Gx,Q,Qx,Map,Opts,Ex,Exx).

cmpName((N1,_),(N2,_)) :- str_lt(N1,N2).

liftEls([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftEls([(_,P)|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftEls(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

letRecMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,[lyr(Vx,Tx,ConsMap,ThVr)|Map],FreeTerm,CellVars) :-
  findFreeVars(letRec(Lc,Decls,Defs,Bnd),Map,Q,ThFree),
  makeConstructorMap(Decls,consMap{},ConsMap),
  collectThunkVars(Defs,ThVr,0,FIx,varMap{},V0,CellVars),
  collectLabelVars(ThFree,ThVr,FIx,_,V0,V1),
  declareThetaDefs(Decls,ThVr,ConsMap,V1,Vx,typeMap{},Tx),
  makeFreeTerm(CellVars,Lc,ThFree,Map,Opts,FreeTerm).

letMap(Lc,Decls,Defs,Bnd,ThVr,Q,Qx,Map,Opts,
       [lyr(Vx,Tx,ConsMap,ThVr)|Map],[lyr(varMap{},Tx,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVars(letExp(Lc,Decls,Defs,Bnd),Map,Q,ThFree),
  varDefs(Defs,CellVars),
  concat(CellVars,ThFree,FreeVars),
  merge(CellVars,Q,Qx),
  makeConstructorMap(Decls,consMap{},ConsMap),
  collectLabelVars(FreeVars,ThVr,0,_,varMap{},V0),
  declareThetaDefs(Decls,ThVr,ConsMap,V0,Vx,typeMap{},Tx),
  makeFreeTerm([],Lc,FreeVars,Map,Opts,FreeTerm).

letActionMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,
       [lyr(Vx,Tx,ConsMap,ThVr)|Map],[lyr(varMap{},Tx,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVarsInAction(doLet(Lc,Decls,Defs,Bnd),Map,Q,ThFree),
  varDefs(Defs,CellVars),
  concat(CellVars,ThFree,FreeVars),
  makeConstructorMap(Decls,consMap{},ConsMap),
  declareThetaDefs(Decls,ThVr,ConsMap,varMap{},V0,typeMap{},Tx),
  collectLabelVars(FreeVars,ThVr,0,_,V0,Vx),
  makeFreeTerm([],Lc,FreeVars,Map,Opts,FreeTerm).

letRecActionMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,[lyr(Vx,Tx,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVarsInAction(doLetRec(Lc,Decls,Defs,Bnd),Map,Q,ThFree),
  makeConstructorMap(Decls,consMap{},ConsMap),

  collectThunkVars(Defs,ThVr,0,FIx,varMap{},V0,CellVars),
  collectLabelVars(ThFree,ThVr,FIx,_,V0,V1),
  declareThetaDefs(Decls,ThVr,ConsMap,V1,Vx,typeMap{},Tx),
  makeFreeTerm(CellVars,Lc,ThFree,Map,Opts,FreeTerm).

declarePtnVars(Q,[lyr(Vars,Tx,Cons,ThVr)|Map],[lyr(NVars,Tx,Cons,ThVr)|Map]) :-
  rfold(Q,transform:declVr,Vars,NVars).

declVr(idnt(Nm,_),Vars,Vx) :-
  declEntry(Nm,notInMap,Vars,Vx).
declVr(Vars,Vars).

lambdaMap(Lam,ThVr,Q,Map,Opts,FreeTerm,[lyr(Vx,typeMap{},consMap{},ThVr)|Map]) :-
  findFreeVars(Lam,Map,Q,LmFree),
  map(LmFree,lterms:tipeOf,FrTps),
  genVar("_ΛV",tplType(FrTps),ThVr),
  collectLabelVars(LmFree,ThVr,0,_,varMap{},Vx),
  locOfCanon(Lam,Lc),
  makeFreeTerm([],Lc,LmFree,Map,Opts,FreeTerm).

findFreeVars(Exp,Map,_Q,LmFr0) :-
  definedProgs(Map,Df),
  freeVars(Exp,Df,transform:addThFree,[],ThFr),
  freeLabelVars(ThFr,Map,[],LmFr0),!.

findFreeVarsInAction(Exp,Map,_Q,LmFr0) :-
  definedProgs(Map,Df),
  freeVarsInAction(Exp,Df,transform:addThFree,[],ThFr),
  freeLabelVars(ThFr,Map,[],LmFr0),!.

addThFree(Ex,Nm,Tp,Fr,Fx) :-
  \+ is_member(idnt(Nm,_),Ex),
  add_mem(idnt(Nm,Tp),Fr,Fx).
addThFree(_,_,Fr,Fr).

freeLabelVars([],_,Fr,Fr).
freeLabelVars([idnt(Nm,_)|Lv],Map,Fr,LmFr) :-
  lookupThetaVar(Map,Nm,ThVr),!,
  merge([ThVr],Fr,Fr1),
  freeLabelVars([ThVr|Lv],Map,Fr1,LmFr).
freeLabelVars([idnt(Nm,_)|Lv],Map,Fr,LmFr) :-
  isModuleVar(Map,Nm),!,
  freeLabelVars(Lv,Map,Fr,LmFr).
freeLabelVars([V|Lv],Map,Fr,LmFr) :-
  merge([V],Fr,Fr1),
  freeLabelVars(Lv,Map,Fr1,LmFr).

isModuleVar(Map,Nm) :-
  lookupVar(Map,Nm,R),
  (R=moduleFun(_,_,_,_) ; R=moduleVar(_,_)),!.

varDefs(Defs,Vars) :-
  rfold(Defs,transform:isVarDef,[],Vars).

isVarDef(varDef(_,Nm,_LclNm,_,Tp,_),F,Fv) :-
  add_mem(idnt(Nm,Tp),F,Fv).
isVarDef(_,Fv,Fv).

makeFreeTerm(CellVars,Lc,ThFr,Map,Opts,FreeTerm) :-
  map(CellVars,transform:emptyCell(Lc),CV),
  map(ThFr,transform:mkFreeVar(Map,Opts,Lc),CFr),
  concat(CV,CFr,Args),
  mkTpl(Args,FreeTerm).

emptyCell(Lc,idnt(_,Tp),sav(Lc,STp)) :-
  savType(Tp,STp).

mkFreeVar(Map,Opts,Lc,idnt(Nm,Tp),Vr) :-
  trVarExp(Lc,Nm,Tp,Vr,Map,Opts,[],_).

refineQ(Q,Qx) :-
  filter(Q,transform:notVar,Qx).

filterVars(Q,Qx) :-
  filterVars(Q,[],Qx).
filterVars(V,Q,Qx) :-
  rfold(V,transform:filterVar,Q,Qx).

filterVar(T,Q,Qx) :-
  isIdnt(T),
  add_mem(T,Q,Qx).
filterVar(ctpl(_,A),Q,Qx) :-
  filterVars(A,Q,Qx).
filterVar(enum(_),Q,Q).

notVar(V) :- V\=idnt(_,_).

isIdnt(idnt(_,_)).

declareThetaDefs([],_,_,Vx,Vx,Tx,Tx).
declareThetaDefs([D|Ds],ThVr,ConsMap,V,Vx,T,Tx) :-
  declareThetaVar(D,ThVr,ConsMap,V,V0,T,T0),!,
  declareThetaDefs(Ds,ThVr,ConsMap,V0,Vx,T0,Tx).

declareThetaVar(funDec(Nm,LclName,Tp),ThV,_,V,Vx,Tx,Tx) :-
  mangleName(LclName,closure,ClosureName),
  progTypeArity(Tp,Ar),
  declEntry(Nm,localFun(LclName,ClosureName,Ar,ThV,Tp),V,Vx).
declareThetaVar(varDec(_Nm,_LclName,_Tp),_ThV,_,Vx,Vx,Tx,Tx).
declareThetaVar(cnsDec(_Nm,LclName,Tp),_ThV,_,V,Vx,Tx,Tx) :-
  progTypeArity(Tp,Ar),
  declEntry(LclName,moduleCons(LclName,Tp,Ar),V,Vx).
declareThetaVar(typeDec(Nm,Tp,_),_,ConsMap,Vx,Vx,T,Tx) :-
  tpName(Tp,TpNm),
  findConsMap(TpNm,ConsMap,IxMap),!,
  declEntry(Nm,localType(TpNm,Tp,IxMap),T,Tx).
declareThetaVar(accDec(_,_,AccName,Tp),ThV,_,V,Vx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  mangleName(AccName,closure,ClosureName),
  declEntry(AccName,localFun(AccName,ClosureName,Ar,ThV,Tp),V,Vx).
declareThetaVar(updDec(_,_,AccName,Tp),ThV,_,V,Vx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  mangleName(AccName,closure,ClosureName),
  declEntry(AccName,localFun(AccName,ClosureName,Ar,ThV,Tp),V,Vx).
declareThetaVar(_,_,_,Vx,Vx,Tx,Tx).

collectLabelVars([],_,Ix,Ix,List,List).
collectLabelVars([idnt(Nm,Tp)|Args],ThVr,Ix,Ixx,List,Lx) :-
  Ix1 is Ix+1,
  declEntry(Nm,labelArg(idnt(Nm,Tp),Ix,ThVr,Tp),List,L0),
  collectLabelVars(Args,ThVr,Ix1,Ixx,L0,Lx).
collectLabelVars([_|Args],ThVr,Ix,Ixx,List,Lx) :-
  collectLabelVars(Args,ThVr,Ix,Ixx,List,Lx).

collectThunkVars([],_,Ix,Ix,List,List,[]).
collectThunkVars([varDef(_Lc,Nm,LclNm,_,Tp,_Vl)|Defs],ThVr,Ix,Ixx,List,Lx,[idnt(Nm,Tp)|XVars]) :-
  Ix1 is Ix+1,
  declEntry(Nm,thunkArg(ThVr,lbl(LclNm,1),Ix),List,L0),
  collectThunkVars(Defs,ThVr,Ix1,Ixx,L0,Lx,XVars).
collectThunkVars([_|Defs],ThVr,Ix,Ixx,List,Lx,XVars) :-
  collectThunkVars(Defs,ThVr,Ix,Ixx,List,Lx,XVars).

liftFreeThunk(Lc,Nm,LclNm,Tp,ThVr,Exp,Fx,Q,Map,Opts,[ThDf|Ex],Exx) :-
  tipeOf(ThVr,FrTp),
  liftExp(Exp,VExp,Q,_,Map,Opts,Ex,Exx),
  savType(Tp,STp),
  Vr = idnt(Nm,STp),
  genVar("ϕ",Tp,SvVl),
  ThDf = fnDef(Lc,LclNm,hard,funType(tplType([FrTp]),Tp),
	       [ThVr],
	       vlof(Lc,seq(Lc,
			   defn(Lc,Vr,nth(Lc,ThVr,Fx,Tp)),
			   iftte(Lc,mtch(Lc,savGet(Lc,SvVl,Tp),Vr),
				 vls(Lc,SvVl),
				 vls(Lc,savSet(Lc,Vr,VExp)))))).

programAccess(moduleFun(Prog,some(Closure),_Arity,_Tp),Prog,Closure).
programAccess(localFun(Prog,Closure,_Arity,_,_),Prog,Closure).

traceTrans(Opts,Lc,Msg,Args) :-
  is_member(traceNormalize,Opts),!,
  reportMsg(Msg,Args,Lc).
traceTrans(_,_,_,_).
