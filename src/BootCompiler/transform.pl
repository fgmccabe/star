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
:- use_module(intrinsics).
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

  F(3,4) —> ocall(F,3,4)

  Calling a function from a theta combines:

  A = add(2)
  B = A.a(3)

  becomes
  A = add(2) (= thetaXX(2))

  ocall(ocall(A,'.a'),3)

*/

transformProg(PkgDecls,prog(pkg(Pkg,Vers),Imports,Decls,LDecls,Defs),
	      Opts,mdule(pkg(Pkg,Vers),Imports,Decls,LDecls,Dfs)) :-
  makePkgMap(Pkg,PkgDecls,Map),
%  (is_member(showTrCode,Opts) -> dispMap("Package map: ",0,Map);true),
  transformModuleDefs(Defs,Pkg,Map,Opts,Dfs,[]).

makePkgMap(Pkg,PkgDecls,[lyr(VarMap,TpMap,ConsMap,void)]) :-
  makeConstructorMap(PkgDecls,consMap{},ConsMap),
  declareModuleGlobals(Pkg,PkgDecls,ConsMap,varMap{},VarMap,typeMap{},TpMap),!.

declareModuleGlobals(Pkg,[Def|Rest],ConsMap,VMap,VMx,TMap,TMx) :-
  declMdlGlobal(Pkg,Def,ConsMap,VMap,M0,TMap,TM0),
  declareModuleGlobals(Pkg,Rest,ConsMap,M0,VMx,TM0,TMx).
declareModuleGlobals(_,[],_,Map,Map,TMap,TMap).

declMdlGlobal(_Pkg,funDec(Nm,LclName,Tp),_,VMp,VMx,TMx,TMx) :-
  localName(LclName,closure,ClosureName),
  progTypeArity(Tp,Ar),
  declEntry(Nm,moduleFun(LclName,some(ClosureName),Ar),VMp,VMx).
declMdlGlobal(_Pkg,varDec(Nm,LclName,_),_,Mp,Mx,TMx,TMx) :-
  declEntry(Nm,moduleVar(LclName),Mp,Mx).
declMdlGlobal(_Pkg,cnsDec(_Nm,FullNm,Tp),_,Mp,Mx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  declEntry(FullNm,moduleCons(FullNm,Tp,Ar),Mp,Mx).
declMdlGlobal(_Pkg,typeDec(Nm,Tp,_,_),ConsMap,VMx,VMx,TMp,TMx) :-
  tpName(Tp,TpNm),
  findConsMap(TpNm,ConsMap,IxMap),!,
  declEntry(Nm,moduleType(TpNm,Tp,IxMap),TMp,TMx).
declMdlGlobal(_,accDec(_,_,AccName,Tp),_,VMp,VMx,TMx,TMx) :-
  progTypeArity(Tp,Ar),
  makeKey(AccName,Key),
  (get_dict(Key,VMp,_),VMx=VMp;
   declEntry(AccName,moduleFun(AccName,none,Ar),VMp,VMx)).

declMdlGlobal(_,_,_,Mx,Mx,TMx,TMx).

contractArity(allType(_,Con),Ar) :- contractArity(Con,Ar).
contractArity(constrained(Con,_),Ar) :- contractArity(Con,A), Ar is A+1.
contractArity(contractExists(_,_),0).

contractStruct(0,Nm,enum(Nm)).
contractStruct(Ar,Nm,lbl(Nm,Ar)).

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
transformMdlDef(cnsDef(Lc,Nm,Tp),_Pkg,Map,_,D,Dx) :-
  transformConsDef(Lc,Nm,Tp,Map,D,Dx).

transformGlobal(Lc,ExtNm,Val,Tp,Map,Opts,[glbDef(Lc,ExtNm,Tp,Vl)|Dx],Dxx) :-
  (is_member(showTrCode,Opts) -> dispDef(varDef(Lc,ExtNm,ExtNm,[],Tp,Val));true),
  liftExp(Val,Vl,[],_Q3,Map,Opts,Dx,Dxx), % replacement expression
  (is_member(showTrCode,Opts) -> dispRuleSet(glbDef(Lc,ExtNm,Tp,Vl));true).

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
  is_member((Nm,Ix),IxMap).

transformFunction(Lc,Nm,LclName,H,Tp,Extra,Eqns,Map,Opts,[Fun|Ex],Exx) :-
  (is_member(showTrCode,Opts) -> dispFunction(LclName,Tp,Eqns);true),
  progTypeArity(Tp,Arity),
  extraArity(Arity,Extra,Ar),
  extendFunTp(Tp,Extra,ATp),
  transformEquations(Map,Extra,Opts,Eqns,Rules,[],Ex,Ex0),
%  (is_member(showTrCode,Opts) -> dispEquations(Rules);true),
  closureEntry(Map,Lc,Nm,Tp,Arity,Extra,Ex0,Exx),
  functionMatcher(Lc,Ar,lbl(LclName,Ar),H,ATp,Rules,Map,Fun),!,
  (is_member(showTrCode,Opts) -> dispRuleSet(Fun);true).

closureEntry(Map,Lc,Name,Tp,Arity,Extra,[ClEntry|L],L) :-
  lookupVar(Map,Name,Reslt),
  programAccess(Reslt,Prog,Closure),
  genVars(Arity,Args),
  extendFunTp(Tp,[_],TTp),
  Ar is Arity+1,
  (Extra = [] ->
   genVar("ϕ",FrVr),
   ClEntry = fnDef(Lc,lbl(Closure,Ar),hard,TTp,[FrVr|Args],cll(Lc,lbl(Prog,Arity),Args)) ;
   concat(Extra,Args,XArgs),
   length(XArgs,ArXX),
   ClEntry = fnDef(Lc,lbl(Closure,Ar),hard,TTp,XArgs,cll(Lc,lbl(Prog,ArXX),XArgs))).
					%  dispRuleSet(ClEntry).

extendFunTp(Tp,[],Tp):-!.
extendFunTp(funType(tplType(Els),Rt),Extra,funType(tplType(NEls),Rt)) :-
  extendTplTp(Extra,Anons),!,
  concat(Anons,Els,NEls).
extendFunTp(allType(V,T),Extra,allType(V,NT)) :-
  extendFunTp(T,Extra,NT).
extendFunTp(existType(V,T),Extra,existType(V,NT)) :-
  extendFunTp(T,Extra,NT).
extendFunTp(constrained(T,C),Extra,constrained(NT,C)) :-
  extendFunTp(T,Extra,NT).

extendTplTp([],[]).
extendTplTp([_|M],[anonType|NEls]) :-
  extendTplTp(M,NEls).

transformEquations(_,_,_,[],Rules,Rules,Ex,Ex).
transformEquations(Map,Extra,Opts,[Eqn|Defs],Rules,Rx,Ex,Exx) :-
  transformEqn(Eqn,Map,Extra,Opts,Rules,R0,Ex,Ex0),
  transformEquations(Map,Extra,Opts,Defs,R0,Rx,Ex0,Exx).

transformEqn(equation(Lc,A,G,Value),OMap,Extra,Opts,[(Lc,Args,Test,Rep)|Rx],Rx,Ex,Exx) :-
  filterVars(Extra,Q0),
  liftArgPtn(A,AA,Q0,Q1,OMap,Opts,Ex,Ex0), % head args
  liftGuard(G,Test,Q1,Q2,OMap,Opts,Ex0,Ex1),
  concat(Extra,AA,Args),
  liftExp(Value,Rep,Q2,_Q3,OMap,Opts,Ex1,Exx). % replacement expression

liftGuard(none,none,Q,Q,_,_,Ex,Ex) :-!.
liftGuard(some(G),some(LG),Q,Qx,Map,Opts,Ex,Exx) :-
  liftGoal(G,LG,Q,Qx,Map,Opts,Ex,Exx).

transformThetaVarDef(_Lc,Nm,_LclName,_Tp,Exp,Map,OMap,Opts,F,[(Nm,Ix,Rep)|F],Dx,Dxx) :-
  liftExp(Exp,Rep,[],_Qx,OMap,Opts,Dx,Dxx),
  lookupVar(Map,Nm,labelArg(_,Ix,_ThVr)).

transformThetaDefs(_,_,_,_,[],Fx,Fx,Dfs,Dfs).
transformThetaDefs(Map,OMap,Extra,Opts,[Def|Defs],F,Fx,Ex,Exx) :-
  transformThetaDef(Def,Extra,Map,OMap,Opts,F,F1,Ex,Ex1),!,
  transformThetaDefs(Map,OMap,Extra,Opts,Defs,F1,Fx,Ex1,Exx).

transformThetaDef(funDef(Lc,Nm,ExtNm,H,Tp,_,Eqns),Extra,Map,_OMap,Opts,Fx,Fx,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,H,Tp,Extra,Eqns,Map,Opts,Dx,Dxx).
transformThetaDef(varDef(Lc,Nm,ExtNm,_,Tp,Value),_,Map,OMap,Opts,F,Fx,Dx,Dxx) :-
  transformThetaVarDef(Lc,Nm,ExtNm,Tp,Value,Map,OMap,Opts,F,Fx,Dx,Dxx).
transformThetaDef(cnsDef(_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(typeDef(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(conDef(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(accDef(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(implDef(_,_,_,_),_,_,_,_,Fx,Fx,Dx,Dx).

liftArgPtn(tple(_Lc,Els),A,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(Els,A,Q,Qx,Map,Opts,Ex,Exx).

liftPtns([],[],Q,Q,_,_,Ex,Ex) :-!.
liftPtns([P|More],[A|Args],Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftPtns(More,Args,Q0,Qx,Map,Opts,Ex0,Exx).

liftPtn(v(_,"this",_),ThVr,Q,Qx,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
liftPtn(v(Lc,Nm,_),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts).
liftPtn(anon(_,_),anon,Q,Q,_,_,Ex,Ex).
liftPtn(enm(Lc,Nm,_),Ptn,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,Ptn,Q,Qx,Map,Opts).
liftPtn(void,voyd,Q,Q,_,_,Ex,Ex):-!.
liftPtn(intLit(_,Ix),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(floatLit(_,Dx),float(Dx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(stringLit(_,Sx),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(tple(_,Ptns),PTpl,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(Ptns,Ps,Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(Ps,PTpl).
liftPtn(apply(Lc,v(_,Nm,_),tple(_,A),_),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(apply(Lc,cons(_,Nm,_),tple(_,A),_),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(XX,whr(Lc,Vr,mtch(Lc,Vr,Val)),Q,Qx,Map,Opts,Ex,Exx) :-
  locOfCanon(XX,Lc),
  genVar("ϕ",Vr),
  liftExp(XX,Val,Q,Qx,Map,Opts,Ex,Exx).

trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts) :-
  lookupVar(Map,Nm,V),!,
  implementVarPtn(V,Nm,Lc,A,Map,Opts,Q,Qx).

implementVarPtn(moduleVar(Vn),_,Lc,cll(Lc,lbl(Vn,0),[]),_,_,Q,Q) :-
  reportError("not allowed to have globals in patterns: %w",[Vn],Lc). % module variable
implementVarPtn(labelArg(N,Ix,ThVr),_,Lc,
		whr(Lc,N,mtch(Lc,N,nth(Lc,Vr,Ix))),Map,Opts,Q,Qx) :- !, % argument from label
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Q0),
  merge([N],Q0,Qx).
implementVarPtn(moduleCons(Enum,_,0),_,_,enum(Enum),_,_,Q,Q).
implementVarPtn(notInMap,Nm,_,idnt(Nm),_,_,Q,Qx) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(Lc,Nm,Args,whr(Lc,X,mtch(Lc,X,intrinsic(Lc,Op,Args))),
	    Q,Qx,_,_,Ex,Ex) :-
  isIntrinsic(Nm,_,Op),!,
  genVar("_X",X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Args,whr(Lc,X,mtch(Lc,X,ecll(Lc,Nm,Args))),Q,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!,
  genVar("_X",X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Args,Ptn,Q,Qx,Map,Opts,Ex,Ex) :-
  lookupVar(Map,Nm,V),
  implementPtnCall(V,Lc,Args,Ptn,Map,Opts,Q,Qx).

implementPtnCall(localFun(Fn,_,Ar,ThVr),Lc,Args,
		 whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,A2),XArgs))),Map,Opts,Q,Qx) :-
  genVar("_X",X),
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx),
  concat(Args,[Vr],XArgs),
  merge([X],Q,Qx),
  A2 is Ar+1.
implementPtnCall(moduleFun(Fn,_,Ar),Lc,Args,
		 whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,Ar),Args))),_,_,Q,Qx) :-
  genVar("_X",X),
  merge([X],Q,Qx).
implementPtnCall(moduleCons(Mdl,_,Ar),_,Args,ctpl(lbl(Mdl,Ar),Args),_,_,Q,Q).

liftExps([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftExps([P|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftExps(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

liftExp(v(Lc,Nm,_),Vr,Q,Qx,Map,Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Vr,Map,Opts,Q,Qx).
liftExp(enm(Lc,Nm,_),Exp,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Exp,Map,Opts,Q,Qx).
liftExp(cons(Lc,Nm,_),Vr,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Vr,Map,Opts,Q,Qx).
liftExp(intLit(_,Ix),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(floatLit(_,Dx),float(Dx),Q,Q,_,_,Ex,Ex) :-!.
liftExp(stringLit(_,Sx),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftExp(tple(_,A),TApl,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,TA,[],Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(TA,TApl).
liftExp(throw(Lc,E,_),rais(Lc,Exp),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(apply(Lc,Op,tple(_,A),_),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,LA,[],Q,Q1,Map,Opts,Ex,Ex1),
  trExpCallOp(Lc,Op,LA,Exp,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(tag(Lc,Tp),tg(Lc,Tp),Q,Q,_,_,Ex,Ex) :-!.
liftExp(shift(Lc,Lb,E),shft(Lc,LL,EE),Q,Q,Map,Opts,Ex,Exx) :-!,
  liftExp(Lb,LL,Q,_,Map,Opts,Ex,Ex0),
  liftExp(E,EE,Q,_,Map,Opts,Ex0,Exx).
liftExp(prompt(Lc,L,E,_),prmpt(Lc,Lb,EE),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(L,Lb,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(E,EE,Q0,Qx,Map,Opts,Ex0,Exx).
liftExp(resume(Lc,Kont,Arg,_Tp),resme(Lc,KK,AA),Q,Q,Map,Opts,Ex,Exx) :-!,
  liftExp(Kont,KK,Q,_,Map,Opts,Ex,Ex0),
  liftExp(Arg,AA,Q,_,Map,Opts,Ex0,Exx).
liftExp(sequence(Lc,Lb,E),seq(Lc,LL,EE),Q,Q,Map,Opts,Ex,Exx) :-!,
  liftExp(Lb,LL,Q,_,Map,Opts,Ex,Ex0),
  liftExp(E,EE,Q,_,Map,Opts,Ex0,Exx).
liftExp(case(Lc,Bnd,Cses,_),Result,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(Bnd,Bound,Q,Q0,Map,Opts,Ex,Ex0),
  liftCases(Cses,Cases,Q0,Qx,Map,Opts,transform:liftExp,Ex0,Exx),
  (idnt(_)=Bound ->
   caseMatcher(Lc,Bound,Cases,Map,Result) ;
   genVar("_C",V),
   caseMatcher(Lc,V,Cases,Map,Res),
   Result = ltt(Lc,V,Bound,Res)).
liftExp(cell(Lc,In),intrinsic(Lc,iCell,[CellV]),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(In,CellV,Q,Qx,Map,Opts,Ex,Exx).
liftExp(deref(Lc,In),intrinsic(Lc,iGet,[CellV]),Q,Qx,Map,Opts,Ex,Exx) :- !,
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
liftExp(assertion(Lc,G),ecll(Lc,"_assert",[Gx,Lx]),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(G,Gx,Q,Qx,Map,Opts,Ex,Exx),
  locTerm(Lc,Lx).
liftExp(letExp(Lc,Decls,Defs,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  (is_member(showTrCode,Opts) -> dispCanon(letExp(Lc,Decls,Defs,Bnd));true),
  liftLetExp(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(letRec(Lc,Decls,Defs,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetRec(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(lambda(Lc,Lbl,Rle,Tp),Rslt,Q,Q,Map,Opts,Ex,Exx) :-!,
  liftLambda(lambda(Lc,Lbl,Rle,Tp),Rslt,Q,Map,Opts,Ex,Exx).
liftExp(abstraction(Lc,Bnd,Cond,Zed,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftAbstraction(abstraction(Lc,Bnd,Cond,Zed,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx).
liftExp(doTerm(Lc,Action,_),doAct(Lc,Act),Q,Q,Map,Opts,Ex,Exx) :-
  liftAction(last,Action,Act,Q,_,Map,Opts,Ex,Exx).
liftExp(XX,void,Q,Q,_,_,Ex,Ex) :-
  locOfCanon(XX,Lc),
  reportFatal("internal: cannot transform %s as expression",[XX],Lc).

liftLetExp(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  genVar("_ThR",ThVr),
  letMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,ThMap,_RMap,FreeTerm),
  transformThetaDefs(ThMap,Map,[ThVr],Opts,Defs,[],Fx,Ex,Ex1),
  liftExp(Bnd,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp),
  (is_member(showTrCode,Opts) -> dispTerm(Exp);true).

liftLetRec(Lc,Decls,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  genVar("_ThV",ThVr),
  letRecMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,ThMap,FreeTerm),
%  (is_member(showTrCode,Opts) -> dispMap("Theta map: ",1,ThMap);true),
  transformThetaDefs(ThMap,ThMap,[ThVr],Opts,Defs,[],Fx,Ex,Ex1),
  liftExp(Bnd,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp),
  (is_member(showTrCode,Opts) -> dispTerm(Exp);true).

mkFreeLet(Lc,Vr,Fr,Ups,Exp,AExp) :-
  computeFreeVect(Lc,Vr,Fr,Ups,Exp,AExp).

computeFreeVect(Lc,Vr,Fr,[],Exp,ltt(Lc,Vr,Fr,Exp)).
computeFreeVect(Lc,Vr,ctpl(Lbl,Args),[(_,Ix,Term)|Ups],Exp,Reslt) :-
  \+idInTerm(Vr,Term),!,
  split_list(Ix,Args,F,[voyd|R]),
  concat(F,[Term|R],NArgs),
  computeFreeVect(Lc,Vr,ctpl(Lbl,NArgs),Ups,Exp,Reslt).
computeFreeVect(Lc,Vr,Fr,[(_,Ix,Term)|Ups],Exp,Reslt) :-
  computeFreeVect(Lc,Vr,Fr,Ups,seq(Lc,setix(Lc,Vr,Ix,Term),Exp),Reslt).
  
trVarExp(Lc,Nm,Exp,Map,Opts,Q,Qx) :-
  lookupVar(Map,Nm,V),!,
  implementVarExp(V,Lc,Nm,Exp,Map,Opts,Q,Qx),!.
trVarExp(Lc,Nm,idnt("_"),_,_,Q,Q) :-
  reportError("'%s' not defined",[Nm],Lc).

liftVar(_,Vr,Vr,Map,_Opts,Q,Qx):-
  thisVar(Map,Vr),!,
  merge([Vr],Q,Qx).
liftVar(Lc,idnt(Nm),Vr,Map,Opts,Q,Qx) :-
  trVarExp(Lc,Nm,Vr,Map,Opts,Q,Qx).

implementVarExp(localVar(_,Val),_,_Nm,Exp,Map,Opts,Q,Qx) :-
  liftExp(Val,Exp,Q,Qx,Map,Opts,[],[]).
implementVarExp(moduleVar(V),_Lc,_,idnt(V),_,_,Qx,Qx).
implementVarExp(labelArg(_N,Ix,ThVr),Lc,_,nth(Lc,ThV,Ix),Map,Opts,Q,Qx) :-
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementVarExp(moduleCons(Enum,_,0),_,_,enum(Enum),_,_,Q,Q).
implementVarExp(moduleCons(C,_,Ar),_,_,Cns,_,_,Q,Q) :-
  trCons(C,Ar,Cns).
implementVarExp(notInMap,_,Nm,idnt(Nm),_,_,Q,Qx) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(moduleFun(_,some(Closure),_),_,_,ctpl(lbl(Closure,1),[Unit]),_,_,Q,Q) :-
  Closure\==void,
  mkTpl([],Unit).
implementVarExp(localFun(_Fn,Closure,_,ThVr),Lc,_,ctpl(lbl(Closure,1),[Vr]),Map,Opts,Q,Qx) :-
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx).
implementVarExp(_Other,Lc,Nm,idnt(Nm),_,_,Q,Q) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

trExpCallOp(Lc,v(_,Nm,_),Args,intrinsic(Lc,Op,Args),Qx,Qx,_,_,Ex,Ex) :-
  isIntrinsic(Nm,_,Op),!.
trExpCallOp(Lc,v(_,Nm,_),Args,ecll(Lc,Nm,Args),Qx,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!.
trExpCallOp(Lc,v(_,Nm,_),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  lookupVar(Map,Nm,Reslt),
  Reslt\=notInMap,!,
  implementFunCall(Lc,Reslt,Nm,Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,enm(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  trExpCallOp(Lc,v(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,cons(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  trExpCallOp(Lc,v(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,Op,A,ocall(Lc,Rc,A),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Op,Rc,Q,Qx,Map,Opts,Ex,Exx).

implementFunCall(Lc,localFun(Fn,_,Ar,ThVr),_,Args,cll(Lc,lbl(Fn,Ar2),XArgs),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx),
  concat([Vr],Args,XArgs),
  Ar2 is Ar+1.
implementFunCall(Lc,moduleFun(Fn,_,Ar),_,Args,cll(Lc,lbl(Fn,Ar),Args),Qx,Qx,_,_,Ex,Ex).
implementFunCall(Lc,moduleVar(Fn),_,Args,ocall(Lc,idnt(Fn),Args),Qx,Qx,_,_,Ex,Ex).
implementFunCall(_,moduleCons(Mdl,_,Ar),_,Args,ctpl(lbl(Mdl,Ar),Args),Q,Q,_,_,Ex,Ex).
implementFunCall(Lc,labelArg(_,Ix,ThVr),_,Args,ocall(Lc,nth(Lc,ThV,Ix),Args),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,ThV,Map,Opts,Q,Qx).
implementFunCall(Lc,notInMap,Nm,Args,ocall(Lc,idnt(Nm),Args),Q,Q,_Map,_Opts,Ex,Ex) :-
  reportError("cannot compile unknown function %s",[Nm],Lc).

liftCases([],[],Qx,Qx,_Map,_Opts,_,Exx,Exx) :- !.
liftCases([C|Cses],[Case|Cases],Q,Qx,Map,Opts,Lifter,Ex,Exx) :-
  liftCase(C,Case,Q,Q0,Map,Opts,Lifter,Ex,Ex0),
  liftCases(Cses,Cases,Q0,Qx,Map,Opts,Lifter,Ex0,Exx).

liftCase(equation(Lc,P,G,Value),(Lc,[Ptn],Test,Rep),Q,Qx,Map,Opts,Lifter,Ex,Exx) :-
  liftPtn(P,Ptn,Q,Q0,Map,Opts,Ex,Ex0),
  liftGuard(G,Test,Q0,Q1,Map,Opts,Ex0,Ex1), % condition goals
  call(Lifter,Value,Rep,Q1,Qx,Map,Opts,Ex1,Exx). % replacement expression
  
%  liftExp(Value,Rep,Q1,Qx,Map,Opts,Ex1,Exx).  % replacement expression

liftLambda(lambda(Lc,LamLbl,Eqn,Tp),Closure,Q,Map,Opts,[LamFun|Ex],Exx) :-
  (is_member(showTrCode,Opts) -> dispCanon(lambda(Lc,LamLbl,Eqn,Tp));true),
  lambdaMap(lambda(Lc,LamLbl,Eqn,Tp),ThVr,LamLbl,Q,Map,Opts,Closure,LMap),
%  (is_member(showTrCode,Opts) -> dispMap("lambda map: ",1,LMap);true),
  transformEqn(Eqn,LMap,[ThVr],Opts,Rls,[],Ex,Exx),
  is_member((_,Args,_,_),Rls),!,
  length(Args,Ar),
  functionMatcher(Lc,Ar,lbl(LamLbl,Ar),hard,Tp,Rls,Map,LamFun),
  (is_member(showTrCode,Opts) -> dispRuleSet(LamFun);true).

liftAbstraction(Ab,Rslt,Q,Qx,Map,Opts,Ex,Exx) :-
  layerName(Map,Path),
  genAbstraction(Ab,Path,AbExp),
  liftExp(AbExp,Rslt,Q,Qx,Map,Opts,Ex,Exx).

liftSearch(Serch,Rslt,Q,Qx,Map,Opts,Ex,Exx) :-
  layerName(Map,Path),
  genSearch(Serch,Path,AbGl),
  liftExp(AbGl,Rslt,Q,Qx,Map,Opts,Ex,Exx).

mkClosure(Lam,FreeVars,Closure) :-
  length(FreeVars,Ar),
  (Ar = 0 ->
    Closure=enum(Lam) |
  Closure=ctpl(lbl(Lam,Ar),FreeVars)).

liftAction(Last,noDo(Lc),nop(Lc),Qx,Qx,_,_,Ex,Ex) :- checkNotLast(Last,Lc).
liftAction(_,valisDo(Lc,E),rtnDo(Lc,Exp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftAction(_,throwDo(Lc,E),raisDo(Lc,Exp),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftAction(Last,seqDo(Lc,E1,E2),seq(Lc,L1,L2),Q,Qx,Map,Opts,Ex,Exx) :-
  liftAction(notLast,E1,L1,Q,Q0,Map,Opts,Ex,Ex1),
  liftAction(Last,E2,L2,Q0,Qx,Map,Opts,Ex1,Exx).
liftAction(Last,varDo(Lc,P,E),varD(Lc,P1,E1),Q,Q,Map,Opts,Ex,Exx) :-
  checkNotLast(Last,Lc),
  liftPtn(P,P1,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(E,E1,Q0,_,Map,Opts,Ex0,Exx).
liftAction(Last,assignDo(Lc,P,E),assignD(Lc,P1,E1),Q,Q,Map,Opts,Ex,Exx) :-
  checkNotLast(Last,Lc),
  liftExp(P,P1,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(E,E1,Q0,_,Map,Opts,Ex0,Exx).
liftAction(_,performDo(Lc,Exp),perfDo(Lc,E1),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Exp,E1,Q,Qx,Map,Opts,Ex,Exx).
liftAction(_,simpleDo(Lc,Exp),justDo(Lc,EE),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Exp,EE,Q,Qx,Map,Opts,Ex,Exx).
liftAction(_,promptDo(Lc,Lb,Lam,_),promptD(Lc,LL,Thnk),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Lb,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(Lam,Thnk,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(_,cutDo(Lc,Lb,Lm),cutD(Lc,LL,Thnk),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Lb,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(Lm,Thnk,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(_,resumeDo(Lc,K,A,_),resumeD(Lc,KK,AA),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(K,KK,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(A,AA,Q0,Qx,Map,Opts,Ex0,Exx).
liftAction(Last,ifThenDo(Lc,Ts,Th,El),cnd(Lc,Tst,Th1,El1),Q,Qx,Map,Opts,Ex,Exx) :-
  liftGoal(Ts,Tst,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(Last,Th,Th1,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftAction(Last,El,El1,Q1,Qx,Map,Opts,Ex1,Exx).
liftAction(Last,whileDo(Lc,G,B),whle(Lc,Gl,Bdy),Q,Q,Map,Opts,Ex,Exx) :-
  checkNotLast(Last,Lc),
  liftGoal(G,Gl,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(Last,B,Bdy,Q0,_,Map,Opts,Ex0,Exx).
liftAction(Last,untilDo(Lc,G,B),untl(Lc,Gl,Bdy),Q,Q,Map,Opts,Ex,Exx) :-
  liftGoal(G,Gl,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(Last,B,Bdy,Q0,_,Map,Opts,Ex0,Exx).
liftAction(Last,forDo(Lc,G,B),forD(Lc,Gl,Bdy),Q,Q,Map,Opts,Ex,Exx) :-
  checkNotLast(Last,Lc),
  liftGoal(G,Gl,Q,Q0,Map,Opts,Ex,Ex0),
  liftAction(Last,B,Bdy,Q0,_,Map,Opts,Ex0,Exx).
liftAction(Last,caseDo(Lc,G,C),Result,Q,Q,Map,Opts,Ex,Exx) :-
  liftExp(G,Bound,Q,_,Map,Opts,Ex,Ex0),
  liftCases(C,Cs,Q,_,Map,Opts,transform:liftAction(Last),Ex0,Exx),
  (idnt(_)=Bound ->
   caseMatcher(Lc,Bound,Cs,Map,Result) ;
   genVar("_C",V),
   caseMatcher(Lc,V,Cs,Map,Res),
   Result = seqD(Lc,varD(Lc,V,Bound),Res)).
liftAction(_,tryCatchDo(Lc,Bdy,Lam),tryDo(Lc,BB,H),Q,Q,Map,Opts,Ex,Exx) :-
  liftAction(last,Bdy,BB,Q,_,Map,Opts,Ex,Ex0),
  liftExp(Lam,H,Q,_,Map,Opts,Ex0,Exx).

checkNotLast(last,Lc) :-
  reportError("not permitted to be last action",[],Lc).
checkNotLast(notLast,_).

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
liftGl(search(Lc,Ptn,Src,Iterator),Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftSearch(search(Lc,Ptn,Src,Iterator),Exp,Q,Qx,Map,Opts,Ex,Exx).
liftGl(G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(G,Gx,Q,Qx,Map,Opts,Ex,Exx).

cmpName((N1,_),(N2,_)) :- str_lt(N1,N2).

liftEls([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftEls([(_,P)|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftEls(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

letRecMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,[lyr(Vx,Tx,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVars(letRec(Lc,Decls,Defs,Bnd),Map,Q,ThFree),
  varDefs(Defs,CellVars),
  concat(CellVars,ThFree,FreeVars),
  collectLabelVars(FreeVars,ThVr,0,varMap{},V0),
  makeConstructorMap(Defs,consMap{},ConsMap),
  declareThetaVars(Decls,ThVr,CellVars,ConsMap,V0,Vx,typeMap{},Tx),
  makeFreeTerm(CellVars,Lc,ThFree,Map,Opts,FreeTerm).

letMap(Lc,Decls,Defs,Bnd,ThVr,Q,Map,Opts,
       [lyr(Vx,Tx,ConsMap,ThVr)|Map],[lyr(V0,Tx,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVars(letExp(Lc,Decls,Defs,Bnd),Map,Q,ThFree),
  varDefs(Defs,CellVars),
  concat(CellVars,ThFree,FreeVars),
  makeConstructorMap(Defs,consMap{},ConsMap),
  declareThetaVars(Decls,ThVr,CellVars,ConsMap,varMap{},V0,typeMap{},Tx),
  collectLabelVars(FreeVars,ThVr,0,V0,Vx),
  makeFreeTerm(CellVars,Lc,ThFree,Map,Opts,FreeTerm).
%  (is_member(showTrCode,Opts) -> dispMap("Record map: ",1,[lyr(Vx,Tx,ConsMap,ThVr)|Map]);true).

lambdaMap(Lam,ThVr,LamLbl,Q,Map,Opts,ctpl(lbl(LamLbl,1),[FreeTerm]),
	  [lyr(Vx,typeMap{},consMap{},ThVr)|Map]) :-
  findFreeVars(Lam,Map,Q,LmFree),
  genVar("_ΛV",ThVr),
  collectLabelVars(LmFree,ThVr,0,varMap{},Vx),
  locOfCanon(Lam,Lc),
  makeFreeTerm([],Lc,LmFree,Map,Opts,FreeTerm).

findFreeVars(Exp,Map,_Q,LmFr0) :-
  definedProgs(Map,Df),
  freeVars(Exp,Df,transform:addThFree,[],ThFr),
  freeLabelVars(ThFr,Map,[],LmFr0).

addThFree(Ex,Nm,Fr,Fx) :-
  \+ is_member(idnt(Nm),Ex),
  add_mem(idnt(Nm),Fr,Fx).
addThFree(_,_,Fr,Fr).

freeLabelVars([],_,Fr,Fr).
freeLabelVars([idnt(Nm)|Lv],Map,Fr,LmFr) :-
  lookupThetaVar(Map,Nm,ThVr),!,
  merge([ThVr],Fr,Fr1),
  freeLabelVars([ThVr|Lv],Map,Fr1,LmFr).
freeLabelVars([idnt(Nm)|Lv],Map,Fr,LmFr) :-
  isModuleVar(Map,Nm),!,
  freeLabelVars(Lv,Map,Fr,LmFr).
freeLabelVars([V|Lv],Map,Fr,LmFr) :-
  merge([V],Fr,Fr1),
  freeLabelVars(Lv,Map,Fr1,LmFr).

isModuleVar(Map,Nm) :-
  lookupVar(Map,Nm,R),
  (R=moduleFun(_,_,_) ; R=moduleVar(_)),!.

varDefs(Defs,Vars) :-
  rfold(Defs,transform:isVarDef,[],Vars).

isVarDef(varDef(_,Nm,_,_,_,_),F,Fv) :-
  add_mem(idnt(Nm),F,Fv).
isVarDef(_,Fv,Fv).

cellVars(Defs,CellVars) :-
  rfold(Defs,transform:pickCellVar,[],CellVars).

pickCellVar(varDef(_,Nm,_,_,Tp,_),F,Fv) :-
  deRef(Tp,refType(_)),!,
  add_mem(idnt(Nm),F,Fv).
pickCellVar(varDef(_,Nm,_,_,_,Val),F,Fv) :-
  \+isSimpleCanon(Val),!,
  add_mem(idnt(Nm),F,Fv).
pickCellVar(_,F,F).

makeFreeTerm(CellVars,Lc,ThFr,Map,Opts,FreeTerm) :-
  map(CellVars,transform:emptyCell(Lc),CV),
  map(ThFr,transform:mkFreeVar(Map,Opts,Lc),FrExps),
  concat(CV,FrExps,Args),
  mkTpl(Args,FreeTerm).

emptyCell(_Lc,idnt(_),voyd).

mkFreeVar(Map,Opts,Lc,idnt(Nm),Vr) :-
  trVarExp(Lc,Nm,Vr,Map,Opts,[],_).

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

notVar(V) :- V\=idnt(_).

isIdnt(idnt(_)).

declareThetaVars([],_,_,_,Vx,Vx,Tx,Tx).
declareThetaVars([D|Ds],ThVr,CellVars,ConsMap,V,Vx,T,Tx) :-
  declareThetaVar(D,ThVr,CellVars,ConsMap,V,V0,T,T0),!,
  declareThetaVars(Ds,ThVr,CellVars,ConsMap,V0,Vx,T0,Tx).

declareThetaVar(funDec(Nm,LclName,Tp),ThV,_,_,V,Vx,Tx,Tx) :-
  localName(LclName,closure,ClosureName),
  progTypeArity(Tp,Ar),
  declEntry(Nm,localFun(LclName,ClosureName,Ar,ThV),V,Vx).
declareThetaVar(varDec(Nm,LclName,_),ThV,CellVars,_,V,Vx,Tx,Tx) :-
  (is_member(idnt(Nm),CellVars) -> V=Vx ;
   declEntry(Nm,localVar(LclName,ThV),V,Vx)).
declareThetaVar(cnsDec(Nm,LclName,Tp),ThV,_,_,V,Vx,Tx,Tx) :-
  progTypeArity(Tp,Ar),
  declEntry(Nm,localCons(LclName,Tp,Ar,ThV),V,Vx).
declareThetaVar(typeDec(Nm,Tp,_,_),_,ConsMap,Vx,Vx,T,Tx) :-
  tpName(Tp,TpNm),
  findConsMap(TpNm,ConsMap,IxMap),!,
  declEntry(Nm,localType(TpNm,Tp,IxMap),T,Tx).
declareThetaVar(_,_,_,Vx,Vx,Tx,Tx).

collectLabelVars([],_,_,List,List).
collectLabelVars([idnt(Nm)|Args],ThVr,Ix,List,Lx) :-
  Ix1 is Ix+1,
  declEntry(Nm,labelArg(idnt(Nm),Ix,ThVr),List,L0),
  collectLabelVars(Args,ThVr,Ix1,L0,Lx).
collectLabelVars([_|Args],ThVr,Ix,List,Lx) :-
  collectLabelVars(Args,ThVr,Ix,List,Lx).

/*
 Generate the closure return:
 OuterNm(ThVr,.Nm) => ClosureNm(ThVr)
*/
closureRule(_,Lc,Nm,ClosureName,ThVr,
    (Lc,[ThVr,DotName],none,ctpl(lbl(ClosureName,1),[ThVr]))) :-
  makeDotLbl(Nm,DotName).

accessRule(_,Lc,Nm,LclName,ThV,(Lc,[ThV,DotName],enum("star.core#true"),cll(Lc,lbl(LclName,1),[ThV]))) :-
  makeDotLbl(Nm,DotName).

programAccess(moduleFun(Prog,some(Closure),_Arity),Prog,Closure).
programAccess(localFun(Prog,Closure,_Arity,_),Prog,Closure).

makeDotLbl(Nm,lbl(Nm,0)).

mkUnit(U) :-
  mkTpl([],U).

