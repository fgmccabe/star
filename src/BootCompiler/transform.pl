:- module(transform,[transformProg/3]).

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
:- use_module(cnc).

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

transformProg(prog(pkg(Pkg,Vers),Imports,Defs,PkgTp,Fields,Types,Contracts,Impls),
	      Opts,mdule(pkg(Pkg,Vers),Imports,PkgTp,faceType(Fields,Types),Ctors,Dfs,Contracts,Impls)) :-
  makePkgMap(Pkg,Defs,Types,Imports,Ctors,Map),
  transformModuleDefs(Defs,Pkg,Map,Opts,Dfs,[]).

makePkgMap(Pkg,Defs,Types,Imports,Ctors,[lyr(Entries,ConsMap,void)]) :-
  makeImportsMap(Imports,[],M1,consMap{},CnMap),
  makeConstructorMap(Defs,CnMap,Ctors,ConsMap),
  makeTypesMap(Pkg,Types,M1,M2),
  makeModuleMap(Pkg,Defs,ConsMap,M2,Entries).

makeModuleMap(Pkg,[Def|Rest],ConsMap,Map,Mx) :-
  makeMdlEntry(Pkg,Def,ConsMap,Map,M0),
  makeModuleMap(Pkg,Rest,ConsMap,M0,Mx).
makeModuleMap(_,[],_,Map,Map).

makeMdlEntry(Pkg,funDef(_,Nm,LclName,Tp,_,_),_,Mx,[(Nm,moduleFun(LclName,ClosureName,Ar))|Mx]) :-
  localName(Pkg,closure,Nm,ClosureName),
  progTypeArity(Tp,Ar).
makeMdlEntry(_Pkg,varDef(_,Nm,LclName,_,_,_),_,Mx,[(Nm,moduleVar(LclName))|Mx]).
makeMdlEntry(_Pkg,cnsDef(_,Nm,Cons),_,Mx,
	     [(Nm,moduleCons(LclName,Tp,Ar)),
	      (LclName,moduleCons(LclName,Tp,Ar))|Mx]) :-
  constructorName(Cons,LclName),
  constructorType(Cons,Tp),
  progTypeArity(Tp,Ar).
makeMdlEntry(_Pkg,typeDef(_,_,Tp,_IxMap,_),ConsMap,Mx,[(TpNm,moduleType(TpNm,Tp,IxMap))|Mx]) :-
  tpName(Tp,TpNm),
  findConsMap(TpNm,ConsMap,IxMap),!.
makeMdlEntry(_,implDef(_,_,ImplNm,Tp),_,Mx,[(ImplNm,Entry)|Mx]) :-
  makeImplEntry(ImplNm,Tp,Entry).
makeMdlEntry(_,_,_,Mx,Mx).

makeImplEntry(ImplNm,Tp,Entry) :-
  estimateImplArity(Tp,Ar),
  (Ar=0 -> Entry=moduleVar(ImplNm) ; Entry=moduleFun(ImplNm,ImplNm,Ar)).

estimateImplArity(allType(_,Tp),Ar) :- estimateImplArity(Tp,Ar).
estimateImplArity(constrained(T,_),Ar) :- estimateImplArity(T,A), Ar is A+1.
estimateImplArity(_,0).

makeImportsMap([Import|Rest],Map,Mx,CnMp,ConsMap) :-
  makeImportMap(Import,Map,M0,CnMp,CnMp0),
  makeImportsMap(Rest,M0,Mx,CnMp0,ConsMap).
makeImportsMap([],Map,Map,ConsMap,ConsMap).

makeImportMap(import(_,pkg(Pkg,_),_,
		     faceType(Fields,Types),Cns,_,_Impls),Map,Mx,CnMp,ConsMap) :-
  importFields(Pkg,Fields,Map,M0),
  importCns(Cns,CnMp,ConsMap,M0,M1),
  importTypes(Types,M1,Mx).

importFields(_,[],Map,Map).
importFields(Pkg,[(Nm,Tp)|Fields],Map,Mx) :-
  moveQuants(Tp,_,QTp),
  getConstraints(QTp,_,Template),
  progTypeArity(Tp,Ar),
  makeImportEntry(Template,Ar,Pkg,Nm,Map,M0),
  importFields(Pkg,Fields,M0,Mx).

makeImportEntry(funType(_,_),Ar,Pkg,Nm,Mx,[(Nm,moduleFun(LclName,ClosureName,Ar))|Mx]) :-
  packageVarName(Pkg,Nm,LclName),
  localName(Pkg,closure,Nm,ClosureName).
makeImportEntry(consType(At,Tp),Ar,Pkg,Nm,Mx,
		[(Nm,moduleCons(LclName,consType(At,Tp),Ar)),
		 (LclName,moduleCons(LclName,consType(At,Tp),Ar))|Mx]) :-
  localName(Pkg,class,Nm,LclName).
makeImportEntry(_,_,Pkg,Nm,Mx,[(Nm,moduleVar(LclName))|Mx]) :-
  packageVarName(Pkg,Nm,LclName).

importImplementations([],Map,Map).
importImplementations([imp(_,FullNm,Con)|L],M,[(FullNm,moduleVar(FullNm))|Mx]) :-
  typeArity(Con,0),
  importImplementations(L,M,Mx).
importImplementations([imp(_,FullNm,Con)|L],M,[(FullNm,moduleFun(FullNm,ClosureName,Ar))|Mx]) :-
  typeArity(Con,Ar),
  importImplementations(L,M,Mx),
  localName("",closure,FullNm,ClosureName).
importImplementations([acc(_,_,_,_)|L],M,Mx) :-
  importImplementations(L,M,Mx).

importCns([],Cnx,Cnx,Map,Map) :-!.
importCns([(TpNm,L)|Cs],CnIx,Cxs,Map,Mpx) :-
  makeKey(TpNm,Key),
  map(L,transform:mkConsLbl,L0),
  sort(L0,transform:lblLt,L1),
  index_list(L1,0,SL),
  put_dict(Key,CnIx,SL,CnIx0),
  moduleEntries(L,Map,Map0),
  importCns(Cs,CnIx0,Cxs,Map0,Mpx).

moduleEntries(Cns,Map,Mpx) :-
  rfold(Cns,transform:moduleConsEntry,Map,Mpx).

moduleConsEntry((Nm,FlNm,Tp),Map,
		[(Nm,moduleCons(FlNm,Tp,Ar)),
		 (FlNm,moduleCons(FlNm,Tp,Ar))|Map]) :-
  progTypeArity(Tp,Ar).

importTypes(_,L,L).

contractArity(allType(_,Con),Ar) :- contractArity(Con,Ar).
contractArity(constrained(Con,_),Ar) :- contractArity(Con,A), Ar is A+1.
contractArity(contractExists(_,_),0).

contractStruct(0,Nm,enum(Nm)).
contractStruct(Ar,Nm,lbl(Nm,Ar)).

makeTypesMap(_,_,List,List).

makeConstructorMap(Defs,CnMp,Cons,ConsMap) :-
  findAllConstructors(Defs,[],Cons),
  indexConstructors(Cons,CnMp,ConsMap).

findAllConstructors([],Cons,Cons) :-!.
findAllConstructors([cnsDef(_,Nm,Cns)|Defs],Cons,Cnx) :-
  constructorName(Cns,FullNm),
  constructorType(Cns,CnsTp),
  consTpName(CnsTp,TpNm),
  (concat(L1,[(TpNm,L)|L2],Cons) ->
   concat(L1,[(TpNm,[(Nm,FullNm,CnsTp)|L])|L2],C0) ;
   C0=[(TpNm,[(Nm,FullNm,CnsTp)])|Cons]),
  findAllConstructors(Defs,C0,Cnx).
findAllConstructors([_|Defs],Cons,Cnx) :-
  findAllConstructors(Defs,Cons,Cnx).

constructorName(enm(_,Nm,_),Nm) :-!.
constructorName(cons(_,Nm,_),Nm).

constructorType(enm(_,_,Tp),Tp) :-!.
constructorType(cons(_,_,Tp),Tp).

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

transformMdlDef(funDef(Lc,Nm,ExtNm,Tp,[],Eqns),_,Map,Opts,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,Tp,Eqns,Map,Map,Opts,Dx,Dxx).
transformMdlDef(varDef(Lc,_Nm,ExtNm,[],Tp,Val),_Pkg,Map,Opts,Dx,Dxx) :-
  transformGlobal(Lc,ExtNm,Val,Tp,Map,Opts,Dx,Dxx).
transformMdlDef(cnsDef(_,_,_),_Pkg,_Map,_Opts,Dx,Dx).
transformMdlDef(typeDef(Lc,_Nm,Tp,ConMap,Rl),_Pkg,Map,_,D,Dx) :-
  tpName(Tp,TpNm),
  transformTypeDef(Lc,TpNm,Tp,ConMap,Rl,Map,D,Dx).
transformMdlDef(conDef(_,_,_,_,_),_,_,_,Dx,Dx).
transformMdlDef(implDef(_,_,_,_),_,_,_,Dx,Dx).
transformMdlDef(accDef(_,_,_,_),_,_,_,Dx,Dx).

transformGlobal(Lc,ExtNm,Val,Tp,Map,Opts,[glbDef(Lc,ExtNm,Tp,Vl)|Dx],Dxx) :-
  liftExp(Val,Vl,[],_Q3,Map,Opts,Dx,Dxx). % replacement expression
%  dispRuleSet(glbDef(Lc,ExtNm,Tp,Vl)).

extraArity(Arity,Vars,ExAr) :-
  length(Vars,E),
  ExAr is E+Arity.

transformTypeDef(Lc,TpNm,Tp,_ConMap,Rl,Map,[tpDef(Lc,Tp,Rl,IxMap)|Dx],Dx) :-
  lookupType(Map,TpNm,moduleType(_,_,IxMap)),!.
transformTypeDef(Lc,_Nm,Tp,IxMap,Rl,_Map,[tpDef(Lc,Tp,Rl,IxMap)|Dx],Dx).

genConsArgs(_,[],Args,Args,BndArgs,BndArgs).
genConsArgs(Lc,[_|Els],[V|Args],Ax,[V|Bnd],Bx) :-
  genVar("_",V),
  genConsArgs(Lc,Els,Args,Ax,Bnd,Bx).

transformFunction(Lc,Nm,LclName,Tp,Eqns,Map,OMap,Opts,[Fun|Ex],Exx) :-
%  dispFunction(LclName,Tp,Eqns),
  lookupFunName(Map,Nm,Reslt),
  programAccess(Reslt,_,_,Arity),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = lbl(LclName,Ar),
  extendFunTp(Tp,Extra,ATp),
%  dispMap("OMap",OMap),
  transformEquations(Map,OMap,Opts,LclPrg,Eqns,Rules,[],Ex,Ex0),
  closureEntry(Map,Lc,Nm,Tp,Ex0,Exx),
  functionMatcher(Lc,Ar,LclPrg,ATp,Rules,Map,Fun).

extendFunTp(Tp,[],Tp):-!.
extendFunTp(funType(tupleType(Els),Rt),Extra,funType(tupleType(NEls),Rt)) :-
  extendTplTp(Extra,Anons),!,
  concat(Anons,Els,NEls).
extendFunTp(allType(V,T),Extra,allType(V,NT)) :-
  extendFunTp(T,Extra,NT).
extendFunTp(constrained(T,C),Extra,constrained(NT,C)) :-
  extendFunTp(T,Extra,NT).

extendTplTp([],[]).
extendTplTp([_|M],[anonType|NEls]) :-
  extendTplTp(M,NEls).

transformEquations(_,_,_,_,[],Rules,Rules,Ex,Ex).
transformEquations(Map,OMap,Opts,LclPrg,[Eqn|Defs],Rules,Rx,Ex,Exx) :-
  transformEqn(Eqn,Map,OMap,Opts,LclPrg,Rules,R0,Ex,Ex0),
  transformEquations(Map,OMap,Opts,LclPrg,Defs,R0,Rx,Ex0,Exx).

transformEqn(equation(Lc,A,G,Value),Map,OMap,Opts,_LclPrg,
    [(Lc,Args,Test,Rep)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),
  filterVars(Extra,Q0),
  liftArgPtn(A,AA,Q0,Q1,Map,Opts,Ex,Ex0), % head args
  liftGuard(G,Test,Q1,Q2,Map,Opts,Ex0,Ex1),
  concat(Extra,AA,Args),
  liftExp(Value,Rep,Q2,_Q3,OMap,Opts,Ex1,Exx). % replacement expression

liftGuard(none,none,Q,Q,_,_,Ex,Ex) :-!.
liftGuard(some(G),some(LG),Q,Qx,Map,Opts,Ex,Exx) :-
  liftGoal(G,LG,Q,Qx,Map,Opts,Ex,Exx).

transformThetaVarDef(Lc,Nm,_LclName,_Tp,Exp,Map,OMap,Opts,F,Fx,Dx,Dxx) :-
  \+isSimpleCanon(Exp),!,
  liftExp(Exp,Rep,[],_Qx,OMap,Opts,Dx,Dxx),
  lookupVarName(Map,Nm,labelArg(_,Ix,_ThVr)),
  updateFreeTerm(F,Ix,Lc,Rep,Fx).
transformThetaVarDef(_Lc,_Nm,_LclName,_Tp,_Exp,_Map,_OMap,_Opts,Fx,Fx,Dx,Dx).

/*updateFreeTerm((ctpl(Lbl,Args),Fx),Ix,_,Term,ThVr,(ctpl(Lbl,NArgs),Fx)) :-
  \+idInTerm(ThVr,Term),
  split_list(Ix,Args,F,[voyd|R]),
  refactorTerm(ThVr,ctpl(Lbl,Args),Term,Trm1),
  concat(F,[Trm1|R],NArgs).
*/
updateFreeTerm(SoFar,Ix,Lc,Term, [(Lc,Ix,Term)|SoFar]).
/*  refactorTerm(ThVr,In,Term,Term1).
updateFreeTerm((In,some(Up)),Ix,Lc,Term,ThVr,
	       (In,some(seq(Lc,Up,setix(Lc,ThVr,Ix,Term1))))) :-
  refactorTerm(ThVr,In,Term,Term1).
    */

% This works without blowing up because free terms are always relatively flat.
refactorTerm(ThVr,Fx,Term,T1) :-
  rewriteTerm(transform:rewriteFrV(ThVr,Fx),Term,T1).

rewriteFrV(Vr,ctpl(_,Args),nth(_Lc,Vr,Ix),Out) :-
  split_list(Ix,Args,_,[Out|_]).

transformThetaDefs(_,_,_,[],Fx,Fx,Dfs,Dfs).
transformThetaDefs(Map,OMap,Opts,[Def|Defs],F,Fx,Ex,Exx) :-
  transformThetaDef(Def,Map,OMap,Opts,F,F1,Ex,Ex1),!,
  transformThetaDefs(Map,OMap,Opts,Defs,F1,Fx,Ex1,Exx).

transformThetaDef(funDef(Lc,Nm,ExtNm,Tp,_,Eqns),Map,OMap,Opts,Fx,Fx,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,Tp,Eqns,Map,OMap,Opts,Dx,Dxx).
transformThetaDef(varDef(Lc,Nm,ExtNm,_,Tp,Value),Map,OMap,Opts,F,Fx,Dx,Dxx) :-
  transformThetaVarDef(Lc,Nm,ExtNm,Tp,Value,Map,OMap,Opts,F,Fx,Dx,Dxx).
transformThetaDef(cnsDef(_Lc,_Nm,_),_Map,_,_Opts,Fx,Fx,Dx,Dx).
transformThetaDef(typeDef(_,_,_,_,_),_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(conDef(_,_,_,_,_),_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(accDef(_,_,_,_),_,_,_,Fx,Fx,Dx,Dx).
transformThetaDef(implDef(_,_,_,_),_,_,_,Fx,Fx,Dx,Dx).

closureEntry(Map,Lc,Name,Tp,[ClEntry|L],L) :-
  lookupVarName(Map,Name,Reslt),
  programAccess(Reslt,Prog,Closure,Arity),
  genVars(Arity,Args),
  extraVars(Map,Extra),
  extendFunTp(Tp,[_],TTp),
  Ar is Arity+1,
  (Extra = [] ->
   genVar("_",FrVr),
   ClEntry = fnDef(Lc,lbl(Closure,Ar),TTp,[FrVr|Args],cll(Lc,lbl(Prog,Arity),Args)) ;
   concat(Extra,Args,XArgs),
   length(XArgs,ArXX),
   ClEntry = fnDef(Lc,lbl(Closure,Ar),TTp,XArgs,cll(Lc,lbl(Prog,ArXX),XArgs))).
					%  dispRuleSet(ClEntry).

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
liftPtn(enm(Lc,Nm,_),Ptn,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,Ptn,Q,Qx,Map,Opts).
liftPtn(void,voyd,Q,Q,_,_,Ex,Ex):-!.
liftPtn(intLit(Ix,_),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(floatLit(Ix,_),float(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(stringLit(Sx,_),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
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
  genVar("_",Vr),
  liftExp(XX,Val,Q,Qx,Map,Opts,Ex,Exx).

trVarPtn(_,"_",idnt("_"),Q,Q,_,_).
trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts) :-
  lookupVarName(Map,Nm,V),!,
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
  lookupVarName(Map,Nm,V),
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
liftExp(intLit(Ix,_),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(floatLit(Ix,_),float(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(stringLit(Ix,_),strg(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(tple(_,A),TApl,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,TA,[],Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(TA,TApl).
liftExp(apply(Lc,Op,tple(_,A),_),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,LA,[],Q,Q1,Map,Opts,Ex,Ex1),
  trExpCallOp(Lc,Op,LA,Exp,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(case(Lc,Bnd,Cses,_),Result,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(Bnd,Bound,Q,Q0,Map,Opts,Ex,Ex0),
  liftCases(Cses,Cases,Q0,Qx,Map,Opts,Ex0,Exx),
  (idnt(_)=Bound ->
   caseMatcher(Lc,Bound,Cases,Map,Result) ;
   genVar("_C",V),
   caseMatcher(Lc,V,Cases,Map,Res),
   Result = ltt(Lc,V,Bound,Res)).
liftExp(cell(Lc,In),ecll(Lc,"_cell",[CellV]),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(In,CellV,Q,Qx,Map,Opts,Ex,Exx).
liftExp(assign(Lc,Vr,Vl),ecll(Lc,"_assign",[VVr,Val]),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Vr,VVr,Q,Q0,Map,Opts,Ex,Ex1),
  liftExp(Vl,Val,Q0,Qx,Map,Opts,Ex1,Exx).
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
liftExp(letExp(Lc,Defs,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetExp(Lc,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(letRec(Lc,Defs,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetRec(Lc,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(lambda(Lc,Lbl,Rle,Tp),Rslt,Q,Q,Map,Opts,Ex,Exx) :-!,
  liftLambda(lambda(Lc,Lbl,Rle,Tp),Rslt,Q,Map,Opts,Ex,Exx).
liftExp(abstraction(Lc,Bnd,Cond,Zed,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftAbstraction(abstraction(Lc,Bnd,Cond,Zed,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx).
liftExp(doTerm(Lc,Action),doAct(Lc,Act),Q,Q,Map,Opts,Ex,Exx) :-
  liftAction(Action,Act,Q,_,Map,Opts,Ex,Exx).
liftExp(XX,void,Q,Q,_,_,Ex,Ex) :-
  locOfCanon(XX,Lc),
  reportFatal("internal: cannot transform %s as expression",[XX],Lc).

liftLetExp(Lc,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  genVar("_ThR",ThVr),
  recordMap(Lc,Defs,Bnd,ThVr,Q,Map,Opts,ThMap,RMap,FreeTerm),
%  (is_member(showTrCode,Opts) -> dispMap("Record map: ",ThMap);true),
  transformThetaDefs(ThMap,RMap,Opts,Defs,[],Fx,Ex,Ex1),
  liftExp(Bnd,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp).

liftLetRec(Lc,Defs,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  genVar("_ThV",ThVr),
  thetaMap(Lc,Defs,Bnd,ThVr,Q,Map,Opts,ThMap,FreeTerm),
%  (is_member(showTrCode,Opts) -> dispMap("Theta map: ",ThMap);true),
  transformThetaDefs(ThMap,ThMap,Opts,Defs,[],Fx,Ex,Ex1),
  liftExp(Bnd,BExpr,Q,Qx,ThMap,Opts,Ex1,Exx),
  mkFreeLet(Lc,ThVr,FreeTerm,Fx,BExpr,Exp).

mkFreeLet(Lc,Vr,Fr,Ups,Exp,AExp) :-
  computeFreeVect(Lc,Vr,Fr,Ups,Exp,AExp).

computeFreeVect(Lc,Vr,Fr,[],Exp,ltt(Lc,Vr,Fr,Exp)).
computeFreeVect(Lc,Vr,ctpl(Lbl,Args),[(_,Ix,Term)|Ups],Exp,Reslt) :-
  \+idInTerm(Vr,Term),!,
  split_list(Ix,Args,F,[voyd|R]),
  concat(F,[Term|R],NArgs),
  computeFreeVect(Lc,Vr,ctpl(Lbl,NArgs),Ups,Exp,Reslt).
computeFreeVect(Lc,Vr,Fr,[(Lc1,Ix,Term)|Ups],Exp,Reslt) :-
  computeFreeVect(Lc,Vr,Fr,Ups,seq(Lc,setix(Lc1,Vr,Ix,Term),Exp),Reslt).
  
trVarExp(Lc,Nm,Exp,Map,Opts,Q,Qx) :-
  lookupVarName(Map,Nm,V),!,
  implementVarExp(V,Lc,Nm,Exp,Map,Opts,Q,Qx).
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
implementVarExp(moduleFun(_,Closure,_),_,_,ctpl(lbl(Closure,1),[Unit]),_,_,Q,Q) :-
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
  lookupFunName(Map,Nm,Reslt),
  Reslt\=notInMap,!,
  implementFunCall(Lc,Reslt,Nm,Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,enm(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  trExpCallOp(Lc,v(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,cons(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  trExpCallOp(Lc,v(Lc0,Nm,Tp),Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,Op,A,ocall(Lc,Rc,A),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Op,Rc,Q,Qx,Map,Opts,Ex,Exx).

mkBinds(Lc,[],[],Exp,perf(Lc,Exp)).
mkBinds(Lc,[P|Ps],[A|As],Exp,Reslt) :-
  mkBinds(Lc,Ps,As,seq(Lc,varD(Lc,P,A),Exp),Reslt).

implementFunCall(Lc,localFun(Fn,_,Ar,ThVr),_,Args,cll(Lc,lbl(Fn,Ar2),XArgs),Q,Qx,Map,Opts,Ex,Ex) :-
  liftVar(Lc,ThVr,Vr,Map,Opts,Q,Qx),
  concat([Vr],Args,XArgs),
  Ar2 is Ar+1.
implementFunCall(Lc,moduleFun(Fn,_,Ar),_,Args,cll(Lc,lbl(Fn,Ar),Args),Qx,Qx,_,_,Ex,Ex).
implementFunCall(_,moduleCons(Mdl,_,Ar),_,Args,ctpl(lbl(Mdl,Ar),Args),Q,Q,_,_,Ex,Ex).
implementFunCall(Lc,notInMap,Nm,Args,ocall(Lc,idnt(Nm),Args),Q,Q,_Map,_Opts,Ex,Ex) :-
  reportError("cannot compile unknown function %s",[Nm],Lc).

liftCases([],[],Qx,Qx,_Map,_Opts,Exx,Exx) :- !.
liftCases([C|Cses],[Case|Cases],Q,Qx,Map,Opts,Ex,Exx) :-
  liftCase(C,Case,Q,Q0,Map,Opts,Ex,Ex0),
  liftCases(Cses,Cases,Q0,Qx,Map,Opts,Ex0,Exx).

liftCase(equation(Lc,P,G,Value),(Lc,[Ptn],Test,Rep),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,Ptn,Q,Q0,Map,Opts,Ex,Ex0),
  liftGuard(G,Test,Q0,Q1,Map,Opts,Ex0,Ex1),   % condition goals
  liftExp(Value,Rep,Q1,Qx,Map,Opts,Ex1,Exx).  % replacement expression

liftLambda(lambda(Lc,LamLbl,Eqn,Tp),Closure,Q,Map,Opts,[LamFun|Ex],Exx) :-
  lambdaMap(lambda(Lc,LamLbl,Eqn,Tp),LamLbl,Q,Map,Opts,Closure,LMap),
%  reportMsg("lift lambda %s",[lambda(Lc,LamLbl,Eqn,Tp)],Lc),
%  dispMap("lambda map",LMap),
  transformEqn(Eqn,LMap,LMap,Opts,LamLbl,Rls,[],Ex,Exx),
  is_member((_,Args,_,_),Rls),!,
  length(Args,Ar),
  functionMatcher(Lc,Ar,lbl(LamLbl,Ar),Tp,Rls,Map,LamFun).

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

liftAction(seqDo(Lc,E1,E2),seq(Lc,L1,L2),Q,Qx,Map,Opts,Ex,Exx) :-
  liftAction(E1,L1,Q,Q0,Map,Opts,Ex,Ex1),
  liftAction(E2,L2,Q0,Qx,Map,Opts,Ex1,Exx).
liftAction(varDo(Lc,P,E,_,_,_),varD(Lc,P1,E1),Q,Q,Map,Opts,Ex,Exx) :-
  liftPtn(P,P1,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(E,E1,Q0,_,Map,Opts,Ex0,Exx).
liftAction(performDo(Lc,Exp,_,_,_),perf(Lc,E1),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Exp,E1,Q,Qx,Map,Opts,Ex,Exx).

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

pickVarDefs([],_,_,[],Q,Q,Ex,Ex).
pickVarDefs([varDef(_Lc,Nm,_ExtNm,_,_Tp,Val)|Defs],Map,Opts,[(Nm,Value)|Els],Q,Qx,Ex,Exx) :-
  liftExp(Val,Value,Q,Q0,Map,Opts,Ex,Ex1),
  pickVarDefs(Defs,Map,Opts,Els,Q0,Qx,Ex1,Exx).
pickVarDefs([funDef(Lc,Nm,_ExtNm,_Tp,_,_)|Defs],Map,Opts,[(Nm,Value)|Els],Q,Qx,Ex,Exx) :-
  trVarExp(Lc,Nm,Value,Map,Opts,Q,Q0),
  pickVarDefs(Defs,Map,Opts,Els,Q0,Qx,Ex,Exx).
pickVarDefs([_|Defs],Map,Opts,Els,Q,Qx,Ex,Exx) :-
  pickVarDefs(Defs,Map,Opts,Els,Q,Qx,Ex,Exx).

pickTypes([],[]).
pickTypes([varDef(_Lc,Nm,_ExtNm,_,Tp,_)|Defs],[(Nm,Tp)|Els]) :-
  pickTypes(Defs,Els).
pickTypes([funDef(_Lc,Nm,_ExtNm,Tp,_,_)|Defs],[(Nm,Tp)|Els]) :-
  pickTypes(Defs,Els).
pickTypes([_|Defs],Els) :-
  pickTypes(Defs,Els).

liftEls([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftEls([(_,P)|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftEls(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

thetaMap(Lc,Defs,Bnd,ThVr,Q,Map,Opts,[lyr(Lx,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVars(letRec(Lc,Defs,Bnd),Map,Q,ThFree),
  cellVars(Defs,CellVars),
  concat(CellVars,ThFree,FreeVars),
  collectLabelVars(FreeVars,ThVr,0,[],L0),
  makeConstructorMap(Defs,consMap{},_,ConsMap),
  makeMtdMap(Defs,ThVr,CellVars,ConsMap,L0,Lx),
  makeFreeTerm(CellVars,Lc,ThFree,Map,Opts,FreeTerm).

recordMap(Lc,Defs,Bnd,ThVr,Q,Map,Opts,[lyr(Lx,ConsMap,ThVr)|Map],[lyr(L0,ConsMap,ThVr)|Map],FreeTerm) :-
  findFreeVars(letExp(Lx,Defs,Bnd),Map,Q,ThFree),
  cellVars(Defs,CellVars),
  concat(CellVars,ThFree,FreeVars),
  collectLabelVars(FreeVars,ThVr,0,[],L0),
  makeConstructorMap(Defs,consMap{},_,ConsMap),
  makeMtdMap(Defs,ThVr,CellVars,ConsMap,L0,Lx),
  makeFreeTerm(CellVars,Lc,ThFree,Map,Opts,FreeTerm).

lambdaMap(Lam,LamLbl,Q,Map,Opts,ctpl(lbl(LamLbl,1),[FreeTerm]),
    [lyr(Lx,consMap{},ThVr)|Map]) :-
  findFreeVars(Lam,Map,Q,LmFree),
  genVar("_ΛV",ThVr),
  collectLabelVars(LmFree,ThVr,0,[],Lx),
  locOfCanon(Lam,Lc),
  makeFreeTerm([],Lc,LmFree,Map,Opts,FreeTerm).

findFreeVars(Term,Map,Q,LmFr0) :-
  definedProgs(Map,Df),
  labelVars(Map,Lv),
  merge(Lv,Q,Q1),
  merge(Df,Q1,Q2),
  freeVars(Term,Df,Q2,[],ThFr),
  freeLabelVars(Q2,Map,ThFr,LmFr0).

freeLabelVars([],_,Fr,Fr).
freeLabelVars([idnt(Nm)|Lv],Map,Fr,LmFr) :-
  lookupThetaVar(Map,Nm,ThVr),!,
  merge([ThVr],Fr,Fr1),
  freeLabelVars([ThVr|Lv],Map,Fr1,LmFr).
freeLabelVars([_|Lv],Map,Fr,LmFr) :-
  freeLabelVars(Lv,Map,Fr,LmFr).

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

thetaLbl(theta(_,Path,_,_),Path).
thetaLbl(record(_,Path,_,_),Path).

makeMtdMap(Defs,ThVr,CellVars,ConsMap,L,Lx) :-
  collectMtds(Defs,ThVr,CellVars,ConsMap,L,Lx).

collectMtds([],_,_,_,List,List).
collectMtds([Entry|Defs],ThVr,CellVars,ConsMap,List,Lx) :-
  collectMtd(Entry,ThVr,CellVars,ConsMap,List,L0),
  collectMtds(Defs,ThVr,CellVars,ConsMap,L0,Lx).

collectMtd(funDef(_Lc,Nm,LclName,Tp,_,_),ThV,_,_,List,
      [(Nm,localFun(LclName,ClosureName,Ar,ThV))|List]) :-
  localName(LclName,closure,ClosureName),
  progTypeArity(Tp,Ar).
collectMtd(varDef(_Lc,Nm,_LclName,_,_Tp,_),_ThV,CellVars,_,List,List) :-
  is_member(idnt(Nm),CellVars),!.
collectMtd(varDef(Lc,Nm,_LclNm,_,_Tp,Value),_,_,_,Ls,[(Nm,localVar(Lc,Value))|Ls]) :-!.
collectMtd(typeDef(_,_,Tp,_,_),_,_,ConsMap,Ls,[(TpNm,moduleType(TpNm,Tp,IxMap))|Ls]) :-
  tpName(Tp,TpNm),
  (findConsMap(TpNm,ConsMap,IxMap);IxMap=[]),!.
collectMtd(cnsDef(_Lc,Nm,Cons),_ThV,_,_,List,
	   [(Nm,moduleCons(LclName,Tp,Ar)),
	    (LclName,moduleCons(LclName,Tp,Ar))|List]) :-
  constructorName(Cons,LclName),
  constructorType(Cons,Tp),
  progTypeArity(Tp,Ar).
collectMtd(accDef(Tp,Fld,Mtd,FTp),_ThV,_,_,List,
	   [(Fld,fieldAcc(TpNm,Mtd,FTp))|List]) :-
  tpName(Tp,TpNm),!.
collectMtd(implDef(_,_,_,_),_,_,_,List,List).
collectMtd(conDef(_,_,_,_,_),_,_,_,List,List).

collectLabelVars([],_,_,List,List).
collectLabelVars([idnt(Nm)|Args],ThVr,Ix,List,Lx) :-
  Ix1 is Ix+1,
  collectLabelVars(Args,ThVr,Ix1,[(Nm,labelArg(idnt(Nm),Ix,ThVr))|List],Lx).
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

programAccess(moduleFun(Prog,Closure,Arity),Prog,Closure,Arity).
programAccess(localFun(Prog,Closure,Arity,_),Prog,Closure,Arity).

makeDotLbl(Nm,lbl(Nm,0)).

mkUnit(U) :-
  mkTpl([],U).

