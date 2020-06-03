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

transformProg(prog(pkg(Pkg,Vers),Lc,Imports,Defs,Fields,Types,Contracts,Impls),
    Opts,mdule(pkg(Pkg,Vers),Imports,faceType(Fields,Types),Classes,Dfs,Contracts,Impls)) :-
  makePkgMap(Pkg,Defs,Types,Imports,Classes,Map),
  mkUnit(I),
  transformModuleDefs(Defs,Pkg,Map,Opts,I,Ix,Dfs,D0),
  packageInit(Pkg,Lc,Map,Opts,Imports,Ix,D0,[]).

makePkgMap(Pkg,Defs,Types,Imports,Enums,[lyr(Pkg,DfList,void,void)]) :-
  makeModuleMap(Pkg,Defs,DfList,Rest,Enums),
  makeImportsMap(Imports,Rest,R0),
  makeTypesMap(Pkg,Types,R0,[]).

makeModuleMap(Pkg,[Def|Rest],Map,Mx,Enums) :-
  makeMdlEntry(Pkg,Def,Map,M0,Enums,Clx),
  makeModuleMap(Pkg,Rest,M0,Mx,Clx).
makeModuleMap(_,[],Map,Map,[]).

makeMdlEntry(Pkg,funDef(_,Nm,LclName,Tp,_,_),[(Nm,moduleFun(LclName,ClosureName,Ar))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"^",Nm,ClosureName),
  progTypeArity(Tp,Ar).
makeMdlEntry(_Pkg,varDef(_,Nm,LclName,_,_,_),[(Nm,moduleVar(LclName))|Mx],Mx,Clx,Clx).
makeMdlEntry(Pkg,cnsDef(_,Nm,cons(_,_,_),Tp),[(Nm,moduleCons(LclName,AccessName,Ar))|Mx],Mx,[LclName|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName),
  progTypeArity(Tp,Ar).
makeMdlEntry(Pkg,cnsDef(_,Nm,enm(_,_,_),_),[(Nm,moduleCons(LclName,AccessName,0))|Mx],Mx,[LclName|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName).
makeMdlEntry(Pkg,typeDef(_,Nm,Tp,_),[(Nm,moduleType(Pkg,LclName,Tp))|Mx],Mx,Clx,Clx) :-
  packageTypeName(Pkg,Nm,LclName).
makeMdlEntry(_,implDef(_,_,ImplNm,Tp),[(ImplNm,Entry)|Mx],Mx,Clx,Clx) :- makeImplEntry(ImplNm,Tp,Entry).
makeMdlEntry(_,_,Mx,Mx,Clx,Clx).

makeImplEntry(ImplNm,Tp,Entry) :-
  estimateImplArity(Tp,Ar),
  (Ar=0 -> Entry=moduleVar(ImplNm) ; Entry=moduleFun(ImplNm,ImplNm,Ar)).

estimateImplArity(allType(_,Tp),Ar) :- estimateImplArity(Tp,Ar).
estimateImplArity(constrained(T,_),Ar) :- estimateImplArity(T,A), Ar is A+1.
estimateImplArity(_,0).

makeImportsMap([Import|Rest],Map,Mx) :-
  makeImportMap(Import,Map,M0),
  makeImportsMap(Rest,M0,Mx).
makeImportsMap([],Map,Map).

makeImportMap(import(_,pkg(Pkg,_),faceType(Fields,Types),Enums,_,Impls),Map,Mx) :-
  importFields(Pkg,Enums,Fields,Map,M0),
  importImplementations(Impls,M0,M1),
  importTypes(Types,M1,Mx).

importFields(_,_,[],Map,Map).
importFields(Pkg,Enums,[(Nm,Tp)|Fields],Map,Mx) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,Template),
  progTypeArity(Tp,Ar),
  makeImportEntry(Template,Ar,Enums,Pkg,Nm,Map,M0),
  importFields(Pkg,Enums,Fields,M0,Mx).

makeImportEntry(funType(_,_),Ar,_,Pkg,Nm,[(Nm,moduleFun(LclName,ClosureName,Ar))|Mx],Mx) :-
  packageVarName(Pkg,Nm,LclName),
  localName(Pkg,"^",Nm,ClosureName).
makeImportEntry(consType(_,_),Ar,_,Pkg,Nm,[(Nm,moduleCons(LclName,AccessName,Ar))|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName).
makeImportEntry(_,_,Enums,Pkg,Nm,[(Nm,moduleCons(LclName,AccessName,0))|Mx],Mx) :-
  marker(class,Mrk),
  localName(Pkg,Mrk,Nm,Enu),
  is_member(Enu,Enums),!,
  localName(Pkg,"@",Nm,AccessName),
  localName(Pkg,"#",Nm,LclName).
makeImportEntry(_,_,_,Pkg,Nm,[(Nm,moduleVar(LclName))|Mx],Mx) :-
  packageVarName(Pkg,Nm,LclName).

importImplementations([],Map,Map).
importImplementations([imp(_,FullNm,Con)|L],[(FullNm,moduleVar(FullNm))|M],Mx) :-
  contractArity(Con,0),
  importImplementations(L,M,Mx).
importImplementations([imp(_,FullNm,Con)|L],[(FullNm,moduleFun(FullNm,ClosureName,Ar))|M],Mx) :-
  contractArity(Con,Ar),
  importImplementations(L,M,Mx),
  localName("","^",FullNm,ClosureName).

importTypes(_,L,L).

contractArity(allType(_,Con),Ar) :- contractArity(Con,Ar).
contractArity(constrained(Con,_),Ar) :- contractArity(Con,A), Ar is A+1.
contractArity(contractExists(_,_),0).

contractStruct(0,Nm,enum(Nm)).
contractStruct(Ar,Nm,lbl(Nm,Ar)).

makeTypesMap(_,_,List,List).

transformModuleDefs([],_,_,_,I,I,Ex,Ex).
transformModuleDefs([Def|Defs],Pkg,Map,Opts,I,Ix,Ex,Exx) :-
  transformMdlDef(Def,Pkg,Map,Opts,I,I1,Ex,Ex1),
  transformModuleDefs(Defs,Pkg,Map,Opts,I1,Ix,Ex1,Exx).

transformMdlDef(funDef(Lc,Nm,ExtNm,Tp,[],Eqns),_,Map,Opts,Ix,Ix,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,Tp,Eqns,Map,Map,Opts,Dx,Dxx).
transformMdlDef(varDef(Lc,Nm,ExtNm,[],Tp,Value),_,Map,Opts,I,Ix,Dx,Dxx) :-
  transformGblDefn(Lc,Nm,ExtNm,Tp,Value,Map,Opts,I,Ix,Dx,Dxx).
transformMdlDef(cnsDef(_Lc,_Nm,enm(_,_,_),_Tp),_Pkg,_Map,_Opts,Ix,Ix,Dx,Dx).
transformMdlDef(cnsDef(Lc,Nm,cons(_,_,_),Tp),_Pkg,Map,Opts,Ix,Ix,D,Dx) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,Template),
  transformConsDef(Lc,Nm,Template,Map,Opts,D,Dx).
transformMdlDef(typeDef(_,_,_,_),_,_,_,Ix,Ix,Dx,Dx).
transformMdlDef(conDef(_,_,_),_,_,_,Ix,Ix,Dx,Dx).
transformMdlDef(implDef(_,_,_,_),_,_,_,Ix,Ix,Dx,Dx).

extraArity(Arity,Vars,ExAr) :-
  length(Vars,E),
  ExAr is E+Arity.

transformConsDef(Lc,Nm,consType(faceType(Els,Tps),Tp),Map,Opts,[CFun|D],Dx) :-
  lookupVarName(Map,Nm,Reslt),
  programAccess(Reslt,LclName,ConsNm,Arity),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = lbl(LclName,Ar),
  sort(Els,transform:cmpName,SrtEls),
  genConsArgs(Lc,SrtEls,Args,Extra,BndArgs,Extra),
  extendFunTp(funType(tupleType([]),Tp),Args,ATp),
  CFun=fnDef(Lc,LclPrg,ATp,Args,ctpl(lbl(ConsNm,Ar),BndArgs)),
  genConsAccessDef(Lc,LclName,faceType(Els,Tps),Map,Opts,D,Dx).
transformConsDef(Lc,Nm,consType(tupleType(Els),Tp),Map,_Opts,[CFun|D],D) :-
  lookupVarName(Map,Nm,Reslt),
  programAccess(Reslt,LclName,ConsNm,Arity),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = lbl(LclName,Ar),
  genConsArgs(Lc,Els,Args,Extra,BndArgs,Extra),
  extendFunTp(funType(tupleType([]),Tp),Args,ATp),
  CFun=fnDef(Lc,LclPrg,ATp,Args,ctpl(lbl(ConsNm,Ar),BndArgs)).
transformConsDef(Lc,Nm,consType(existType(_,Inn),Tp),Map,Opts,D,Dx) :-
  transformConsDef(Lc,Nm,consType(Inn,Tp),Map,Opts,D,Dx).

genConsAccessDef(Lc,ConsNm,faceType(Els,Tps),_Map,_Opts,[rcDef(Lc,ConsNm,faceType(Els,Tps))|Dx],Dx).

genConsArgs(_,[],Args,Args,BndArgs,BndArgs).
genConsArgs(Lc,[_|Els],[V|Args],Ax,[V|Bnd],Bx) :-
  genVar("_",V),
  genConsArgs(Lc,Els,Args,Ax,Bnd,Bx).

transformFunction(Lc,Nm,LclName,Tp,Eqns,Map,OMap,Opts,[Fun|Ex],Exx) :-
  lookupFunName(Map,Nm,Reslt),
  programAccess(Reslt,_,_,Arity),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = lbl(LclName,Ar),
  extendFunTp(Tp,Extra,ATp),
%  reportMsg("show eqns for %s",[Nm],Lc),
%  dispEqns(Nm,Eqns),
  transformEquations(Map,OMap,Opts,LclPrg,Eqns,Rules,[],Ex,Ex0),
%  reportMsg("show transformed rules for %s",[Nm],Lc),
%  dispRules(Nm,Rules),
  closureEntry(Map,Lc,Nm,Tp,Ex0,Exx),
  functionMatcher(Lc,Ar,LclPrg,ATp,Rules,Fun).
%  dispRuleSet(Fun),
%  liftExp(v(Lc,Nm,Tp),Vr,[],_,Map,Opts,Exx,_).
%  dispTerm(Vr).

dispEqns(Nm,Eqs) :-
  showEqns(Nm,Eqs,Chrs,[]),
  string_chars(Res,Chrs), write(Res).  

dispRules(Nm,Rls) :-
  showRules(Nm,Rls,Chrs,[]),
  string_chars(Res,Chrs), write(Res).  

showRules(_,[],Cx,Cx).
showRules(Nm,[Rl|Rls],C,Cx) :-
  showRule(Nm,Rl,C,C0),
  showRules(Nm,Rls,C0,Cx).

showRule(Nm,(_,Args,Cnd,Val),C,Cx) :-
  appStr(Nm,C,C0a),
  showArgs(Args,0,C0a,C0),
  appStr(" whr ",C0,C1),
  showTerm(Cnd,0,C1,C2),
  appStr(" => ",C2,C3),
  showTerm(Val,0,C3,C4),
  appStr("\n",C4,Cx).

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

transformEqn(equation(Lc,tple(_,A),Cond,Value),Map,OMap,Opts,_LclPrg,
    [(Lc,Args,Test,Rep)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),
  filterVars(Extra,Q0),
  liftPtns(A,AA,Q0,Q1,Map,Opts,Ex,Ex0), % head args
  concat(Extra,AA,Args),
  liftGoal(Cond,Test,Q1,Q2,OMap,Opts,Ex0,Ex1),   % condition goals
  liftExp(Value,Rep,Q2,_Q3,OMap,Opts,Ex1,Exx).  % replacement expression

transformGblDefn(Lc,_Nm,LclName,Tp,Value,Map,Opts,I,Ix,
    [vrDef(Lc,LclName,Tp,Rep)|Dx],Dxx) :-
  extraVars(Map,[]),!,                                 % no extra variables coming from labels
  liftExp(Value,Rep,[],_Q0,Map,Opts,Dx,Dxx),
  mergeSeq(Lc,I,cll(Lc,lbl(LclName,0),[]),Ix).
  %dispRuleSet(vrDef(Lc,LclName,Tp,Rep)).

transformThetaDefn(Lc,Nm,_LclName,_Tp,Exp,Map,OMap,Opts,F,Reslt,Dx,Dxx) :-
  \+isSimpleCanon(Exp),!,
  liftExp(Exp,Rep,[],_Qx,OMap,Opts,Dx,Dxx),
  lookupVarName(Map,Nm,labelArg(_,Ix,ThVr)),
  updateTerm(F,Ix,Lc,Rep,ThVr,Reslt).
transformThetaDefn(_Lc,_Nm,_LclName,_Tp,_Exp,_Map,_OMap,_Opts,F,F,Dx,Dx).

updateTerm(ctpl(Lbl,Args),Ix,_,Term,ThVr,ctpl(Lbl,NArgs)) :-
  split_list(Ix,Args,F,[voyd|R]),
  refactorTerm(ThVr,ctpl(Lbl,Args),Term,Trm1),
  concat(F,[Trm1|R],NArgs).
updateTerm(In,Ix,Lc,Term,ThVr,ecll(Lc,"_tuple_set_nth",[In,intgr(Ix),Term1])) :-
  refactorTerm(ThVr,In,Term,Term1).

% This works without blowing up because free terms are always relatively flat.
refactorTerm(ThVr,Fx,Term,T1) :-
  rewriteTerm(transform:rewriteFrV(ThVr,Fx),Term,T1).

rewriteFrV(Vr,ctpl(_,Args),dte(_Lc,Vr,intgr(Ix)),Out) :-
  split_list(Ix,Args,_,[Out|_]).

transformOthers(_,_,_,[],I,I,Rx,Rx).
transformOthers(Pkg,Map,Opts,[assertion(Lc,G)|Others],I,Ix,Rules,Rx) :-
  collect(Others,canon:isAssertion,Asserts,Rest),
  transformAssertions(Pkg,Map,Opts,Lc,[assertion(Lc,G)|Asserts],AssertName,Rules,R0),
  mergeGoal(I,cll(Lc,AssertName,[]),Lc,I1),
  transformOthers(Pkg,Map,Opts,Rest,I1,Ix,R0,Rx).
transformOthers(Pkg,Map,Opts,[show(Lc,G)|Others],I,Ix,Rules,Rx) :-
  collect(Others,canon:isShow,Shows,Rest),
  transformShows(Pkg,Map,Opts,Lc,[show(Lc,G)|Shows],ShowName,Rules,R0),
  mergeGoal(I,cll(Lc,ShowName,[]),Lc,I1),
  transformOthers(Pkg,Map,Opts,Rest,I1,Ix,R0,Rx).

transformAssertions(Pkg,Map,Opts,Lc,Asserts,LclPrg,
    [fnDef(Lc,LclPrg,funType(tupleType([]),BoolTp),[],Goal)|Ex],Exx) :-
  rfold(Asserts,transform:collectAssertion,enm(Lc,"true",type("star.core*boolean")),G),
  localName(Pkg,"@","assert",LclName),
  LclPrg = lbl(LclName,0),
  liftExp(G,Goal,[],_Q,Map,Opts,Ex,Exx),
  stdType("boolean",BoolTp,_).

collectAssertion(assertion(Lc,G),enm(_,"true",type("star.core*boolean")),assertion(Lc,G)) :-!.
collectAssertion(assertion(Lc,G),O,conj(Lc,O,assertion(Lc,G))).

transformShows(Pkg,Map,Opts,Lc,Shows,LclPrg,
    [fnDef(Lc,LclPrg,funType(tupleType([]),tupleType([])),[],Disp)|Ex],Exx) :-
  isUnit(Z),
  rfold(Shows,transform:collectShow(Map,Opts),([],Z,Ex),(_Q,Disp,Exx)),
  localName(Pkg,"@","show",LclName),
  LclPrg = lbl(LclName,0).

collectShow(Map,Opts,show(Lc,E),(Q0,Z,Ex),(Q1,Zx,Exx)) :-
  locTerm(Lc,Lx),
  liftExp(E,ETrm,Q0,Q1,Map,Opts,Ex,Exx),
  mergeSeq(Lc,Z,ecll(Lc,"_show",[Lx,ETrm]),Zx).

packageInit(Pkg,Lc,_,_,Imports,Inits,[fnDef(Lc,InitPrg,funType(tupleType([]),tupleType([])),[],Init)|R],R) :-
  localName(Pkg,"@","init",InitNm),
  InitPrg = lbl(InitNm,0),
  importInits(Lc,Imports,II),
  mergeGoal(II,Inits,Lc,Body),
  IV = strg(InitNm),
  Init = cnd(Lc,ecll(Lc,"_isDefinedVr",[IV]),enum("star.core#true"),ecll(Lc,"_defineVr",[IV,Body])).

importInits(_,[],enum("star.core#true")).
importInits(Lc,[import(Viz,pkg(Pkg,_),_,_,_,_)|II],IG) :-
  (Viz=private ; Viz=public),
  localName(Pkg,"@","init",PkgInit),
  importInits(Lc,II,LR),
  mergeGoal(cll(Lc,lbl(PkgInit,0),[]),LR,Lc,IG).
importInits(Lc,[import(transitive,_,_,_,_,_)|II],IG) :-
  importInits(Lc,II,IG).

transformThetaDefs(_,_,_,[],Fx,Fx,Ix,Ix,Dfs,Dfs).
transformThetaDefs(Map,OMap,Opts,[Def|Defs],F,Fx,I,Ix,Ex,Exx) :-
  transformThetaDef(Def,Map,OMap,Opts,F,F1,I,I1,Ex,Ex1),
  transformThetaDefs(Map,OMap,Opts,Defs,F1,Fx,I1,Ix,Ex1,Exx).

transformThetaDef(funDef(Lc,Nm,ExtNm,Tp,_,Eqns),Map,OMap,Opts,Fx,Fx,Ix,Ix,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,Tp,Eqns,Map,OMap,Opts,Dx,Dxx).
transformThetaDef(varDef(Lc,Nm,ExtNm,_,Tp,Value),Map,OMap,Opts,F,Fx,Ix,Ix,Dx,Dxx) :-
  transformThetaDefn(Lc,Nm,ExtNm,Tp,Value,Map,OMap,Opts,F,Fx,Dx,Dxx).
transformThetaDef(cnsDef(_Lc,_Nm,enm(_,_,_),_Tp),_Map,_,_Opts,Fx,Fx,Ix,Ix,Dx,Dx).
transformThetaDef(cnsDef(Lc,Nm,cons(_,_,_),Tp),Map,_,Opts,Fx,Fx,Ix,Ix,D,Dx) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,Template),
  transformConsDef(Lc,Nm,Template,Map,Opts,D,Dx).
transformThetaDef(typeDef(_,_,_,_),_,_,_,Fx,Fx,Ix,Ix,Dx,Dx).
transformThetaDef(conDef(_,_,_),_,_,_,Fx,Fx,Ix,Ix,Dx,Dx).

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
%  length(Extra,ExA),
%  dispRuleSet(ClEntry).

liftPtns([],[],Q,Q,_,_,Ex,Ex) :-!.
liftPtns([P|More],[A|Args],Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftPtns(More,Args,Q0,Qx,Map,Opts,Ex0,Exx).

liftPtn(v(_,"this",_),ThVr,Q,Qx,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
liftPtn(v(Lc,Nm,_),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts).
liftPtn(enm(Lc,Nm,_),A,Q,Qx,Map,_Opts,Ex,Ex) :- !,
  trEnum(Lc,Nm,A,Q,Qx,Map).
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
liftPtn(where(_,P,enm(_,"true",type("star.core*boolean"))),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,Ptn,Q,Qx,Map,Opts,Ex,Exx).
liftPtn(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(XX,whr(Lc,Vr,mtch(Lc,Vr,Val)),Q,Qx,Map,Opts,Ex,Exx) :-
  locOfCanon(XX,Lc),
  genVar("_",Vr),
  liftExp(XX,Val,Q,Qx,Map,Opts,Ex,Exx).

trVarPtn(_,"_",idnt("_"),Q,Q,_,_).
trVarPtn(Lc,Nm,A,Q,Qx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  implementVarPtn(V,Nm,Lc,A,Map,Q,Qx).

implementVarPtn(localVar(Vn,_,ThVr),_,Lc,cll(Lc,lbl(Vn,1),[Vr]),Map,Q,Qx) :- !, % instance var
  liftVar(Lc,ThVr,Map,Vr,Q,Qx).
implementVarPtn(moduleVar(Vn),_,Lc,cll(Lc,lbl(Vn,0),[]),_,Q,Q) :-
  reportError("not allowed to have globals in patterns: %w",[Vn],Lc). % module variable
implementVarPtn(labelArg(N,Ix,ThVr),_,Lc,whr(Lc,N,mtch(Lc,N,dte(Lc,Vr,intgr(Ix)))),Map,Q,Qx) :- !,    % argument from label
  liftVar(Lc,ThVr,Map,Vr,Q,Q0),
  merge([N],Q0,Qx).
implementVarPtn(moduleCons(Enum,_,0),_,_,enum(Enum),_,Q,Q).
implementVarPtn(notInMap,Nm,_,idnt(Nm),_,Q,Qx) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(Lc,Nm,Args,whr(Lc,X,mtch(Lc,X,intrinsic(Lc,Op,Args))),Q,Qx,_,_,Ex,Ex) :-
  isIntrinsic(Nm,_,Op),!,
  genVar("_X",X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Args,whr(Lc,X,mtch(Lc,X,ecll(Lc,Nm,Args))),Q,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!,
  genVar("_X",X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Args,Ptn,Q,Qx,Map,_,Ex,Ex) :-
  lookupClassName(Map,Nm,Reslt),
  implementPtnCall(Reslt,Lc,Nm,Args,Ptn,Map,Q,Qx).

implementPtnCall(localFun(Fn,_,_,Ar,ThVr),Lc,_,Args,whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,A2),XArgs))),Map,Q,Qx) :-
  genVar("_X",X),
  liftVar(Lc,ThVr,Map,Vr,Q,Qx),
  concat(Args,[Vr],XArgs),
  merge([X],Q,Qx),
  A2 is Ar+1.
implementPtnCall(moduleFun(Fn,_,Ar),Lc,_,Args,whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,Ar),Args))),_,Q,Qx) :-
  genVar("_X",X),
  merge([X],Q,Qx).
implementPtnCall(moduleCons(Mdl,_,Ar),_,_,Args,ctpl(lbl(Mdl,Ar),Args),_,Q,Q).

trEnum(Lc,Nm,A,Q,Qx,Map) :-
  lookupVarName(Map,Nm,V),!,
  implementEnum(V,Nm,Lc,A,Map,Q,Qx).

implementEnum(moduleCons(Enum,_,0),_,_,enum(Enum),_,Q,Q).

liftExps([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftExps([P|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftExps(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

liftExp(v(Lc,Nm,_),Vr,Q,Qx,Map,_Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Map).
liftExp(enm(Lc,Nm,_),Vr,Q,Qx,Map,_Opts,Ex,Ex) :- !,
  trEnum(Lc,Nm,Vr,Q,Qx,Map).
liftExp(cons(Lc,Nm,_),Vr,Q,Qx,Map,_Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Vr,Q,Qx,Map).
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
   caseMatcher(Lc,Bound,Cases,Result) ;
   genVar("_C",V),
   caseMatcher(Lc,V,Cases,Res),
   Result = ltt(Lc,V,Bound,Res)).
liftExp(dot(Lc,Rec,Fld,_),dte(Lc,Rc,Lbl),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(Rec,Rc,Q,Qx,Map,Opts,Ex,Exx),
  makeDotLbl(Fld,Lbl).
liftExp(dot(Lc,Rec,Fld,_),ocall(Lc,Rc,[Lbl]),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(Rec,Rc,Q,Qx,Map,Opts,Ex,Exx),
  makeDotLbl(Fld,Lbl).
liftExp(cell(Lc,In),ecll(Lc,"_cell",[CellV]),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(In,CellV,Q,Qx,Map,Opts,Ex,Exx).
liftExp(assign(Lc,Vr,Vl),ecll(Lc,"_assign",[VVr,Val]),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Vr,VVr,Q,Q0,Map,Opts,Ex,Ex1),
  liftExp(Vl,Val,Q0,Qx,Map,Opts,Ex1,Exx).
liftExp(where(_,E,enm(_,"true",_)),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
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
liftExp(theta(Lc,Path,Defs,Sig),ltt(Lc,ThVr,FrTrm,Recrd),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftTheta(theta(Lc,Path,Defs,Sig),ThVr,FrTrm,_ThCond,Q,Map,ThMap,Opts,Ex,Ex1),
  genRecord(Lc,Path,Defs,ThMap,Opts,Q,Qx,Recrd,Ex1,Exx).
liftExp(record(Lc,Path,Defs,Sig),ltt(Lc,ThVr,FrTrm,Recrd),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftTheta(record(Lc,Path,Defs,Sig),ThVr,FrTrm,_ThCond,Q,Map,ThMap,Opts,Ex,Ex1),
  genRecord(Lc,Path,Defs,ThMap,Opts,Q,Qx,Recrd,Ex1,Exx).
liftExp(letExp(Lc,Th,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetExp(Lc,Th,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
  % dispTerm(Exp).
liftExp(lambda(Lc,Rle,Tp),Rslt,Q,Q,Map,Opts,Ex,Exx) :-!,
  liftLambda(lambda(Lc,Rle,Tp),Rslt,Q,Map,Opts,Ex,Exx).
liftExp(abstraction(Lc,Bnd,Cond,Zed,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftAbstraction(abstraction(Lc,Bnd,Cond,Zed,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx).
liftExp(doTerm(Lc,Action),doAct(Lc,Act),Q,Q,Map,Opts,Ex,Exx) :-
  liftAction(Action,Act,Q,_,Map,Opts,Ex,Exx).
liftExp(XX,void,Q,Q,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trVarExp(Lc,Nm,Exp,Q,Qx,Map) :-
  lookupVarName(Map,Nm,V),!,
  implementVarExp(V,Lc,Nm,Exp,Map,Q,Qx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,_) :-
  reportError("'%s' not defined",[Nm],Lc).

liftVar(_,Vr,Map,Vr,Q,Qx):-
  thisVar(Map,Vr),!,
  merge([Vr],Q,Qx).
liftVar(Lc,idnt(Nm),Map,Vr,Q,Qx) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Map).

implementVarExp(localVar(Vn,_,ThVr),Lc,_,cll(Lc,lbl(Vn,1),[Vr]),Map,Q,Qx) :-
  liftVar(Lc,ThVr,Map,Vr,Q,Qx).
implementVarExp(moduleVar(V),_Lc,_,idnt(V),_,Qx,Qx).
implementVarExp(labelArg(_N,Ix,ThVr),Lc,_,dte(Lc,ThV,intgr(Ix)),Map,Q,Qx) :-
  liftVar(Lc,ThVr,Map,ThV,Q,Qx).
implementVarExp(moduleCons(Enum,_,0),_,_,enum(Enum),_,Q,Q).
implementVarExp(moduleCons(C,_,Ar),_,_,Cns,_,Q,Q) :-
  trCons(C,Ar,Cns).
implementVarExp(notInMap,_,Nm,idnt(Nm),_,Q,Qx) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(moduleFun(_,Closure,_),_,_,ctpl(lbl(Closure,1),[Unit]),_,Q,Q) :-
  mkTpl([],Unit).
implementVarExp(localFun(_Fn,_,Closure,_,ThVr),Lc,_,ctpl(lbl(Closure,1),[Vr]),Map,Q,Qx) :-
  liftVar(Lc,ThVr,Map,Vr,Q,Qx).
implementVarExp(_Other,Lc,Nm,idnt(Nm),_,Q,Q) :-
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
trExpCallOp(_,lambda(Lc,equation(_,Ptns,_Cond,Exp),_),Args,doAct(Lc,Act),
	    Q,Q,Map,Opts,Ex,Exx) :-
  liftPtns(Ptns,LPtns,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(Exp,LExp,Q0,_,Map,Opts,Ex0,Exx),
  mkBinds(Lc,LPtns,Args,LExp,Act).
trExpCallOp(Lc,Op,A,ocall(Lc,Rc,A),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Op,Rc,Q,Qx,Map,Opts,Ex,Exx).

mkBinds(Lc,[],[],Exp,perf(Lc,Exp)).
mkBinds(Lc,[P|Ps],[A|As],Exp,Reslt) :-
  mkBinds(Lc,Ps,As,seq(Lc,varD(Lc,P,A),Exp),Reslt).

implementFunCall(Lc,localFun(Fn,_,_,Ar,ThVr),_,Args,cll(Lc,lbl(Fn,Ar2),XArgs),Q,Qx,Map,_,Ex,Ex) :-
  liftVar(Lc,ThVr,Map,Vr,Q,Qx),
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

liftCase(equation(Lc,P,Cond,Value),(Lc,[Ptn],Test,Rep),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,Ptn,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(Cond,Test,Q0,Q1,Map,Opts,Ex0,Ex1),   % condition goals
  liftExp(Value,Rep,Q1,Qx,Map,Opts,Ex1,Exx).  % replacement expression

liftLambda(lambda(Lc,Eqn,Tp),Closure,Q,Map,Opts,[LamFun|Ex],Exx) :-
  lambdaMap(lambda(Lc,Eqn,Tp),Q,Map,LclName,Closure,LMap),
%  reportMsg("lift lambda %s",[lambda(Lc,Eqn,Tp)],Lc),
%  dispMap("lambda map",LMap),
  transformEqn(Eqn,LMap,LMap,Opts,LclName,Rls,[],Ex,Exx),
  is_member((_,Args,_,_),Rls),!,
  length(Args,Ar),
  functionMatcher(Lc,Ar,lbl(LclName,Ar),Tp,Rls,LamFun).

lambdaLbl(Map,Variant,Nm) :-
  layerName(Map,Prefix),
  genstr(Variant,V),
  localName(Prefix,"@",V,Nm).

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

/* A theta or record is converted to a structure containing free variables */

liftTheta(Theta,ThVr,Fx,ThCond,Q,Map,ThMap,Opts,Ex,Exx) :-
  Theta=theta(Lc,_Path,Defs,_Sig),!,
  genVar("_ThV",ThVr),
  thetaMap(Theta,ThVr,Q,Map,Opts,ThMap,FreeTerm),
%  dispMap("theta map",ThMap),
  transformThetaDefs(ThMap,ThMap,Opts,Defs,FreeTerm,Fx,mtch(Lc,ThVr,Fx),ThCond,Ex,Exx).
%  (is_member(showTrCode,Opts) -> dispTerm(Fx);true).
liftTheta(Theta,ThVr,Fx,ThCond,Q,Map,ThMap,Opts,Ex,Exx) :-
  Theta=record(Lc,_Path,Defs,_Sig),
  genVar("_ThR",ThVr),
  recordMap(Theta,ThVr,Q,Map,Opts,ThMap,RMap,FreeTerm),
%  (is_member(showTrCode,Opts) -> dispMap("Record map: ",RMap);true),
  transformThetaDefs(ThMap,RMap,Opts,Defs,FreeTerm,Fx,mtch(Lc,ThVr,Fx),ThCond,Ex,Exx).
  % dispTerm(Fx).

liftLetExp(Lc,Theta,Bnd,ltt(Lc,ThVr,FrTrm,Expr),Q,Qx,Map,Opts,Ex,Exx) :-
  liftTheta(Theta,ThVr,FrTrm,_ThCond,Q,Map,ThMap,Opts,Ex,Ex1),
  liftExp(Bnd,Expr,Q,Qx,ThMap,Opts,Ex1,Exx).

genRecord(Lc,Path,Defs,Map,Opts,Q,Qx,Exp,Ex,Exx) :-
  pickVarDefs(Defs,Map,Opts,VTerms,Q,Qx,Ex,Ex1),
  sort(VTerms,transform:cmpName,Els),
  project1(Els,Args),
  length(Args,Ar),
  pickTypes(Defs,ElTps),
  sort(ElTps,transform:cmpName,STps),
  Tp =faceType(STps,[]),
  genConsAccessDef(Lc,Path,Tp,Map,Opts,Ex1,Exx),
  Exp = ctpl(lbl(Path,Ar),Args).

cmpName((N1,_),(N2,_)) :- str_lt(N1,N2).

pickVarDefs([],_,_,[],Q,Q,Ex,Ex).
pickVarDefs([varDef(_Lc,Nm,_ExtNm,_,_Tp,Val)|Defs],Map,Opts,[(Nm,Value)|Els],Q,Qx,Ex,Exx) :-
  liftExp(Val,Value,Q,Q0,Map,Opts,Ex,Ex1),
  pickVarDefs(Defs,Map,Opts,Els,Q0,Qx,Ex1,Exx).
pickVarDefs([funDef(Lc,Nm,_ExtNm,_Tp,_,_)|Defs],Map,Opts,[(Nm,Value)|Els],Q,Qx,Ex,Exx) :-
  trVarExp(Lc,Nm,Value,Q,Q0,Map),
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

allVarDefs([],[],[]).
allVarDefs([varDef(_Lc,Nm,_ExtNm,_,_Tp,Value)|Defs],[(Nm,Value)|Els],FunDefs) :-
  allVarDefs(Defs,Els,FunDefs).
allVarDefs([D|Defs],Els,[D|FunDefs]) :-
  allVarDefs(Defs,Els,FunDefs).

liftEls([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftEls([(_,P)|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftEls(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

thetaMap(Theta,ThVr,Q,Map,_Opts,[lyr(LclName,Lx,FreeTerm,ThVr)|Map],FreeTerm) :-
  findFreeVars(Theta,Map,Q,ThFree),
  cellVars(Theta,CellVars),
  thetaLbl(Theta,LclName),
  concat(CellVars,ThFree,FreeVars),
  collectLabelVars(FreeVars,ThVr,0,[],L0),
  makeMtdMap(Theta,LclName,ThVr,L0,Lx),
  locOfCanon(Theta,Lc),
  makeFreeTerm(CellVars,Lc,ThFree,Map,FreeTerm).

recordMap(Theta,ThVr,Q,Map,_Opts,[lyr(LclName,Lx,FreeTerm,ThVr)|Map],[lyr(LclName,L0,FreeTerm,ThVr)|Map],FreeTerm) :-
  findFreeVars(Theta,Map,Q,ThFree),
  cellVars(Theta,CellVars),
  thetaLbl(Theta,LclName),
  concat(CellVars,ThFree,FreeVars),
  collectLabelVars(FreeVars,ThVr,0,[],L0),
  makeMtdMap(Theta,LclName,ThVr,L0,Lx),
  locOfCanon(Theta,Lc),
  makeFreeTerm(CellVars,Lc,ThFree,Map,FreeTerm).

lambdaMap(Lam,Q,Map,LclName,ctpl(lbl(LclName,1),[FreeTerm]),
    [lyr(LclName,Lx,FreeTerm,ThVr)|Map]) :-
  findFreeVars(Lam,Map,Q,LmFree),
  lambdaLbl(Map,"λ",LclName),
  genVar("_ΛV",ThVr),
  collectLabelVars(LmFree,ThVr,0,[],Lx),
  locOfCanon(Lam,Lc),
  makeFreeTerm([],Lc,LmFree,Map,FreeTerm).

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

cellVars(theta(_Lc,_Path,Defs,_Sig),CellVars) :-
  rfold(Defs,transform:pickCellVar,[],CellVars).
cellVars(record(_Lc,_Path,Defs,_Sig),CellVars) :-
  rfold(Defs,transform:pickCellVar,[],CellVars).

pickCellVar(varDef(_,Nm,_,_,Tp,_),F,Fv) :-
  deRef(Tp,refType(_)),!,
  add_mem(idnt(Nm),F,Fv).
pickCellVar(varDef(_,Nm,_,_,_,Val),F,Fv) :-
  \+isSimpleCanon(Val),!,
  add_mem(idnt(Nm),F,Fv).
pickCellVar(_,F,F).

makeFreeTerm(CellVars,Lc,ThFr,Map,FreeTerm) :-
  map(CellVars,transform:emptyCell(Lc),CV),
  map(ThFr,transform:mkFreeVar(Map,Lc),FrExps),
  concat(CV,FrExps,Args),
  mkTpl(Args,FreeTerm).

emptyCell(_Lc,idnt(_),voyd).

mkFreeVar(Map,Lc,idnt(Nm),Vr) :-
  trVarExp(Lc,Nm,Vr,[],_Qx,Map).

refineQ(Q,Qx) :-
  filter(Q,transform:notVar,Qx).

filterVars(Q,Qx) :-
  filterVars(Q,[],Qx).
filterVars(V,Q,Qx) :-
  rfold(V,transform:filterVar,Q,Qx).

filterVar(T,Q,Qx) :-
  isVar(T),
  add_mem(T,Q,Qx).
filterVar(ctpl(_,A),Q,Qx) :-
  filterVars(A,Q,Qx).
filterVar(enum(_),Q,Q).

notVar(V) :- V\=idnt(_).

isVar(idnt(_)).

thetaLbl(theta(_,Path,_,_),Path).
thetaLbl(record(_,Path,_,_),Path).

makeMtdMap(theta(_,_,Defs,_),OuterNm,ThVr,L,Lx) :-
  collectMtds(Defs,OuterNm,ThVr,L,Lx).
makeMtdMap(record(_,_,Defs,_),OuterNm,ThVr,L,Lx) :-
  collectMtds(Defs,OuterNm,ThVr,L,Lx).

collectMtds([],_,_,List,List).
collectMtds([Entry|Defs],OuterNm,ThVr,List,Lx) :-
  collectMtd(Entry,OuterNm,ThVr,List,L0),
  collectMtds(Defs,OuterNm,ThVr,L0,Lx).

collectMtd(funDef(_Lc,Nm,LclName,Tp,_,_),OuterNm,ThV,List,
      [(Nm,localFun(LclName,AccessName,ClosureName,Ar,ThV))|List]) :-
  localName(OuterNm,"%",Nm,AccessName),
  localName(OuterNm,"^",Nm,ClosureName),
  progTypeArity(Tp,Ar).
collectMtd(varDef(_Lc,_Nm,_LclName,_,_Tp,_),_OuterNm,_ThV,List,List) :-!.
collectMtd(typeDef(_,_,_,_),_,_,List,List).
collectMtd(cnsDef(_Lc,Nm,cons(_,_,_),Tp),OuterNm,_ThV,List,[(Nm,moduleCons(LclName,AccessName,Ar))|List]) :-
  localName(OuterNm,"#",Nm,LclName),
  localName(OuterNm,"@",Nm,AccessName),
  progTypeArity(Tp,Ar).
collectMtd(cnsDef(_Lc,Nm,enm(_,_,_),_),OuterNm,_ThV,List,[(Nm,moduleCons(LclName,AccessName,0))|List]) :-
  localName(OuterNm,"#",Nm,LclName),
  localName(OuterNm,"@",Nm,AccessName).

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
    (Lc,[ThVr,DotName],enum("star.core#true"),ctpl(lbl(ClosureName,1),[ThVr]))) :-
  makeDotLbl(Nm,DotName).

accessRule(_,Lc,Nm,LclName,ThV,(Lc,[ThV,DotName],enum("star.core#true"),cll(Lc,lbl(LclName,1),[ThV]))) :-
  makeDotLbl(Nm,DotName).

programAccess(moduleFun(Prog,Closure,Arity),Prog,Closure,Arity).
programAccess(moduleCons(Prog,Closure,Arity),Prog,Closure,Arity).
programAccess(localFun(Prog,_,Closure,Arity,_),Prog,Closure,Arity).
programAccess(localVar(Prog,Closure,_),Prog,Closure,1).

makeDotLbl(Nm,enum(Nm)).

mkUnit(U) :-
  mkTpl([],U).
