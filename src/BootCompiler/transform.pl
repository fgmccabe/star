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
:- use_module(location).
:- use_module(freevars).
:- use_module(terms).
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

  aXX(Lbl,X) where Lbl =. thetaXX(Y)  => X+Y

  Calls through variables access the closure form of functions:

  F(3,4) —> ocall(F,3,4)

  Calling a function from a theta combines:

  A = add(2)
  B = A.a(3)

  becomes
  A = add(2) (= thetaXX(2))

  ocall(ocall(A,'.a'),3)

*/

transformProg(prog(pkg(Pkg,Vers),Lc,Imports,Defs,Others,Fields,Types,Contracts,Impls),
    Opts,mdule(pkg(Pkg,Vers),Imports,faceType(Fields,Types),Classes,Dfs,Contracts,Impls)) :-
  makePkgMap(Pkg,Defs,Types,Imports,Classes,Map),
  mkUnit(I),
  transformModuleDefs(Defs,Pkg,Map,Opts,I,I0,Dfs,D0),
  transformOthers(Pkg,Map,Opts,Others,I0,Ix,D0,D1),
  packageInit(Pkg,Lc,Map,Opts,Imports,Ix,D1,[]).

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
importImplementations([imp(Nm,Con)|L],[(Nm,moduleVar(Nm))|M],Mx) :-
  contractArity(Con,0),
  importImplementations(L,M,Mx).
importImplementations([imp(Nm,Con)|L],[(Nm,moduleFun(Nm,ClosureName,Ar))|M],Mx) :-
  contractArity(Con,Ar),
  importImplementations(L,M,Mx),
  localName("","^",Nm,ClosureName).

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
transformMdlDef(cnsDef(_,_,_,_),_,_,_,Ix,Ix,Dx,Dx).
transformMdlDef(typeDef(_,_,_,_),_,_,_,Ix,Ix,Dx,Dx).
transformMdlDef(conDef(_,_,_),_,_,_,Ix,Ix,Dx,Dx).
transformMdlDef(implDef(_,_,_,_),_,_,_,Ix,Ix,Dx,Dx).

extraArity(Arity,Vars,ExAr) :-
  length(Vars,E),
  ExAr is E+Arity.

transformFunction(Lc,Nm,LclName,Tp,Eqns,Map,OMap,Opts,[Fun|Ex],Exx) :-
  lookupFunName(Map,Nm,Reslt),
  programAccess(Reslt,_,_,Arity),
  pushOpt(Opts,inProg(Nm),FOpts),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = lbl(LclName,Ar),
  extendFunTp(Tp,Extra,ATp),
  transformEquations(Map,OMap,FOpts,LclPrg,Eqns,Rules,[],Ex,Ex0),
  closureEntry(Map,Lc,Nm,Tp,Ex0,Exx),
  functionMatcher(Lc,Ar,LclPrg,ATp,Rules,Fun).

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
    [(Lc,Args,EqTest,Rep)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),
  filterVars(Extra,Q0),
  liftPtns(A,AA,Q0,Q1,Map,Opts,Ex,Ex0), % head args
  concat(Extra,AA,Args),
  liftGoal(Cond,Test,Q1,Q2,OMap,Opts,Ex0,Ex1),   % condition goals
  liftExp(Value,Rep,Q2,Q3,OMap,Opts,Ex1,Exx),  % replacement expression
  labelAccess(Q3,_Q,Map,Lc,LbLx),         % generate label access goals
  mergeGoal(LbLx,Test,Lc,EqTest).

transformGblDefn(Lc,_Nm,LclName,Tp,Value,Map,Opts,I,Ix,
    [vrDef(Lc,LclName,Tp,Exp)|Dx],Dxx) :-
  extraVars(Map,[]),!,                                 % no extra variables coming from labels
  liftExp(Value,Rep,[],Q0,Map,Opts,Dx,Dxx),
  labelAccess(Q0,_Q,Map,Lc,G0),                        % generate label access
  mergeWhere(Rep,G0,Lc,Exp),
  mergeSeq(Lc,I,cll(Lc,lbl(LclName,0),[]),Ix).

transformThetaDefn(Lc,_Nm,LclName,Tp,Value,Map,OMap,Opts,I,I,
    [fnDef(Lc,VrProg,funType(tupleType(ExTps),Tp),Extra,Body)|Dx],Dxx) :-
  extraVars(Map,Extra),                                   % extra variables coming from labels
  liftExp(Value,Rep,[],Q0,OMap,Opts,Dx,Dxx),
  labelAccess(Q0,_Q,Map,Lc,G0),                        % generate label access goals
  length(Extra,Arity),
  ExTps = [],
  VrProg = lbl(LclName,Arity),
  mergeWhere(Rep,G0,Lc,Body).

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

transformCnsDef(Map,Opts,Lc,Nm,enm(_,_,_),Tp,Ex,Exx) :-
  transformEnum(Map,Opts,Lc,Nm,Tp,Ex,Exx).
transformCnsDef(Map,Opts,Lc,Nm,cons(_,_,_),Tp,Ex,Exx) :-
  transformCns(Map,Opts,Lc,Nm,Tp,Ex,Exx).

transformEnum(Map,Opts,Lc,Nm,Tp,Dfs,Dx) :-
  labelDefn(Map,Opts,Lc,Nm,Tp,Dfs,Dx).

transformCns(Map,Opts,Lc,Nm,Tp,Dfs,Dx) :-
  labelDefn(Map,Opts,Lc,Nm,Tp,Dfs,Dx).

labelDefn(Map,_Opts,Lc,Nm,Tp,
  [fnDef(Lc,lbl(Lbl,ArA),funType(tupleType([]),Tp),[ctpl(Con,[LblTerm])|Extra],Lblx)|Rx],Rx) :-
  lookupVarName(Map,Nm,Spec),
  trCons(Nm,1,Con),
  makeLabelTerm(Spec,Lbl,LblTerm),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  extraArity(1,Extra,ArA),
  mkUnit(Unit),
  labelAccess(Extra,_Q,Map,Lc,G),
  mergeWhere(Unit,G,Lc,Lblx).

makeLabelTerm(localClass(LclName,Strct,_LblPrg,ThVr),lbl(LclName,2),ctpl(Strct,[ThVr])).
makeLabelTerm(moduleCons(Lbl,Strct,0),Lbl,enum(Strct)).
makeLabelTerm(moduleCons(Lbl,Strct,Ar),Lbl,ctpl(lbl(Strct,Ar),[])).

transformThetaDefs(_,_,_,[],Ix,Ix,Dfs,Dfs).
transformThetaDefs(Map,OMap,Opts,[Def|Defs],I,Ix,Ex,Exx) :-
  transformThetaDef(Def,Map,OMap,Opts,I,I1,Ex,Ex1),
  transformThetaDefs(Map,OMap,Opts,Defs,I1,Ix,Ex1,Exx).

transformThetaDef(funDef(Lc,Nm,ExtNm,Tp,_,Eqns),Map,OMap,Opts,Ix,Ix,Dx,Dxx) :-
  transformFunction(Lc,Nm,ExtNm,Tp,Eqns,Map,OMap,Opts,Dx,Dxx).
transformThetaDef(varDef(Lc,Nm,ExtNm,_,Tp,Value),Map,OMap,Opts,I,Ix,Dx,Dxx) :-
  transformThetaDefn(Lc,Nm,ExtNm,Tp,Value,Map,OMap,Opts,I,Ix,Dx,Dxx).
transformThetaDef(cnsDef(Lc,Nm,Con,Tp),Map,_,Opts,Ix,Ix,Dx,Dxx) :-
  transformCnsDef(Map,Opts,Lc,Nm,Con,Tp,Dx,Dxx).
transformThetaDef(typeDef(_,_,_,_),_,_,_,Ix,Ix,Dx,Dx).
transformThetaDef(conDef(_,_,_),_,_,_,Ix,Ix,Dx,Dx).

closureEntry(Map,Lc,Name,Tp,[fnDef(Lc,lbl(Closure,ArX),TTp,
  [ClLbl|Args],cll(Lc,lbl(Prog,ArXX),XArgs))|L],L) :-
  lookupVarName(Map,Name,Reslt),
  programAccess(Reslt,Prog,Closure,Arity),
  extraVars(Map,Extra),
  genVars(Arity,Args),
  length(Extra,ExA),
  ArX is Arity+1,
  (Extra=[] -> ClLbl=enum(Closure) ; ClLbl=ctpl(lbl(Closure,ExA),Extra)),
  concat(Extra,Args,XArgs),
  length(XArgs,ArXX),
  extendFunTp(Tp,[_],TTp).

liftPtns([],[],Q,Q,_,_,Ex,Ex) :-!.
liftPtns([P|More],[A|Args],Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftPtns(More,Args,Q0,Qx,Map,Opts,Ex0,Exx).

liftPtn(v(_,"this",_),ThVr,Q,Qx,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
liftPtn(v(Lc,Nm,_),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts).
liftPtn(enm(Lc,Nm,_),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts).
liftPtn(intLit(Ix,_),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(floatLit(Ix,_),float(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(stringLit(Sx,_),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(tple(_,Ptns),PTpl,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(Ptns,Ps,Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(Ps,PTpl).
liftPtn(apply(Lc,v(_,Nm),tple(_,A)),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(apply(Lc,cons(_,Nm,_),tple(_,A)),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(apply(Lc,Op,tple(_,A),Tp),whr(Lc,idnt(N),mtch(Lc,XArg,XTrm)),Q,Qx,Map,Opts,Ex,Exx) :-
  genstr("_X",N),
  typeOfCanon(A,ATp),
  liftExp(apply(Lc,Op,tple(Lc,[v(Lc,N,ATp)]),Tp),XTrm,Q,Q0,Map,Opts,Ex,Ex0),
  liftPtns(A,Args,Q0,Q1,Map,Opts,Ex0,Exx),
  merge([idnt(N)],Q1,Qx),
  mkTpl(Args,TA),
  XArg=ctpl(lbl("star.core#some",1),[TA]).
liftPtn(where(_,P,enm(_,"true",type("star.core*boolean"))),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,Ptn,Q,Qx,Map,Opts,Ex,Exx).
liftPtn(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(XX,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(XX,Exp,Q,Qx,Map,Opts,Ex,Exx).

trVarPtn(_,"_",idnt("_"),Q,Q,_,_).
trVarPtn(Lc,Nm,A,Q,Qx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  implementVarPtn(V,Nm,Lc,A,Q,Qx).

implementVarPtn(localVar(Vn,_,TVr),_,Lc,cll(Lc,lbl(Vn,1),[TVr]),Q,Qx) :- !, % instance var
  merge([TVr],Q,Qx).
implementVarPtn(moduleVar(Vn),_,Lc,cll(Lc,lbl(Vn,0),[]),Q,Q) :-
  reportError("not allowed to have globals in patterns: %w",[Vn],Lc). % module variable
implementVarPtn(labelArg(N,TVr),_,_,N,Q,Qx) :- !,    % argument from label
  merge([N,TVr],Q,Qx).
implementVarPtn(moduleCons(Enum,_,0),_,_,enum(Enum),Q,Q).
implementVarPtn(localClass(Enum,_,_,ThVr),_,_,ctpl(Enum,[ThVr]),Q,Qx) :-
  merge([ThVr],Q,Qx).
implementVarPtn(notInMap,Nm,_,idnt(Nm),Q,Qx) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(Lc,Nm,Args,whr(Lc,X,mtch(Lc,X,ecll(Lc,Nm,Args))),Q,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!,
  genVar("_X",X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Args,Ptn,Q,Qx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  implementPtnCall(Reslt,Lc,Nm,Args,Ptn,Q,Qx).

implementPtnCall(localFun(Fn,_,_,Ar,ThVr),Lc,_,Args,whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,A2),XArgs))),Q,Qx) :-
  genVar("_X",X),
  concat(Args,[ThVr],XArgs),
  merge([X,ThVr],Q,Qx),
  A2 is Ar+1.
implementPtnCall(moduleFun(Fn,_,Ar),Lc,_,Args,whr(Lc,X,mtch(Lc,X,cll(Lc,lbl(Fn,Ar),Args))),Q,Qx) :-
  genVar("_X",X),
  merge([X],Q,Qx).
implementPtnCall(modulePtn(Fn,_,_),Lc,_,Args,whr(Lc,X,mtch(Lc,XArg,cll(Lc,lbl(Fn,1),[X]))),Q,Qx) :-
  genVar("_X",X),
  merge([X],Q,Qx),
  mkTpl(Args,TA),
  XArg=ctpl(lbl("star.core#some",1),[TA]).
implementPtnCall(localPtn(_,Fn,_,_,_,ThVr),Lc,_,Args,whr(Lc,X,mtch(Lc,XArg,cll(Lc,lbl(Fn,2),[X,ThVr]))),Q,Qx) :-
  genVar("_X",X),
  merge([X,ThVr],Q,Qx),
  mkTpl(Args,TA),
  XArg=ctpl(lbl("star.core#some",1),[TA]).
implementPtnCall(moduleCons(Mdl,_,Ar),_,_,Args,ctpl(lbl(Mdl,Ar),Args),Q,Q).
implementPtnCall(localClass(Mdl,_,_,ThVr),_,_,Args,ctpl(Mdl,XArgs),Q,Qx) :-
  concat(Args,[ThVr],XArgs),
  merge([ThVr],Q,Qx).

liftExps([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftExps([P|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftExps(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

liftExp(v(_,"this",_),ThVr,Q,Qx,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
liftExp(v(Lc,Nm,_),Vr,Q,Qx,Map,Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Map,Opts).
liftExp(enm(Lc,Nm,_),Vr,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Vr,Q,Qx,Map,Opts).
liftExp(cons(Lc,Nm,_),Vr,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Vr,Q,Qx,Map,Opts).
liftExp(intLit(Ix,_),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(floatLit(Ix,_),float(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(stringLit(Ix,_),strg(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftExp(tple(_,A),TApl,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,TA,[],Q,Qx,Map,Opts,Ex,Exx),
  mkTpl(TA,TApl).
liftExp(apply(Lc,Op,tple(_,A),_),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExps(A,LA,[],Q,Q1,Map,Opts,Ex,Ex1),
  trExpCallOp(Lc,Op,LA,Exp,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(dot(Lc,Rec,Fld,_),ocall(Lc,Rc,[Lbl]),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(Rec,Rc,Q,Qx,Map,Opts,Ex,Exx),
  makeDotLbl(Fld,Lbl).
liftExp(where(_,E,enm(_,"true",_)),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftExp(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
liftExp(conj(Lc,L,R),cnj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(L,LL,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(disj(Lc,L,R),dsj(Lc,LL ,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(L,LL,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(neg(Lc,R),ng(Lc,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(R,LR,Q,Qx,Map,Opts,Ex,Exx).
liftExp(cond(Lc,T,L,R,_),cnd(Lc,LT,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(T,LT,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(L,LL,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(match(Lc,L,R),mtch(Lc,Lx,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftPtn(L,Lx,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(R,Rx,Q0,Qx,Map,Opts,Ex0,Exx).
liftExp(assertion(Lc,G),Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  liftGoal(assertion(Lc,G),Gx,Q,Qx,Map,Opts,Ex,Exx).
liftExp(theta(Lc,Path,Defs,Others,Types,Sig),Theta,Q,Q,Map,Opts,Ex,Exx) :-!,
  genVar("_ThV",ThVr),
  liftTheta(theta(Lc,Path,Defs,Others,Types,Sig),ThVr,Theta,Q,Map,_,Opts,Ex,Exx).
liftExp(record(Lc,Path,Defs,Others,Types,Sig),Theta,Q,Q,Map,Opts,Ex,Exx) :-!,
  genVar("_ThV",ThVr),
  liftTheta(record(Lc,Path,Defs,Others,Types,Sig),ThVr,Theta,Q,Map,_,Opts,Ex,Exx).
liftExp(letExp(Lc,Th,Bnd),Exp,Q,Qx,Map,Opts,Ex,Exx) :-!,
  liftLetExp(Lc,Th,Bnd,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(lambda(Lc,Rle,Tp),Rslt,Q,Q,Map,Opts,Ex,Exx) :-!,
  liftLambda(Lc,Rle,Tp,Rslt,Q,Map,Opts,Ex,Exx).
liftExp(abstraction(Lc,Bnd,Cond,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftAbstraction(abstraction(Lc,Bnd,Cond,Gen,Tp),Rslt,Q,Qx,Map,Opts,Ex,Exx).
liftExp(XX,void,Q,Q,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trVarExp(Lc,Nm,Exp,Q,Qx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  implementVarExp(V,Lc,Nm,Exp,Q,Qx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

implementVarExp(localVar(Vn,_,ThVr),Lc,_,cll(Lc,lbl(Vn,1),[ThVr]),Q,Qx) :-
  merge([ThVr],Q,Qx).
implementVarExp(moduleVar(V),_Lc,_,idnt(V),Qx,Qx).
implementVarExp(labelArg(N,ThVar),_,_,N,Q,Qx) :-
  merge([N,ThVar],Q,Qx).
implementVarExp(moduleCons(Enum,_,0),_,_,enum(Enum),Q,Q).
implementVarExp(moduleCons(C,_,Ar),_,_,Cns,Q,Q) :-
  trCons(C,Ar,Cns).
implementVarExp(localClass(Enum,_,_,ThVr),_,_,ctpl(Enum,[ThVr]),Q,Qx) :-
  merge([ThVr],Q,Qx).
implementVarExp(notInMap,_,Nm,idnt(Nm),Q,Qx) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(moduleFun(_,Closure,_),_,_,enum(Closure),Q,Q).
implementVarExp(localFun(_Fn,_,Closure,_,ThVr),_,_,ctpl(lbl(Closure,1),[ThVr]),Q,Q).
implementVarExp(_Other,Lc,Nm,idnt(Nm),Q,Q) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

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

implementFunCall(Lc,localFun(Fn,_,_,Ar,ThVr),_,Args,cll(Lc,lbl(Fn,Ar2),XArgs),Q,Qx,_,_,Ex,Ex) :-
  concat([ThVr],Args,XArgs),
  merge([ThVr],Q,Qx),
  Ar2 is Ar+1.
implementFunCall(Lc,moduleFun(Fn,_,Ar),_,Args,cll(Lc,lbl(Fn,Ar),Args),Qx,Qx,_,_,Ex,Ex).
implementFunCall(_,moduleCons(Mdl,_,Ar),_,Args,ctpl(lbl(Mdl,Ar),Args),Q,Q,_,_,Ex,Ex).
implementFunCall(_,localClass(Mdl,_,_,Ar,ThVr),_,Args,ctpl(lbl(Mdl,Ar2),XArgs),Q,Qx,_,_,Ex,Ex) :-
  concat([ThVr],Args,XArgs),
  merge([ThVr],Q,Qx),
  Ar2 is Ar+1.
implementFunCall(Lc,notInMap,Nm,Args,ocall(Lc,idnt(Nm),Args),Q,Q,_Map,_Opts,Ex,Ex) :-
  reportError("cannot compile unknown function %s",[Nm],Lc).

liftLambda(Lc,Rule,Tp,Closure,Q,Map,Opts,[LamFun|Ex],Exx) :-
  lambdaMap(lambda(Lc,Rule,Tp),Q,Map,Opts,LclName,Closure,LMap,Ex,Ex0),
  transformRule(Rule,LMap,Opts,LclName,Rls,[],Ex0,Exx),
  is_member((_,Args,_,_),Rls),!,
  length(Args,Ar),
  functionMatcher(Lc,Ar,lbl(LclName,Ar),Tp,Rls,LamFun).

transformRule(equation(Lc,A,Cond,Value),Map,Opts,LclPrg,R,Rx,E,Ex) :-
  transformEqn(equation(Lc,A,Cond,Value),Map,Map,Opts,LclPrg,R,Rx,E,Ex).

lambdaLbl(Map,Variant,Nm) :-
  layerName(Map,Prefix),
  genstr(Variant,V),
  localName(Prefix,"@",V,Nm).

lambdaMap(Lam,Q,Map,_Opts,LclName,LblTerm,[lyr(LclName,Lx,LblTerm,ThVr)|Map],Ex,Ex) :-
  definedProgs(Map,Df),
  labelVars(Map,Lv),
  merge(Lv,Q,Q1),
  freeVars(Lam,Df,Q1,Lv,ThFr),
  lambdaLbl(Map,"λ",LclName),
  genVar("_ThV",ThVr),
  collectLabelVars(ThFr,ThVr,[],Lx),
  makeLblTerm(LclName,ThFr,LblTerm).

liftAbstraction(Ab,Rslt,Q,Qx,Map,Opts,Ex,Exx) :-
  layerName(Map,Path),
  genAbstraction(Ab,Path,AbExp),
  (is_member(showSetCode,Opts) -> dispCanonTerm(AbExp);true),
  liftExp(AbExp,Rslt,Q,Qx,Map,Opts,Ex,Exx).

mkClosure(Lam,FreeVars,Closure) :-
  length(FreeVars,Ar),
  (Ar = 0 ->
    Closure=enum(Lam) |
    Closure=ctpl(lbl(Lam,Ar),FreeVars)).

liftGoal(conj(Lc,L,R),cnj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(R,LR,Q0,Qx,Map,Opts,Ex0,Exx).
liftGoal(disj(Lc,L,R),dsj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(R,LR,Q0,Qx,Map,Opts,Ex0,Exx).
liftGoal(cond(Lc,T,L,R,_),cnd(Lc,LT,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(T,LT,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(L,LL,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftGoal(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftGoal(match(Lc,L,R),mtch(Lc,Lx,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftPtn(L,Lx,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(R,Rx,Q0,Qx,Map,Opts,Ex0,Exx).
liftGoal(neg(Lc,R),ng(Lc,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(R,Rx,Q,Qx,Map,Opts,Ex,Exx).
liftGoal(assertion(Lc,G),ecll(Lc,"_assert",[Gx,Lx]),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(G,Gx,Q,Qx,Map,Opts,Ex,Exx),
  locTerm(Lc,Lx).
liftGoal(G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(G,Gx,Q,Qx,Map,Opts,Ex,Exx).

/* A theta or record is converted to a structure containing free variables */

liftTheta(Theta,ThVr,LblTerm,Q,Map,ThMap,Opts,[ThetaFun|Ex],Exx) :-
  Theta=theta(Lc,_Path,Defs,_Others,_Types,Sig),!,
  thetaMap(Theta,ThVr,Q,Map,Opts,ThLbl,LblTerm,ThMap,EnRls,Ex,Ex0),
  transformThetaDefs(ThMap,ThMap,Opts,Defs,enum("star.core#true"),_I,Ex0,Exx),
  functionMatcher(Lc,2,lbl(ThLbl,2),funType(tupleType([tupleType([]),tupleType([])]),Sig),EnRls,ThetaFun).
liftTheta(Theta,ThVr,LblTerm,Q,Map,ThMap,Opts,[ThetaFun|Ex],Exx) :-
  Theta=record(Lc,_Path,Defs,_Others,_Types,Sig),
  thetaMap(Theta,ThVr,Q,Map,Opts,ThLbl,LblTerm,ThMap,EnRls,Ex,Ex0),
  transformThetaDefs(ThMap,Map,Opts,Defs,enum("star.core#true"),_I,Ex0,Exx),
  functionMatcher(Lc,2,lbl(ThLbl,2),funType(tupleType([tupleType([]),tupleType([])]),Sig),EnRls,ThetaFun).

liftLetExp(_Lc,Theta,Bnd,Expr,Q,Qx,Map,Opts,Ex,Exx) :-
  liftTheta(Theta,LblTerm,LblTerm,Q,Map,ThMap,Opts,Ex,Ex1),
  liftExp(Bnd,Expr,Q,Qx,ThMap,Opts,Ex1,Exx).

thetaMap(Theta,ThVr,Q,Map,_Opts,LclName,LblTerm,[lyr(LclName,Lx,LblTerm,ThVr)|Map],EnRls,Ex,Ex) :-
  definedProgs(Map,Df),
  labelVars(Map,Lv),
  merge(Lv,Q,Q1),
  freeVars(Theta,Df,Q1,Lv,ThFr),
  thetaLbl(Theta,Map,LclName),
  collectLabelVars(ThFr,ThVr,[],L0),
  makeLblTerm(LclName,ThFr,LblTerm),
  makeMtdMap(Theta,LclName,ThVr,L0,Lx,EnRls,[]).

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

thetaLbl(theta(_,Path,_,_,_,_),_Map,Path).
thetaLbl(record(_,Path,_,_,_,_),_Map,Path).

makeLblTerm(Nm,[],enum(Nm)) :- !.
makeLblTerm(Nm,Extra,ctpl(lbl(Nm,Ar),Extra)) :- length(Extra,Ar).

makeMtdMap(theta(_,_,Defs,_,_,_),OuterNm,ThVr,L,Lx,Ex,Exx) :-
  collectMtds(Defs,OuterNm,ThVr,L,Lx,Ex,Exx).
makeMtdMap(record(_,_,Defs,_,_,_),OuterNm,ThVr,L,Lx,Ex,Exx) :-
  collectMtds(Defs,OuterNm,ThVr,L,Lx,Ex,Exx).

collectMtds([],_,_,List,List,Ex,Ex).
collectMtds([Entry|Defs],OuterNm,ThVr,List,Lx,Ex,Exx) :-
  collectMtd(Entry,OuterNm,ThVr,List,L0,Ex,Ex0),
  collectMtds(Defs,OuterNm,ThVr,L0,Lx,Ex0,Exx).

collectMtd(funDef(Lc,Nm,LclName,Tp,_,_),OuterNm,ThV,List,
      [(Nm,localFun(LclName,AccessName,ClosureName,Ar,ThV))|List],[/*EnRl,*/ClRl|Ex],Ex) :-
  localName(OuterNm,"%",Nm,AccessName),
  localName(OuterNm,"^",Nm,ClosureName),
  progTypeArity(Tp,Ar),
  OuterPrg = lbl(OuterNm,2),
  %entryRule(OuterPrg,Lc,Nm,LclName,Ar,ThV,EnRl),
  closureRule(OuterPrg,Lc,Nm,ClosureName,ThV,ClRl).
collectMtd(varDef(Lc,Nm,LclName,_,_,_),OuterNm,ThV,List,
      [(Nm,localVar(LclName,AccessName,ThV))|List],[/*EnRl,*/AcRl|Ex],Ex) :-
  localName(OuterNm,"%",Nm,AccessName),
  OuterPrg = lbl(OuterNm,2),
  % entryRule(OuterPrg,Lc,Nm,LclName,0,ThV,EnRl),
  accessRule(OuterPrg,Lc,Nm,LclName,ThV,AcRl).
collectMtd(typeDef(_,_,_,_),_,_,List,List,Ex,Ex).

collectLabelVars([],_,List,List).
collectLabelVars([idnt(Nm)|Args],ThVr,List,Lx) :-
  collectLabelVars(Args,ThVr,[(Nm,labelArg(idnt(Nm),ThVr))|List],Lx).
collectLabelVars([_|Args],ThVr,List,Lx) :-
  collectLabelVars(Args,ThVr,List,Lx).

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
programAccess(modulePtn(Prog,Closure,Arity),Prog,Closure,Arity).
programAccess(localFun(Prog,_,Closure,Arity,_),Prog,Closure,Arity).
programAccess(localPtn(Prog,_,Closure,Arity,_),Prog,Closure,Arity).
programAccess(localVar(Prog,Closure,_),Prog,Closure,1).

labelAccess(Q,Qx,[lyr(_,_,LblPtn,idnt(LbVr))|_],Lc,mtch(Lc,LblPtn,idnt(LbVr))) :- !, merge([idnt(LbVr)],Q,Qx).
labelAccess(Q,Q,[lyr(_,_,_,_)|_],_,enum("star.core#true")) :- !.

makeDotLbl(Nm,enum(Dot)) :-
  localName("",".",Nm,Dot).

mkUnit(U) :-
  mkTpl([],U).
