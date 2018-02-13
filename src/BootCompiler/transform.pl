:- module(transform,[transformProg/3]).

/*
 * Implement a lambda lifting transformation to reduce (slightly) the semantics to
 * a flat language
 */

:- use_module(canon).
:- use_module(transutils).
:- use_module(errors).
:- use_module(types).
:- use_module(debug).
:- use_module(matcher).
:- use_module(misc).
:- use_module(escapes).
:- use_module(location).
:- use_module(freevars).

/*
  Functions are converted to top-level functions with explicit parameters containing
  free variables. Theta records are converted to structures.

  E.g. add(X) => { a(Y)=>X+Y }.

  is converted to (assuming add is already top-level):

  add(X) => thetaXX(X)

  We add accessor functions to thetaXX:

  thetaXX(a(X),Lbl) => aXX(Lbl,X)
  thetaXX('.a',Lbl) => thetaXX_A(Lbl)

  and individual functions are augmented with their theta parameter:

  aXX(Lbl,X) where thetaXX(Y) .= Lbl => X+Y

  calls to records and accesses to records are handled through a special builtin:

  A.a(3) —> ocall(a(3),A)

  A.a —-> ocall('.a',A)

  Calls through variables access the closure form of functions:

  F(3) —> ocall((3),F)

  which requires an additional agumentation for thetaXX:

  thetaXX_A((X),Lbl) => aXX(Lbl,X)
*/

transformProg(prog(pkg(Pkg,Vers),Lc,Imports,Defs,Others,Fields,Types,Contracts,Impls),
    Opts,module(pkg(Pkg,Vers),Imports,Fields,Types,Classes,Dfs,Contracts,Impls)) :-
  makePkgMap(Pkg,Defs,Types,Imports,Classes,Map),
  transformModuleDefs(Defs,Pkg,Map,Opts,Dfs,D0),
  transformOthers(Pkg,Map,Opts,Others,Inits,D0,D1),
  packageInit(Pkg,Lc,Map,Opts,Inits,D1,[]).

makePkgMap(Pkg,Defs,Types,Imports,Enums,[lyr(Pkg,DfList,void,void)]) :-
  makeModuleMap(Pkg,Defs,DfList,Rest,Enums),
  makeImportsMap(Imports,Rest,R0),
  makeTypesMap(Pkg,Types,R0,[]).

makeModuleMap(Pkg,[Def|Rest],Map,Mx,Enums) :-
  makeMdlEntry(Pkg,Def,Map,M0,Enums,Clx),
  makeModuleMap(Pkg,Rest,M0,Mx,Clx).
makeModuleMap(_,[],Map,Map,[]).

makeMdlEntry(Pkg,funDef(_,Nm,Tp,_,_),[(Nm,moduleFun(Pkg,LclName,AccessName,ClosureName,Ar))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(Tp,Ar).
makeMdlEntry(Pkg,varDef(_,Nm,_,_,_),[(Nm,moduleVar(Pkg,LclName,AccessName))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName).
makeMdlEntry(Pkg,cnsDef(_,Nm,cns(_,_),Tp),[(Nm,moduleCons(LclName,AccessName,Ar))|Mx],Mx,[LclName|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName),
  typeArity(Tp,Ar).
makeMdlEntry(Pkg,cnsDef(_,Nm,enm(_,_),_),[(Nm,moduleCons(LclName,AccessName,0))|Mx],Mx,[LclName|Clx],Clx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName).
makeMdlEntry(Pkg,typeDef(_,Nm,Tp,_),[(Nm,moduleType(Pkg,LclName,Tp))|Mx],Mx,Clx,Clx) :-
  localName(Pkg,"*",Nm,LclName).
makeMdlEntry(_,impl(_,_,ImplNm,0,_,_,_,_,_),[(ImplNm,moduleImpl(ImplNm,enu(ImplNm)))|Mx],Mx,Clx,Clx).
makeMdlEntry(_,impl(_,_,ImplNm,Arity,_,_,_,_,_),[(ImplNm,moduleImpl(ImplNm,strct(ImplNm,Arity)))|Mx],Mx,Clx,Clx).
makeMdlEntry(_,_,Mx,Mx,Clx,Clx).

makeImportsMap([Import|Rest],Map,Mx) :-
  makeImportMap(Import,Map,M0),
  makeImportsMap(Rest,M0,Mx).
makeImportsMap([],Map,Map).

makeImportMap(import(_,pkg(Pkg,_),faceType(Fields,Types),_,Enums,_,Impls),Map,Mx) :-
  importFields(Pkg,Enums,Fields,Map,M0),
  importImplementations(Impls,M0,M1),
  importTypes(Types,M1,Mx).

importFields(_,_,[],Map,Map).
importFields(Pkg,Enums,[(Nm,Tp)|Fields],Map,Mx) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,Template),
  makeImportEntry(Template,Enums,Pkg,Nm,Map,M0),
  importFields(Pkg,Enums,Fields,M0,Mx).

makeImportEntry(funType(A,_),_,Pkg,Nm,[(Nm,moduleFun(Pkg,LclName,AccessName,ClosureName,Ar))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName),
  localName(Pkg,"^",Nm,ClosureName),
  typeArity(A,Ar).
makeImportEntry(_,Enums,Pkg,Nm,[(Nm,moduleCons(LclName,AccessName,0))|Mx],Mx) :-
  is_member(Nm,Enums),!,
  localName(Pkg,"@",Nm,AccessName),
  localName(Pkg,"#",Nm,LclName).
makeImportEntry(consType(A,_),_,Pkg,Nm,[(Nm,moduleCons(LclName,AccessName,Ar))|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName),
  localName(Pkg,"@",Nm,AccessName),
  length(A,Ar).
makeImportEntry(_,_,Pkg,Nm,[(Nm,moduleVar(Pkg,LclName,AccessName))|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName),
  localName(Pkg,"%",Nm,AccessName).

importImplementations([],Map,Map).
importImplementations([imp(Nm,Con)|L],[(Nm,moduleImpl(Nm,Struct))|M],Mx) :-
  contractArity(Con,Ar),
  contractStruct(Ar,Nm,Struct),
  importImplementations(L,M,Mx).

contractArity(allType(_,Con),Ar) :- contractArity(Con,Ar).
contractArity(constrained(Con,_),Ar) :- contractArity(Con,A), Ar is A+1.
contractArity(_,0).

contractStruct(0,Nm,enu(Nm)).
contractStruct(Ar,Nm,strct(Nm,Ar)).

makeTypesMap(_,_,List,List).

transformModuleDefs([],_,_,_,Ex,Ex).
transformModuleDefs([Def|Defs],Pkg,Map,Opts,Ex,Exx) :-
  transformMdlDef(Def,Pkg,Map,Opts,Ex,Ex1),
  transformModuleDefs(Defs,Pkg,Map,Opts,Ex1,Exx).

transformMdlDef(funDef(Lc,Nm,Tp,[],Eqns),_,Map,Opts,Dx,Dxx) :-
  transformFunction(Lc,Nm,Tp,Eqns,Map,Opts,Dx,Dxx).
transformMdlDef(varDef(Lc,Nm,[],Tp,Value),_,Map,Opts,Dx,Dxx) :-
  transformDefn(Map,Opts,Lc,Nm,Tp,Value,Dx,Dxx).
transformMdlDef(cnsDef(_,_,_,_),_,_,_,Dx,Dx).
transformMdlDef(typeDef(_,_,_,_),_,_,_,Dx,Dx).
transformMdlDef(conDef(_,_,_),_,_,_,Dx,Dx).
transformMdlDef(implDef(_,_,_,_),_,_,_,Dx,Dx).

extraArity(Arity,Vars,ExAr) :-
  length(Vars,E),
  ExAr is E+Arity.

transformFunction(Lc,Nm,Tp,Eqns,Map,Opts,[Fun|Ex],Exx) :-
  lookupFunName(Map,Nm,Reslt),
  programAccess(Reslt,LclFun,_,_,Arity),
  pushOpt(Opts,inProg(Nm),FOpts),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = prg(LclFun,Ar),
  transformEquations(Map,FOpts,LclPrg,Eqns,Rules,[],Ex,Ex0),
  closureEntry(Map,Lc,Nm,Ex0,Exx),
  functionMatcher(Lc,Ar,LclPrg,Tp,Rules,Fun).

transformEquations(_,_,_,[],Rules,Rules,Ex,Ex).
transformEquations(Map,Opts,LclPrg,[Eqn|Defs],Rules,Rx,Ex,Exx) :-
  transformEqn(Eqn,Map,Opts,LclPrg,Rules,R0,Ex,Ex0),
  transformEquations(Map,Opts,LclPrg,Defs,R0,Rx,Ex0,Exx).

transformEqn(equation(Lc,_,tple(_,_Tp,A),Cond,Value),Map,Opts,_LclPrg,
    [eqn(Lc,Args,Rhs)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),
  liftPtns(A,Args,Extra,Extra,Q1,Map,Opts,Ex,Ex0), % head args
  liftGoal(Cond,Test,Q1,Q2,Map,Opts,Ex0,Ex1),   % condition goals
  liftExp(Value,Rep,Q2,Q3,Map,Opts,Ex1,Exx),  % replacement expression
  labelAccess(Q3,_Q,Map,Lc,LbLx),
  mergeGoal(Test,LbLx,Lc,EqTest),
  mergeWhere(Rep,EqTest,Lc,Rhs).         % generate label access goals

genRaise(Lc,LclName,[raise(ctpl(strct("error",4),[LclName,intgr(Lno),intgr(Off),intgr(Sz)]))|P],P) :-
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(Lc,Sz).

transformDefn(Map,Opts,Lc,Nm,Tp,Value,[fnDef(Lc,VrProg,Tp,[eqn(Lc,Extra,Body)])|Dx],Dxx) :-
  lookupVarName(Map,Nm,Reslt),
  programAccess(Reslt,LclName,_,_,_),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  liftExp(Value,Rep,[],Q0,Map,Opts,Dx,Dxx),
  labelAccess(Q0,_Q,Map,Lc,G0),                        % generate label access goals
  length(Extra,Arity),
  VrProg = prg(LclName,Arity),
  mergeWhere(Rep,G0,Lc,Body).

transformOthers(_,_,_,[],enu("star.core#true"),Rx,Rx).
transformOthers(Pkg,Map,Opts,[assertion(Lc,G)|Others],Inits,Rules,Rx) :-
  collect(Others,canon:isAssertion,Asserts,Rest),
  transformAssertions(Pkg,Map,Opts,Lc,[assertion(Lc,G)|Asserts],AssertName,Rules,R0),
  transformOthers(Pkg,Map,Opts,Rest,More,R0,Rx),
  mergeGoal(cll(Lc,AssertName,[]),More,Lc,Inits).

transformAssertions(Pkg,Map,Opts,Lc,Asserts,LclPrg,[fnDef(Lc,LclPrg,funType(tupleType([]),BoolTp),[eqn(Lc,[],Goal)])|Ex],Exx) :-
  rfold(Asserts,transform:collectGoal,enm(Lc,"true"),G),
  localName(Pkg,"@","assert",LclName),
  LclPrg = prg(LclName,0),
  liftExp(G,Goal,[],_Q,Map,Opts,Ex,Exx),
  stdType("boolean",BoolTp).

collectGoal(assertion(_,G),enm(_,"true"),G) :-!.
collectGoal(assertion(Lc,G),O,conj(Lc,O,G)).

packageInit(Pkg,Lc,_,_,Inits,[fnDef(Lc,InitPrg,funType(tupleType([]),tupleType([])),[eqn(Lc,[],Inits)])|R],R) :-
  localName(Pkg,"@","init",InitNm),
  InitPrg = prg(InitNm,0).

transformCnsDef(Map,Opts,Lc,Nm,enm(_,_),Tp,Ex,Exx) :-
  transformEnum(Map,Opts,Lc,Nm,Tp,Ex,Exx).
transformCnsDef(Map,Opts,Lc,Nm,cns(_,_),Tp,Ex,Exx) :-
  transformCns(Map,Opts,Lc,Nm,Tp,Ex,Exx).

transformEnum(Map,Opts,Lc,Nm,Tp,Dfs,Dx) :-
  labelDefn(Map,Opts,Lc,Nm,_LclName,Tp,Dfs,Dx).

transformCns(Map,Opts,Lc,Nm,Tp,Dfs,Dx) :-
  labelDefn(Map,Opts,Lc,Nm,_LclName,Tp,Dfs,Dx).

labelDefn(Map,_Opts,Lc,Nm,LclName,[fnDef(Lc,prg(Access,ArA),funType(tupleType([]),Tp),
    Tp,[eqn(Lc,[tpl(Con,[LblTerm])|Extra],Lblx)])|Rx],Rx) :-
  lookupVarName(Map,Nm,Spec),
  trCons(Nm,1,Con),
  makeLabelTerm(Spec,Access,LblTerm,LclName),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  extraArity(1,Extra,ArA),
  mkUnit(Unit),
  labelAccess(Extra,_Q,Map,Lc,G),
  mergeWhere(Unit,G,Lc,Lblx).

makeLabelTerm(localClass(LclName,Strct,LblPrg,ThVr),prg(LclName,2),ctpl(Strct,[ThVr]),LblPrg).
makeLabelTerm(moduleCons(Access,Strct,0),Access,enu(Strct),Access).
makeLabelTerm(moduleCons(Access,Strct,Ar),Access,ctpl(strct(Strct,Ar),[]),Access).
makeLabelTerm(moduleImpl(Access,Strct),Access,Strct,Strct).

transformThetaDefs(_,_,[],Dfs,Dfs).
transformThetaDefs(Map,Opts,[Def|Defs],Ex,Exx) :-
  transformThetaDef(Map,Opts,Def,Ex,Ex1),
  transformThetaDefs(Map,Opts,Defs,Ex1,Exx).

transformThetaDef(Map,Opts,funDef(Lc,Nm,Tp,_,Eqns),Dx,Dxx) :-
  transformFunction(Lc,Nm,Tp,Eqns,Map,Opts,Dx,Dxx).
transformThetaDef(Map,Opts,varDef(Lc,Nm,_,Tp,Value),Dx,Dxx) :-
  transformDefn(Map,Opts,Lc,Nm,Tp,Value,Dx,Dxx).
transformThetaDef(Map,Opts,cnsDef(Lc,Nm,Con,Tp),Dx,Dxx) :-
  transformCnsDef(Map,Opts,Lc,Nm,Con,Tp,Dx,Dxx).
transformThetaDef(_,_,typeDef(_,_,_,_),Dx,Dx).
transformThetaDef(_,_,contract(_,_,_),Dx,Dx).
transformThetaDef(_,_,typeDef(_,_,_,_),Dx,Dx).

closureEntry(Map,Lc,Name,[fnDef(Lc,prg(Closure,2),funType(tupleType([]),voidType),[eqn(Lc,[CallStrct,ClosureCons],
    cll(Lc,prg(Prog,ArX),Q))])|L],L) :-
  lookupVarName(Map,Name,Reslt),
  programAccess(Reslt,Prog,_,Closure,Arity),
  extraVars(Map,Extra),
  genVars(Arity,Args),
  concat(Args,Extra,Q),
  trCons("_call",Arity,Con),
  CallStrct = ctpl(Con,Args),
  length(Extra,ExAr),
  (Extra=[] -> ClosureCons = enu(Closure) ; ClosureCons = ctpl(strct(Closure,ExAr),Extra)),
  length(Q,ArX).

transformImplementation(Lc,ImplName,Def,Face,Map,Opts,Rules,Rx,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,ImplName,_,Face,Rules,R0),
  genClassMap(Map,Opts,Lc,ImplName,[Def],Face,CMap,R0,En0,Ex,Ex1),!,
  transformClassBody([Def],CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).

liftPtns([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftPtns([P|More],[A|Args],Ax,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtn(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftPtns(More,Args,Ax,Q0,Qx,Map,Opts,Ex0,Exx).

liftPtn(v(_,"this"),ThVr,Q,Qx,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
liftPtn(v(Lc,Nm),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts).
liftPtn(enm(Lc,Nm),A,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Map,Opts).
liftPtn(intLit(Ix),intgr(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(floatLit(Ix),float(Ix),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(stringLit(Sx),strg(Sx),Q,Q,_,_,Ex,Ex) :-!.
liftPtn(dot(Lc,Rc,Fld),Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("XV",X),
  trCons(Fld,1,S),
  trDotExp(Lc,Rc,ctpl(S,[X]),X,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftPtn(tple(_,Tp,Ptns),tpl(Tp,P),Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(Ptns,P,[],Q,Qx,Map,Opts,Ex,Exx).
liftPtn(apply(Lc,v(_,Nm),tple(_,Tps,A)),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,[],Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(apply(Lc,cns(_,Nm),tple(_,A)),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
  liftPtns(A,Args,[],Q,Q0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Lc,Nm,Args,Ptn,Q0,Qx,Map,Opts,Ex0,Exx).
liftPtn(where(_,P,enm(_,"true")),Ptn,Q,Qx,Map,Opts,Ex,Exx) :-
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

implementVarPtn(localVar(Vn,_,TVr),_,Lc,cll(Lc,prg(Vn,1),[TVr]),Q,Qx) :- !, % instance var
  merge([TVr],Q,Qx).
implementVarPtn(moduleVar(_,Vn,_),_,Lc,cll(Lc,prg(Vn,0),[]),Q,Q) :-
      !. % module variable
implementVarPtn(labelArg(N,TVr),_,_,N,Q,Qx) :- !,    % argument from label
  merge([N,TVr],Q,Qx).
implementVarPtn(moduleCons(Enum,_,0),_,_,enu(Enum),Q,Q).
implementVarPtn(localClass(Enum,_,_,ThVr),_,_,ctpl(Enum,[ThVr]),Q,Qx) :-
  merge([ThVr],Q,Qx).
implementVarPtn(notInMap,Nm,_,idnt(Nm),Q,Qx) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(Lc,Nm,Args,whr(Lc,X,mtch(Lc,X,ecll(Nm,Args))),Q,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!,
  genVar("X",X),
  merge([X],Q,Qx).
trPtnCallOp(Lc,Nm,Args,Ptn,Q,Qx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  implementPtnCall(Reslt,Lc,Nm,Args,Ptn,Q,Qx).

implementPtnCall(localFun(Fn,_,_,Ar,ThVr),Lc,_,Args,whr(Lc,X,mtch(Lc,X,cll(Lc,prg(Fn,A2),XArgs))),Q,Qx) :-
  genVar("X",X),
  concat(Args,[ThVr],XArgs),
  merge([X,ThVr],Q,Qx),
  A2 is Ar+1.
implementPtnCall(moduleFun(_,Fn,_,_,Ar),Lc,_,Args,whr(Lc,X,mtch(Lc,X,cll(Lc,prg(Fn,Ar),Args))),Q,Qx) :-
  genVar("X",X),
  merge([X],Q,Qx).
implementPtnCall(moduleCons(Mdl,_,Ar),_,_,Args,ctpl(strct(Mdl,Ar),Args),Q,Q).
implementPtnCall(localClass(Mdl,_,_,ThVr),_,_,Args,ctpl(Mdl,XArgs),Q,Qx) :-
  concat(Args,[ThVr],XArgs),
  merge([ThVr],Q,Qx).
implementPtnCall(moduleImpl(_,Mdl),_,_,Args,ctpl(Mdl,Args),Q,Q).

liftExps([],Args,Args,Q,Q,_,_,Ex,Ex) :-!.
liftExps([P|More],[A|Args],Extra,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,A,Q,Q0,Map,Opts,Ex,Ex0),
  liftExps(More,Args,Extra,Q0,Qx,Map,Opts,Ex0,Exx).

liftExp(v(_,"this"),ThVr,Q,Qx,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
liftExp(v(Lc,Nm),Vr,Q,Qx,Map,Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Map,Opts).
liftExp(enm(Lc,Nm),Vr,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Vr,Q,Qx,Map,Opts).
liftExp(cns(Lc,Nm),Vr,Q,Qx,Map,Opts,Ex,Ex) :- !,
  trVarExp(Lc,Nm,Vr,Q,Qx,Map,Opts).
liftExp(intLit(Ix,Tp),intgr(Ix,Tp),Q,Q,_,_,Ex,Ex) :-!.
liftExp(floatLit(Ix,Tp),float(Ix,Tp),Q,Q,_,_,Ex,Ex) :-!.
liftExp(stringLit(Ix,Tp),strg(Ix,Tp),Q,Q,_,_,Ex,Ex) :-!.
liftExp(tple(_,Tp,A),tpl(TA),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExps(A,TA,[],Q,Qx,Map,Opts,Ex,Exx).
liftExp(apply(Lc,Op,tple(_,Tp,A)),Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExps(A,LA,[],Q,Q1,Map,Opts,Ex,Ex1),
  trExpCallOp(Lc,Op,LA,Exp,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(dot(Lc,Rec,Fld),Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(Rec,LRec,Q,Q0,Map,Opts,Ex,Ex0),
  trDotExp(Lc,LRec,Fld,Exp,Q0,Qx,Map,Opts,Ex0,Exx).
liftExp(where(_,E,enm(_,"true")),Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(E,Exp,Q,Qx,Map,Opts,Ex,Exx).
liftExp(where(Lc,P,C),whr(Lc,LP,LC),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(P,LP,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(C,LC,Q0,Qx,Map,Opts,Ex0,Exx).
liftExp(conj(Lc,L,R),cnj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(L,LL,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(disj(Lc,L,R),dsj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(L,LL,Q,Q1,Map,Opts,Ex,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(neg(Lc,R),ng(Lc,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftExp(R,LR,Q,Qx,Map,Opts,Ex,Exx).
liftExp(cond(Lc,T,L,R),cnd(Lc,LT,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(T,LT,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(L,LL,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftExp(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftExp(match(Lc,L,R),mtch(Lc,Lx,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftPtn(L,Lx,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(R,Rx,Q0,Qx,Map,Opts,Ex0,Exx).
liftExp(theta(Lc,Path,Defs,Others,Types,Sig),Theta,Q,Q,Map,Opts,Ex,Exx) :-
  liftTheta(theta(Lc,Path,Defs,Others,Types,Sig),Theta,Q,Map,Opts,Ex,Exx).
liftExp(record(Lc,Path,Defs,Others,Types,Sig),Theta,Q,Q,Map,Opts,Ex,Exx) :-
  liftTheta(record(Lc,Path,Defs,Others,Types,Sig),Theta,Q,Map,Opts,Ex,Exx).
liftExp(lambda(Lc,Rl,Tp),Rslt,Q,Q,Map,Opts,Ex,Exx) :-
  trLambdaRule(Lc,Rl,Tp,Rslt,Q,Map,Opts,Ex,Exx).
liftExp(XX,void,Q,Q,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trDotExp(_,v(Lc,Nm),Fld,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  lookupVarName(Map,Nm,Reslt),
  implementDotExp(Reslt,Lc,idnt(Nm),Fld,Exp,Q,Qx,Map,Opts,Ex,Exx).
trDotExp(Lc,R,Fld,ocall(Lc,Lbl,R),Qx,Qx,_,_,Ex,Ex) :-
  makeDotLbl(Fld,Lbl).

implementDotExp(moduleCons(_,Lbl,_),Lc,Rc,_,cll(Lc,prg(Lbl,1),[Rc]),Qx,Qx,_,_,Ex,Ex).
implementDotExp(_,Lc,Rc,Fld,ocall(Lc,F,Rc),Qx,Qx,_,_,Ex,Ex) :-
  makeDotLbl(Fld,F).

trVarExp(_,"_",idnt("_"),Q,Q,_,_).
trVarExp(Lc,Nm,Exp,Q,Qx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  implementVarExp(V,Lc,Nm,Exp,Q,Qx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

implementVarExp(localVar(Vn,_,ThVr),Lc,_,cll(Lc,prg(Vn,1),[ThVr]),Q,Qx) :-
  merge([ThVr],Q,Qx).
implementVarExp(moduleVar(_,V,_),Lc,_,cll(Lc,prg(V,0),[]),Qx,Qx).
implementVarExp(labelArg(N,ThVar),_,_,N,Q,Qx) :-
  merge([N,ThVar],Q,Qx).
implementVarExp(moduleCons(Enum,_,0),_,_,enum(Enum),Q,Q).
implementVarExp(moduleCons(C,_,Ar),_,_,Cns,Q,Q) :-
  trCons(C,Ar,Cns).
implementVarExp(moduleImpl(_,enum(Enum)),_,_,enum(Enum),Q,Q).
implementVarExp(localClass(Enum,_,_,ThVr),_,_,ctpl(Enum,[ThVr]),Q,Qx) :-
  merge([ThVr],Q,Qx).
implementVarExp(notInMap,_,Nm,idnt(Nm),Q,Qx) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(moduleFun(_,_,_,Acc,_),_,_,enum(Acc),Q,Q).
implementVarExp(moduleRel(_,_,_,Acc,_),_,_,enum(Acc),Q,Q).
implementVarExp(localFun(_,_,Closure,_,ThVr),_,_,ctpl(strct(Closure,1),[ThVr]),Q,Q).
implementVarExp(_Other,Lc,Nm,idnt(Nm),Q,Q) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

trExpCallOp(Lc,v(_,Nm),Args,ecll(Lc,Nm,Args),Qx,Qx,_,_,Ex,Ex) :-
  isEscape(Nm),!.
trExpCallOp(Lc,v(_,Nm),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  implementFunCall(Lc,Reslt,Nm,Args,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,dot(_,Rec,Fld),Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  length(Args,Arity),
  trCons(Fld,Arity,Op),
  trExpCallDot(Lc,Rec,ctpl(Op,Args),Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallOp(Lc,Op,A,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  length(A,Arity),
  trCons("_call",Arity,ClosOp),
  trExpCallDot(Lc,Op,ctpl(ClosOp,A),Exp,Q,Qx,Map,Opts,Ex,Exx).

trExpCallDot(Lc,v(_,Nm),Rec,C,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  implementDotFunCall(Lc,Reslt,Rec,C,Exp,Q,Qx,Map,Opts,Ex,Exx).
trExpCallDot(Lc,R,C,ocall(Lc,C,Rc),Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(R,Rc,Q,Qx,Map,Opts,Ex,Exx).

implementDotFunCall(Lc,_,Rc,C,ocall(Lc,C,Rc),Qx,Qx,_,_,Ex,Ex).

implementFunCall(Lc,localFun(Fn,_,_,Ar,ThVr),_,Args,cll(Lc,prg(Fn,Ar2),XArgs),Q,Qx,_,_,Ex,Ex) :-
  concat(Args,[ThVr],XArgs),
  merge([ThVr],Q,Qx),
  Ar2 is Ar+1.
implementFunCall(Lc,moduleFun(_,Fn,_,_,Ar),_,Args,cll(Lc,prg(Fn,Ar),Args),Qx,Qx,_,_,Ex,Ex).
implementFunCall(_,moduleCons(Mdl,_,Ar),_,Args,ctpl(strct(Mdl,Ar),Args),Q,Q,_,_,Ex,Ex).
implementFunCall(_,localClass(Mdl,_,_,Ar,ThVr),_,Args,ctpl(strct(Mdl,Ar2),XArgs),Q,Qx,_,_,Ex,Ex) :-
  concat(Args,[ThVr],XArgs),
  merge([ThVr],Q,Qx),
  Ar2 is Ar+1.
implementFunCall(_,moduleImpl(_,Mdl),_,Args,ctpl(Mdl,Args),Q,Q,_,_,Ex,Ex).
implementFunCall(Lc,notInMap,Nm,Args,Exp,Q,Qx,Map,Opts,Ex,Exx) :-
  trExpCallOp(Lc,dot(v(Lc,Nm),"_call"),Args,Exp,Q,Qx,Map,Opts,Ex,Exx).

% We build $$(_call(Args<>Rep),$$(Free),_) :- Cond, !, replacement
trLambdaRule(Lc,Rule,Tp,Closure,Q,Map,Opts,[LamFun|Ex],Exx) :-
  lambdaMap(Rule,Q,Map,LclName,Closure,LMap,Ex,Ex0),
  transformEqn(Rule,LMap,Opts,LclName,Rl,[],Ex0,Exx),
  extraVars(LMap,Extra),
  mkClosure(LclName,Extra,Closure),
  ruleArity(Rl,Ar),
  functionMatcher(Lc,Ar,LclName,Tp,[Rl],LamFun).

lambdaLbl(Map,Variant,Nm) :-
  layerName(Map,Prefix),
  genstr(Variant,V),
  localName(Prefix,"@",V,Nm).

lambdaMap(Rule,Q,Map,Opts,LclName,LblTerm,[lyr(LclName,Lx,LblTerm,ThVr)|Map],Ex,Exx) :-
  genVar("ThV",ThVr),
  extraVars(Map,Extra),
  freeVars(Rule,Q,Extra,FV),
  lambdaLbl(Map,"_lambda",LclName),
  collectLabelVars(FV,ThVr,[],Lx),
  liftExps(ThVr,ThVars,Extra,FV,_,Map,Opts,Ex,Exx),
  makeLblTerm(LclName,ThVars,LblTerm).

mkClosure(Lam,FreeVars,Closure) :-
  length(FreeVars,Ar),
  (Ar = 0 ->
    Closure=enu(Lam) |
    Closure=ctpl(strct(Lam,Ar),FreeVars)).

liftGoal(conj(Lc,L,R),cnj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(R,LR,Q0,Qx,Map,Opts,Ex0,Exx).
liftGoal(disj(Lc,L,R),dsj(Lc,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(L,LL,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(R,LR,Q0,Qx,Map,Opts,Ex0,Exx).
liftGoal(cond(Lc,T,L,R),cnd(Lc,LT,LL,LR),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(T,LT,Q,Q0,Map,Opts,Ex,Ex0),
  liftGoal(L,LL,Q0,Q1,Map,Opts,Ex0,Ex1),
  liftGoal(R,LR,Q1,Qx,Map,Opts,Ex1,Exx).
liftGoal(match(Lc,L,R),mtch(Lc,Lx,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftPtn(L,Lx,Q,Q0,Map,Opts,Ex,Ex0),
  liftExp(R,Rx,Q0,Qx,Map,Opts,Ex0,Exx).
liftGoal(neg(Lc,R),ng(Lc,Rx),Q,Qx,Map,Opts,Ex,Exx) :- !,
  liftGoal(R,Rx,Q,Qx,Map,Opts,Ex,Exx).
liftGoal(G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  liftExp(G,Gx,Q,Qx,Map,Opts,Ex,Exx).

trLocation(loc(Ln,Col,_,Sz),tpl([intgr(Ln),intgr(Col),intgr(Sz)]),G,G,Q,Q,_,_,Ex,Ex).

/* A theta or record is converted to a structure containing free variables */

liftTheta(Theta,LblTerm,Q,Map,Opts,[ThetaFun|Ex],Exx) :-
  thetaMap(Theta,Q,Map,Opts,ThLbl,LblTerm,ThMap,EnRls,Ex,Ex0),
  thetaLoc(Theta,Lc),
  thetaDefs(Theta,Defs),
  thetaSig(Theta,Sig),
  transformThetaDefs(ThMap,Opts,Defs,Ex0,Exx),
  functionMatcher(Lc,2,prg(ThLbl,2),funType(tupleType([]),Sig),EnRls,ThetaFun).

thetaMap(Theta,Q,Map,Opts,LclName,LblTerm,[lyr(LclName,Lx,LblTerm,ThVr)|Map],EnRls,Ex,Exx) :-
  genVar("ThV",ThVr),
  extraVars(Map,Extra),
  freeVars(Theta,Q,Extra,ThFr),
  thetaLbl(Theta,Map,LclName),
  collectLabelVars(ThFr,ThVr,[],L0),
  liftExps(ThFr,ThVars,Extra,ThFr,_,Map,Opts,Ex,Exx),
  makeLblTerm(LclName,ThVars,LblTerm),
  makeMtdMap(Theta,LclName,ThVr,L0,Lx,EnRls,[]).

thetaLbl(theta(_,Path,_,_,_,_),Map,Lbl) :-
  layerName(Map,Outer),
  localName(Outer,"•",Path,Lbl).
thetaLbl(record(_,Path,_,_,_,_),Map,Lbl) :-
  layerName(Map,Outer),
  localName(Outer,"•",Path,Lbl).

makeLblTerm(Nm,[],enu(Nm)) :- !.
makeLblTerm(Nm,Extra,ctpl(strct(Nm,Ar),Extra)) :- length(Extra,Ar).

makeMtdMap(theta(_,_,Defs,_,_,_),OuterNm,ThVr,L,Lx,Ex,Exx) :-
  collectMtds(Defs,OuterNm,ThVr,L,Lx,Ex,Exx).
makeMtdMap(record(_,_,Defs,_,_,_),OuterNm,ThVr,L,Lx,Ex,Exx) :-
  collectMtds(Defs,OuterNm,ThVr,L,Lx,Ex,Exx).

collectMtds([],_,_,List,List,Ex,Ex).
collectMtds([Entry|Defs],OuterNm,ThVr,List,Lx,Ex,Exx) :-
  collectMtd(Entry,OuterNm,ThVr,List,L0,Ex,Ex0),
  collectMtds(Defs,OuterNm,ThVr,L0,Lx,Ex0,Exx).

collectMtd(funDef(Lc,Nm,Tp,_,_),OuterNm,ThV,List,
      [(Nm,localFun(LclName,AccessName,ClosureName,Ar,ThV))|List],[EnRl,ClRl|Ex],Ex) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName),
  localName(OuterNm,"^",Nm,ClosureName),
  typeArity(Tp,Ar),
  OuterPrg = prg(OuterNm,2),
  entryRule(OuterPrg,Lc,Nm,LclName,Ar,ThV,EnRl),
  closureRule(OuterPrg,Lc,Nm,ClosureName,ThV,ClRl).
collectMtd(varDef(Lc,Nm,_,_,_),OuterNm,ThV,List,
      [(Nm,localVar(LclName,AccessName,ThV))|List],[EnRl,AcRl|Ex],Ex) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName),
  OuterPrg = prg(OuterNm,2),
  entryRule(OuterPrg,Lc,Nm,LclName,0,ThV,EnRl),
  accessRule(OuterPrg,Lc,Nm,LclName,ThV,AcRl).
collectMtd(typeDef(_,_,_,_),_,_,List,List,Ex,Ex).

collectLabelVars([],_,List,List).
collectLabelVars([v(_,Nm)|Args],ThVr,List,Lx) :-
  collectLabelVars(Args,ThVr,[(Nm,labelArg(idnt(Nm),ThVr))|List],Lx).

/*
* Generate the equation:
  OuterNm(Nm(Args),ThVr) => LclName(Args,ThVr)
*/
entryRule(_,Lc,Nm,LclName,Arity,ThV,
    eqn(Lc,[ctpl(Acc,Args),ThV],cll(Lc,prg(LclName,Ar1),AllArgs))) :-
  genVars(Arity,Args),
  trCons(Nm,Arity,Acc),
  concat(Args,[ThV],AllArgs),
  length(AllArgs,Ar1).

/*
 Generate the closure return:
 OuterNm(.Nm,ThVr) => ClosureNm(ThVr)
*/
closureRule(_,Lc,Nm,ClosureName,ThVr,
    eqn(Lc,[DotName,ThVr],ctpl(strct(ClosureName,1),[ThVr]))) :-
  makeDotLbl(Nm,DotName).

accessRule(_,Lc,Nm,LclName,ThV,eqn(Lc,[DotName,ThV],cll(Lc,prg(LclName,1),[ThV]))) :-
  makeDotLbl(Nm,DotName).

programAccess(moduleFun(_,Prog,Access,Closure,Arity),Prog,Access,Closure,Arity).
programAccess(localFun(Prog,Access,Closure,Arity,_),Prog,Access,Closure,Arity).
programAccess(moduleVar(_,Prog,Access),Prog,Access,Access,1).
programAccess(localVar(Prog,Access,_),Prog,Access,Access,1).

labelAccess(Q,Q,[lyr(_,_,_,void)|_],_,enu("star.core#true")) :- !.
labelAccess(Q,Qx,[lyr(_,_,LblPtn,LbVr)|_],Lc,mtch(Lc,LblPtn,LbVr)) :- merge([LbVr],Q,Qx).

makeDotLbl(Nm,enu(Dot)) :-
  localName("",".",Nm,Dot).

mkUnit(tpl([])).
