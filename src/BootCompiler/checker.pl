:- module(checker,[checkProgram/4]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(macro).
:- use_module(dependencies).
:- use_module(freshen).
:- use_module(unify).
:- use_module(types).
:- use_module(parsetype).
:- use_module(dict).
:- use_module(misc).
:- use_module(canon).
:- use_module(errors).
:- use_module(keywords).
:- use_module(macro).
:- use_module(import).
:- use_module(transitive).
:- use_module(resolve).
:- use_module(display).
:- use_module(vartypes).

checkProgram(Prog,Pkg,Repo,
    prog(Pkg,Lc,Imports,ODefs,OOthers,Exports,Types,Cons,Impls)) :-
  stdDict(Base),
  isBraceTerm(Prog,Lc,_,Els),
  pushScope(Base,Env),
  Pkg = pkg(Pk,_),
  thetaEnv(Pk,Repo,Lc,Els,faceType([],[]),Env,_OEnv,Defs,Public,Imports,Others),
  findImportedImplementations(Imports,[],OverDict),
  overload(Defs,OverDict,ODict,ODefs),
  overloadOthers(Others,ODict,OOthers),
  computeExport(Defs,faceType([],[]),Public,Exports,Types,Cons,Impls),!.

thetaEnv(Pkg,Repo,Lc,Els,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  macroRewrite(Els,Stmts),
  collectDefinitions(Stmts,Dfs,Public,Annots,Imps,Otrs),
  (noErrors ->
    dependencies(Dfs,Groups,Annots),
    processImportGroup(Imps,Imports,Repo,Base,IBase),
    pushFace(Fields,Lc,IBase,Env),
    checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Pkg),
    checkOthers(Otrs,Others,TheEnv,Pkg);
    Defs=[],Others=[],Imports=[]).
%  dispDefs(Defs).

recordEnv(Path,Repo,_Lc,Els,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  macroRewrite(Els,Stmts),
  collectDefinitions(Stmts,Dfs,Public,Annots,Imps,Otrs),
  processImportGroup(Imps,Imports,Repo,Base,TmpEnv),
  parseAnnotations(Dfs,Fields,Annots,TmpEnv,Path,Face),
  checkGroup(Dfs,Defs,[],TmpEnv,TheEnv,Face,Path),
  checkOthers(Otrs,Others,TheEnv,Path).
%  dispDefs(Defs).

processImportGroup(Stmts,ImportSpecs,Repo,Env,Ex) :-
  findAllImports(Stmts,Lc,Imports),
  importAll(Imports,Repo,AllImports),
  importAllDefs(AllImports,Lc,ImportSpecs,Repo,Env,Ex),!.

findAllImports([],_,[]).
findAllImports([St|More],Lc,[Spec|Imports]) :-
  findImport(St,Lc,private,Spec),
  findAllImports(More,_,Imports).

findImport(St,Lc,_,Spec) :-
  isPrivate(St,Lc,I),
  findImport(I,_,private,Spec).
findImport(St,Lc,_,Spec) :-
  isPublic(St,Lc,I),
  findImport(I,_,public,Spec).
findImport(St,Lc,Viz,import(Viz,Pkg)) :-
  isImport(St,Lc,P),
  pkgName(P,Pkg).

importAll(Imports,Repo,AllImports) :-
  closure(Imports,[],checker:notAlreadyImported,checker:importMore(Repo),AllImports).

importDefs(spec(Pkg,faceType(Exported,Types),Enums,Cons,Impls,_),Lc,E,Evx) :-
  importTypes(Types,Lc,E,E1),
  declareImportedFields(Exported,Pkg,Enums,Lc,E1,E2),
  importContracts(Cons,Lc,E2,E3),
  importImplementations(Impls,E3,Evx).

declareImportedFields([],_,_,_,Env,Env).
declareImportedFields([(Nm,Tp)|More],Pkg,Enums,Lc,Env,Ex) :-
  declareImportedVar(Lc,Nm,Pkg,Enums,Tp,Env,E0),
  declareImportedFields(More,Pkg,Enums,Lc,E0,Ex).

declareImportedVar(Lc,Nm,pkg(Pkg,_),Enums,Tp,Env,E0) :-
  marker(class,Mrk),
  localName(Pkg,Mrk,Nm,LclNm),
  (is_member(LclNm,Enums),\+isProgramType(Tp) ->
     faceOfType(Tp,Env,FcTp),
     declareEnum(Lc,Nm,Tp,FcTp,Env,E0) ;
     declareVr(Lc,Nm,Tp,Env,E0)).

importTypes([],_,Env,Env).
importTypes([(Nm,Rule)|More],Lc,Env,Ex) :-
  pickTypeTemplate(Rule,Type),
  declareType(Nm,tpDef(Lc,Type,Rule),Env,E0),
  importTypes(More,Lc,E0,Ex).

importAllDefs([],_,[],_,Env,Env).
importAllDefs([import(Viz,Pkg)|More],Lc,
      [import(Viz,Pkg,Exported,Classes,Cons,Impls)|Specs],Repo,Env,Ex) :-
  importPkg(Pkg,Repo,Spec),
  Spec = spec(_,Exported,Classes,Cons,Impls,_),
  importDefs(Spec,Lc,Env,Ev0),
  importAllDefs(More,Lc,Specs,Repo,Ev0,Ex).

importContracts([],_,Env,Env).
importContracts([C|L],Lc,E,Env) :-
  C = conDef(Nm,_,_),
  defineContract(Nm,Lc,C,E,E0),
  importContracts(L,Lc,E0,Env).

notAlreadyImported(import(_,Pkg),SoFar) :-
  \+ is_member(import(_,Pkg),SoFar),!.

importMore(Repo,import(Viz,Pkg),SoFar,[import(Viz,Pkg)|SoFar],Inp,More) :-
  importPkg(Pkg,Repo,spec(_,_,_,_,_,Imports)),
  addPublicImports(Imports,Inp,More).
importMore(_,import(_,Pkg),SoFar,SoFar,Inp,Inp) :-
  reportError("could not import package %s",[Pkg]).

addPublicImports([],Imp,Imp).
addPublicImports([import(public,Pkg)|I],Rest,[import(transitive,Pkg)|Out]) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(private,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(transitive,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).

findImportedImplementations([import(_,_,_,_,_,Impls)|Specs],D,OverDict) :-
  rfold(Impls,checker:declImpl,D,D1),
  findImportedImplementations(Specs,D1,OverDict).
findImportedImplementations([],D,D).

declImpl(imp(ImplNm,Spec),SoFar,[(ImplNm,Spec)|SoFar]).

importImplementations(Impls,Ev,Evx) :-
  rfold(Impls,checker:declImplementation,Ev,Evx).

declImplementation(imp(ImplNm,Spec),Ev,Evx) :-
  marker(over,Mrk),
  splitLocalName(ImplNm,Mrk,Nm,_),
  declareImplementation(Nm,ImplNm,Spec,Ev,Evx).

checkOthers([],[],_,_).
checkOthers([St|Stmts],Ass,Env,Path) :-
  checkOther(St,Ass,More,Env,Path),!,
  checkOthers(Stmts,More,Env,Path).
checkOthers([St|Stmts],Ass,Env,Path) :-
  locOfAst(St,Lc),
  reportError("cannot understand statement: %s",[St],[Lc]),
  checkOthers(Stmts,Ass,Env,Path).

checkOther(St,[assertion(Lc,Cond)|More],More,Env,Path) :-
  isIntegrity(St,Lc,C),!,
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(C,LogicalTp,Env,_,Cond,Path).
checkOther(St,[show(Lc,Vl)|More],More,Env,Path) :-
  isShow(St,Lc,C),!,
  findType("string",Lc,Env,StrTp),
  typeOfExp(C,StrTp,Env,_,Vl,Path).

checkGroups([],_,_,[],E,E,_).
checkGroups([Gp|More],Fields,Annots,Defs,E,Ev,Path) :-
  parseAnnotations(Gp,Fields,Annots,E,Path,Face),!,
  groupLc(Gp,Lc),
  pushFace(Face,Lc,E,E0),
  checkGroup(Gp,Defs,D0,E0,E1,Face,Path),!,
  checkGroups(More,Fields,Annots,D0,E1,Ev,Path).

groupLc([(_,Lc,_)|_],Lc).

checkGroup([(con(N),Lc,[ConStmt])|More],[Contract|Defs],Dx,Env,Ex,Face,Path) :-
  parseContract(ConStmt,Env,Path,Contract),
  defineContract(N,Lc,Contract,Env,E0),
  checkGroup(More,Defs,Dx,E0,Ex,Face,Path).
checkGroup([(cns(Nm),Lc,[St])|More],Defs,Dx,Env,Ex,Face,Path) :-
  parseConstructor(Nm,Lc,St,Env,E0,Defs,D0,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Path).
checkGroup([(var(N),Lc,Stmts)|More],Defs,Dx,Env,Ex,Face,Path) :-
  checkVarRules(N,Lc,Stmts,Env,E0,Defs,D0,Face,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Path).
checkGroup([(imp(Nm),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Path) :-
  checkImplementation(Stmt,Nm,Defs,D0,Env,E0,Face,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Path).
checkGroup([(tpe(_),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Path) :-
  parseTypeDef(Stmt,Defs,D0,Env,E0,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Path).
checkGroup([],Defs,Defs,Env,Env,_,_).

defineContract(N,Lc,Contract,E0,Ex) :-
  declareContract(N,Contract,E0,E1),
  declareMethods(Contract,Lc,E1,Ex).

declareMethods(conDef(_,_,ConEx),Lc,Env,Ev) :-
  moveQuants(ConEx,Q,C1),
  moveConstraints(C1,Cx,contractExists(CTract,faceType(Methods,[]))),
  formMethods(Methods,Lc,Q,Cx,CTract,Env,Ev).

formMethods([],_,_,_,_,Env,Env).
formMethods([(Nm,Tp)|M],Lc,Q,Cx,Con,Env,Ev) :-
  moveQuants(Tp,FQ,QTp),
  merge(FQ,Q,MQ),
  moveConstraints(CC,Cx,constrained(QTp,Con)),
  moveQuants(MTp,MQ,CC),
  declareMtd(Lc,Nm,MTp,Env,E0),
  formMethods(M,Lc,Q,Cx,Con,E0,Ev).

parseConstructor(Nm,Lc,T,Env,Ev,[cnsDef(Lc,Nm,ConVr,Tp)|Defs],Defs,_) :-
  parseType(T,Env,Tp),
  (isConType(Tp) ->
    declareCns(Lc,Nm,Tp,Env,Ev), ConVr=cns(Lc,Nm) ;
    faceOfType(Tp,Env,FcTp),
    declareEnum(Lc,Nm,Tp,FcTp,Env,Ev), ConVr=enm(Lc,Nm)).

parseAnnotations(Defs,Fields,Annots,Env,Path,faceType(F,T)) :-
  parseAnnots(Defs,Fields,Annots,Env,[],F,[],T,Path).

parseAnnots([],_,_,_,Face,Face,Tps,Tps,_) :-!.
parseAnnots([(var(Nm),Lc,_)|More],Fields,Annots,Env,F0,Face,T,Tps,Path) :-
  parseAnnotation(Nm,Lc,Fields,Annots,Env,F0,F1),
  parseAnnots(More,Fields,Annots,Env,F1,Face,T,Tps,Path).
parseAnnots([(tpe(N),Lc,[Stmt])|More],Fields,Annots,Env,F,Face,T,Tps,Path) :-
  defineType(N,Lc,Stmt,Env,T,T1,Path),
  parseAnnots(More,Fields,Annots,Env,F,Face,T1,Tps,Path).
parseAnnots([_|More],Fields,Annots,Env,F,Face,T,Tps,Path) :-
  parseAnnots(More,Fields,Annots,Env,F,Face,T,Tps,Path).

parseAnnotation(Nm,_,_,Annots,Env,F,[(Nm,Tp)|F]) :-
  is_member((Nm,T),Annots),!,
  parseType(T,Env,Tp).
parseAnnotation(N,_,faceType(Fields,_),_,_,F,[(N,Tp)|F]) :-
  is_member((N,Tp),Fields),!.
parseAnnotation(_,_,_,_,_,Face,Face).

defineType(N,_,_,Env,T,[(N,Tp)|T],_) :-
  isType(N,Env,tpDef(_,Tp,_)),!.
defineType(N,_,St,_,T,[(N,Type)|T],Path) :-
  parseTypeCore(St,Type,Path).
defineType(_,Lc,St,_,T,T,_) :-
  reportError("cannot parse type statement %s",[St],Lc).

parseTypeAnnotation(N,_,faceType(_,Types),_,_,F,[(N,Tp)|F]) :-
  is_member((N,Tp),Types),!.
parseTypeAnnotation(N,Lc,_,_,_,Face,Face) :-
  reportError("no type annotation for variable %s",[N],Lc).

checkVarRules(N,Lc,Stmts,E,Ev,Defs,Dx,Face,Path) :-
  pickupVarType(N,Lc,Face,Stmts,E,E0,Tp),
  evidence(Tp,E0,Q,PT),
  simplifyType(PT,E,Cx,[],ProgramType),
  declareConstraints(Cx,E0,E1),
  declareTypeVars(Q,Lc,E1,E2),
  processStmts(Stmts,ProgramType,Rules,Deflts,Deflts,[],E2,Path),
  packageVarName(Path,N,LclName),
  collectPrograms(Rules,N,LclName,E,Ev,Tp,Cx,Defs,Dx).

processStmts([],_,Defs,Defs,Dflts,Dflts,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Df,Dfx,Env,Path) :-
  processStmt(St,ProgramType,Defs,D0,Df,Df0,Env,Path),!,
  processStmts(More,ProgramType,D0,Dx,Df0,Dfx,Env,Path).

processStmt(St,ProgramType,Defs,Defx,Df,Dfx,E,Path) :-
  isEquation(St,Lc,L,Cond,R),!,
  checkEquation(Lc,L,Cond,R,ProgramType,Defs,Defx,Df,Dfx,E,Path).
processStmt(St,Tp,[Def|Defs],Defs,Df,Df,Env,Path) :-
  isDefn(St,Lc,L,R),
  checkDefn(Lc,L,R,Tp,Def,Env,Path),!.
processStmt(St,Tp,[Def|Defs],Defs,Df,Df,Env,Path) :-
  isAssignment(St,Lc,L,R),!,
  checkVarDefn(Lc,L,R,Tp,Def,Env,Path).
processStmt(St,Tp,Defs,Defs,Df,Df,_,_) :-
  locOfAst(St,Lc),
  reportError("Statement %s not consistent with expected type %s",[St,Tp],Lc).

pickupVarType(N,_,faceType(F,_),_,Ev,Ev,Tp) :-
  is_member((N,Tp),F),!.
pickupVarType(Nm,Lc,_,Stmts,E,Ev,Tp) :-
  guessStmtType(Stmts,Nm,Lc,Tp),
  declareVr(Lc,Nm,Tp,E,Ev).

guessStmtType([],N,Lc,VTp) :- !,
  reportError("%s not declared",[N],Lc),
  newTypeVar("_",VTp).
guessStmtType([St|_],N,Lc,Guess) :-
  guessType(St,N,Lc,Guess).

guessType(St,_,_,funType(tupleType(AT),RTp)) :-
  isEquation(St,_,H,_,_),!,
  splitHead(H,_,tuple(_,_,Args),_),
  genTpVars(Args,AT),
  newTypeVar("_",RTp).
guessType(St,_,_,GTp) :-
  isDefn(St,_,_,_),!,
  newTypeVar("_",GTp).
guessType(St,_,_,GTp) :-
  isAssignment(St,_,_,_),!,
  newTypeVar("_",GTp).
guessType(_,N,Lc,GTp) :- !,
  reportError("%s not declared",[N],Lc),
  newTypeVar("_",GTp).

pickupThisType(Env,Tp) :-
  isVar("this",Env,vrEntry(_,_,Tp,_)),!.
pickupThisType(_,voidType).

declareConstraints([],Env,Env).
declareConstraints([C|L],E,Ex) :-
  declareConstraint(C,E,E0),
  declareConstraints(L,E0,Ex).

findType(Nm,_,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,_)),!.
findType(Nm,Lc,_,anonType) :-
  reportError("type %s not known",[Nm],Lc).

checkEquation(Lc,H,C,R,funType(AT,RT),Defs,Defsx,Df,Dfx,E,Path) :-
  splitHead(H,_,A,IsDeflt),
  pushScope(E,Env),
  typeOfArgPtn(A,AT,Env,E0,Args,Path),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(C,LogicalTp,E0,E1,Cond,Path),
  typeOfExp(R,RT,E1,_E2,Exp,Path),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[equation(Lc,Args,Cond,Exp)|Dfx]; Defs=[equation(Lc,Args,Cond,Exp)|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("equation not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,Tp,varDef(Lc,Nm,ExtNm,[],Tp,Value),Env,Path) :-
  splitHead(L,Nm,none,_),
  pushScope(Env,E),
  typeOfExp(R,Tp,E,_E2,Value,Path),
  packageVarName(Path,Nm,ExtNm).

checkVarDefn(Lc,L,R,ref(Tp),vdefn(Lc,Nm,ExtNm,[],Tp,Value),Env,Path) :-
  splitHead(L,Nm,none,_),
  pushScope(Env,E1),
  typeOfExp(R,Tp,E1,_E2,Value,Path),
  packageVarName(Path,Nm,ExtNm).

checkThetaBody(Tp,Lc,Els,Env,OEnv,Defs,Others,Types,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  moveConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Cx,E0,BaseEnv),
  declareVr(Lc,"this",ETp,Face,BaseEnv,ThEnv),
  thetaEnv(Path,nullRepo,Lc,Els,Face,ThEnv,OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,Face,Public,_,Types,[],[]),!.

checkRecordBody(Tp,Lc,Els,Env,OEnv,Defs,Others,Types,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,Face),
  moveConstraints(Face,Cx,CFace),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Cx,E0,BaseEnv),
  declareVr(Lc,"this",Tp,Face,BaseEnv,ThEnv),
  recordEnv(Path,nullRepo,Lc,Els,CFace,ThEnv,OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,CFace,Public,_,Types,[],[]),!.

checkLetExp(Tp,Lc,Th,Ex,Env,letExp(Lc,theta(Lc,ThPath,Defs,Others,Types,Tp),Bound),Path):-
  isBraceTuple(Th,_,Els),!,
  genstr("Γ",ThNm),
  thetaName(Path,ThNm,ThPath),
  newTypeVar("_",ThTp),
  checkThetaBody(ThTp,Lc,Els,Env,ThEnv,Defs,Others,Types,ThPath),
  typeOfExp(Ex,Tp,ThEnv,_,Bound,Path).
checkLetExp(Tp,Lc,Th,Ex,Env,letExp(Lc,record(Lc,ThPath,Defs,Others,Types,Tp),Bound),Path):-
  isQBraceTuple(Th,_,Els),!,
  genstr("Γ",ThNm),
  thetaName(Path,ThNm,ThPath),
  newTypeVar("_",ThTp),
  checkThetaBody(ThTp,Lc,Els,Env,ThEnv,Defs,Others,Types,ThPath),
  typeOfExp(Ex,Tp,ThEnv,_,Bound,Path).

splitHead(tuple(_,"()",[A]),Nm,Args,IsDeflt) :-!,
  splitHd(A,Nm,Args,IsDeflt).
splitHead(Term,Nm,Args,IsDeflt) :-
  splitHd(Term,Nm,Args,IsDeflt).

splitHd(Term,Nm,A,isDeflt) :-
  isDefault(Term,_,Lhs),!,
  splitHd(Lhs,Nm,A,_).
splitHd(Term,Nm,A,notDeflt) :-
  isRoundTerm(Term,Lc,O,Args),
  isIden(O,_,Nm),
  roundTuple(Lc,Args,A).
splitHd(Id,Nm,none,isDeflt) :-
  isIden(Id,_,Nm),!.
splitHd(Term,"()",Term,isDeflt) :-
  isTuple(Term,_).

collectPrograms([Eqn|Eqns],Nm,LclNm,Env,Ev,Tp,Cx,[funDef(Lc,Nm,LclNm,Tp,Cx,[Eqn|Eqns])|Dx],Dx) :-
  Eqn = equation(Lc,_,_,_),
  declareVr(Lc,Nm,Tp,Env,Ev).
collectPrograms([varDef(Lc,_,_,_,_,Value)],Nm,LclNm,Env,Ev,Tp,Cx,
    [varDef(Lc,Nm,LclNm,Cx,Tp,Value)|Dx],Dx) :-
  faceOfType(Tp,Env,Face),
  freshen(Face,Env,_,VFace),
  declareVr(Lc,Nm,Tp,VFace,Env,Ev).
collectPrograms([varDef(Lc,_,_,_,refType(Tp),Value)],Nm,LclNm,Env,Ev,Tp,Cx,
    [vdefn(Lc,Nm,LclNm,Cx,Tp,Value)|Dx],Dx) :-
  declareVr(Lc,Nm,refType(Tp),Env,Ev).

checkImplementation(Stmt,INm,[Impl,ImplDef|Dfs],Dfs,Env,Ex,_,_Path) :-
  isImplementationStmt(Stmt,Lc,Quants,Cons,Sq,IBody),
  parseContractConstraint(Quants,Cons,Sq,Env,Nm,ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
  moveConstraints(CnSpec,AC,contractExists(Spec,IFace)),
  declareTypeVars(IQ,Lc,Env,ThEnv),
  implementationName(Spec,ImplName),
  typeOfExp(IBody,IFace,ThEnv,_ThEv,ImplTerm,ImplName),
  Impl = implDef(Lc,INm,ImplName,ConSpec),
  rfold(IQ,checker:pickBoundType,IFace,ImplTp),
  ImplDef = varDef(Lc,INm,ImplName,AC,ImplTp,ImplTerm),
  declareImplementation(Nm,ImplName,ConSpec,Env,Ex),!.
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_) :-
  locOfAst(Stmt,Lc),
  reportError("could not check implementation statement",[Lc]).

pickBoundType((_,Tv),Tp,allType(Tv,Tp)).

sameLength(L1,L2,_) :- length(L1,L), length(L2,L),!.
sameLength(L1,_,Lc) :-
  length(L1,L),
  reportError("expecting %s elements",[L],Lc).

% Patterns are very similarly checked to expressions, except that fewer forms
typeOfArgPtn(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgPtn(T,Tp,Env,Ev,Exp,Path) :-
  typeOfPtn(T,Tp,Env,Ev,Exp,Path).

typeOfPtn(V,_,Env,Env,v(Lc,N),_Path) :-
  isIden(V,Lc,"_"),!,
  genstr("_",N).
typeOfPtn(V,Tp,Env,Ev,Term,Path) :-
  isIden(V,Lc,N),
  isVar(N,Env,Spec),!,
  (isEnumVr(Lc,Spec) ->
    typeOfVar(Lc,N,Tp,Spec,Env,Ev,Term);
    mkWhereEquality(V,TT),
    typeOfPtn(TT,Tp,Env,Ev,Term,Path)).
typeOfPtn(V,Tp,Ev,Env,v(Lc,N),_Path) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,Ev,Env).
typeOfPtn(integer(Lc,Ix),Tp,Env,Env,intLit(Ix),_Path) :- !,
  findType("integer",Lc,Env,IntTp),
  checkType(Lc,IntTp,Tp,Env).
typeOfPtn(float(Lc,Ix),Tp,Env,Env,floatLit(Ix),_Path) :- !,
  findType("float",Lc,Env,FltTp),
  checkType(Lc,FltTp,Tp,Env).
typeOfPtn(string(Lc,Ix),Tp,Env,Env,stringLit(Ix),_Path) :- !,
  findType("string",Lc,Env,StrTp),
  checkType(Lc,StrTp,Tp,Env).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  checkType(Lc,RT,Tp,Env),
  typeOfPtn(L,Tp,Env,Ev,Exp,Path).
typeOfPtn(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),
  typeOfPtn(L,Tp,Env,E0,Ptn,Path),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(C,LogicalTp,E0,Ex,Cond,Path).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isSquareTuple(Term,Lc,Els), !,
  macroSquarePtn(Lc,Els,Ptn),
  typeOfPtn(Ptn,Tp,Env,Ev,Exp,Path).
typeOfPtn(Trm,Tp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfPtn(Inner,Tp,Env,Ev,Exp,Path).
typeOfPtn(Trm,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfPtn(Term,Tp,Env,Ev,Ptn,Path) :-
  isOptionPtn(Term,Lc,Pt,Ex),
  mkWherePtn(Lc,Pt,Ex,Trm),
  typeOfPtn(Trm,Tp,Env,Ev,Ptn,Path).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("A",At),
  typeOfKnown(F,consType(At,Tp),Env,E0,Fun,Path),
  evidence(At,E0,_,AT),
  typeOfArgPtn(tuple(Lc,"()",A),AT,E0,Ev,Args,Path),
  Exp = apply(Lc,Fun,Args).
typeOfPtn(Term,Tp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal pattern: %s, expecting a %s",[Term,Tp],Lc).

typeOfArgTerm(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgTerm(T,Tp,Env,Ev,Exp,Path) :-
  typeOfExp(T,Tp,Env,Ev,Exp,Path).

typeOfExp(V,Tp,Env,Ev,Term,_Path) :-
  isIden(V,Lc,N),
  isVar(N,Env,Spec),!,
  typeOfVar(Lc,N,Tp,Spec,Env,Ev,Term).
typeOfExp(integer(Lc,Ix),Tp,Env,Env,intLit(Ix),_Path) :- !,
  findType("integer",Lc,Env,IntTp),
  checkType(Lc,IntTp,Tp,Env).
typeOfExp(float(Lc,Ix),Tp,Env,Env,floatLit(Ix),_Path) :- !,
  findType("float",Lc,Env,FltTp),
  checkType(Lc,FltTp,Tp,Env).
typeOfExp(string(Lc,Ix),Tp,Env,Env,stringLit(Ix),_Path) :- !,
  findType("string",Lc,Env,StrTp),
  checkType(Lc,StrTp,Tp,Env).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  checkType(Lc,RT,Tp,Env),
  typeOfExp(L,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  binary(Lc,":",LT,R,NT),
  typeOfExp(NT,Tp,Env,Ev,Exp,Path).
typeOfExp(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),
  typeOfExp(L,Tp,Env,E0,Ptn,Path),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(C,LogicalTp,E0,Ex,Cond,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  recordAccessExp(Lc,Rc,Fld,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,cond(Lc,Test,Then,Else),Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(Tst,LogicalTp,Env,E0,Test,Path),
  typeOfExp(Th,Tp,E0,E1,Then,Path),
  typeOfExp(El,Tp,E1,Ev,Else,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSquareTuple(Term,Lc,Els), !,
  squareTupleExp(Lc,Els,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,theta(Lc,ThPath,Defs,Others,Types,Tp),Path) :-
  isBraceTuple(Term,Lc,Els),
  genstr("θ",ThNm),
  thetaName(Path,ThNm,ThPath),
  checkThetaBody(Tp,Lc,Els,Env,_,Defs,Others,Types,ThPath).
typeOfExp(Term,Tp,Env,Env,record(Lc,ThPath,Defs,Others,Types,Tp),Path) :-
  isQBraceTuple(Term,Lc,Els),
  genstr("ρ",RcNm),
  thetaName(Path,RcNm,ThPath),
  checkRecordBody(Tp,Lc,Els,Env,_,Defs,Others,Types,ThPath).
typeOfExp(Term,Tp,Env,Env,theta(Lc,ThPath,Defs,Others,Types,Tp),Path) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfKnown(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  genstr(Lbl,ThNm),
  thetaName(Path,ThNm,ThPath),
  deRef(FnTp,BTp),
  checkThetaBody(BTp,Lc,Els,E0,_,Defs,Others,Types,ThPath).
typeOfExp(Term,Tp,Env,Env,record(Lc,ThPath,Defs,Others,Types,Tp),Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("R",FnTp),
  typeOfKnown(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  genstr(Lbl,RcNm),
  thetaName(Path,RcNm,ThPath),
  checkRecordBody(FnTp,Lc,Els,E0,_,Defs,Others,Types,Lbl,ThPath).
typeOfExp(Term,Tp,Ev,Ev,LetExp,Path) :-
  isLetDef(Term,Lc,Th,Ex),
  checkLetExp(Tp,Lc,Th,Ex,Ev,LetExp,Path).
typeOfExp(Trm,Tp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfExp(Inner,Tp,Env,Ev,Exp,Path).
typeOfExp(Trm,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isUnary(Term,Lc,"-",Arg), % handle unary minus
  (Arg=integer(_,Ix) ->
    findType("integer",Lc,Env,IntTp),
    checkType(Lc,IntTp,Tp,Env),
    Env=Ev,
    Ng is -Ix,
    Exp = intLit(Ng) ;
  Arg=float(_,Dx) ->
    findType("float",Lc,Env,FltTp),
    checkType(Lc,FltTp,Tp,Env),
    Env=Ev,
    Ng is -Dx,
    Exp = floatLit(Ng) ;
  unary(Lc,"__minus",Arg,Sub),
  typeOfExp(Sub,Tp,Env,Ev,Exp,Path)).
typeOfExp(Term,Tp,Env,Ev,search(Lc,Ptn,Src),Path) :-
  isSearch(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  newTypeFun("_#",1,FT),
  typeOfPtn(L,TV,Env,E0,Ptn,Path),
  typeOfExp(R,tpExp(FT,TV),E0,Ev,Src,Path).

typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  typeOfRoundTerm(Lc,F,A,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSquareTerm(Term,Lc,F,[A]),!,
  typeOfIndex(Lc,F,A,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isOfTerm(Term,Lc,Lbl,Tpl),!,
  macroOfTerm(Lbl,Lc,Tpl,Trm),
  typeOfExp(Trm,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Lam,Path) :-
  isEquation(Term,Lc,H,C,R),
  typeOfLambda(Lc,H,C,R,Tp,Env,Lam,Path).
typeOfExp(Term,Tp,Env,Ex,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ex,disj(Lc,Lhs,Rhs),Path) :-
  isDisjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ex,neg(Lc,Rhs),Path) :-
  isNegation(Term,Lc,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  typeOfExp(R,LogicalTp,Env,Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,Env,E0,Lhs,Path),
  typeOfExp(E,TV,E0,Ev,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isOptionMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  unary(Lc,"some",P,SP),
  typeOfPtn(SP,TV,Env,E0,Lhs,Path),
  typeOfExp(E,TV,E0,Ev,Rhs,Path).

typeOfExp(Term,Tp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

funLbl(over(_,T,_,_),L) :- funLbl(T,L).
funLbl(v(_,L),L).
funLbl(cns(_,Nm),Nm).
funLbl(enm(_,Nm),Nm).
funLbl(mtd(_,Nm),Nm).

typeOfRoundTerm(Lc,F,A,Tp,Env,Ev,Exp,Path) :-
  newTypeVar("F",FnTp),
  newTypeVar("A",At),
  typeOfKnown(F,FnTp,Env,E0,Fun,Path),
  simplifyType(FnTp,Env,_,[],FTp),
  evidence(At,E0,_,AT),
  (sameType(funType(At,Tp),FTp,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args,Path),
    Exp = apply(Lc,Fun,Args) ;
  sameType(consType(At,Tp),FTp,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args,Path),
    Exp = apply(Lc,Fun,Args);
  reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FTp,At,Tp],Lc),
  Env=Ev).


  % typeOfKnown(F,FnTp,Env,E0,Fun,Path),
  % simplifyType(FnTp,Env,_,[],FTp),
  % evidence(At,E0,_,AT),
  % typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args,Path),
  % (sameType(funType(At,Tp),FTp,E0) ->
  %  Exp = apply(Lc,Fun,Args) ;
  % sameType(consType(At,Tp),FTp,E0) ->
  %  Exp = apply(Lc,Fun,Args);
  % reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FTp,At,Tp],Lc)).

typeOfLambda(Lc,H,C,R,Tp,Env,lambda(Lc,[equation(Lc,Args,Cond,Exp)],Tp),Path) :-
  newTypeVar("_A",AT),
  typeOfArgPtn(H,AT,Env,E1,Args,Path),
  newTypeVar("_E",RT),
  checkType(Lc,funType(AT,RT),Tp,Env),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(C,LogicalTp,E1,E2,Cond,Path),
  typeOfExp(R,RT,E2,_,Exp,Path).

typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  isBinary(Arg,_,"->",Ky,Vl),!,
  ternary(Lc,"_replace",Mp,Ky,Vl,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  isUnary(Arg,_,"\\+",Ky),!,
  binary(Lc,"_remove",Mp,Ky,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  binary(Lc,"_index",Mp,Arg,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

recordFace(Trm,Env,Env,v(Lc,Nm),Face,_) :-
  isIden(Trm,Lc,Nm),
  isVar(Nm,Env,vrEntry(_,_,_,Fce)),!,
  call(Fce,Env,Face).
recordFace(Trm,Env,Ev,Rec,Face,Path) :-
  newTypeVar("_R",AT),
  typeOfKnown(Trm,AT,Env,Ev,Rec,Path),
  faceOfType(AT,Env,Fc),
  freshen(Fc,Env,_,Face).

recordAccessExp(Lc,Rc,Fld,ET,Env,Ev,dot(Lc,Rec,Fld),Path) :-
  recordFace(Rc,Env,Ev,Rec,Fce,Path),
  deRef(Fce,Face),
  fieldInFace(Face,Fld,Lc,FTp),!,
  freshen(FTp,Env,_,Tp),
  checkType(Lc,Tp,ET,Env).

macroMapEntries(Lc,[],name(Lc,"_empty")).
macroMapEntries(_,[E|L],T) :-
  isBinary(E,Lc,"->",Ky,Vl),!,
  macroMapEntries(Lc,L,Tr),
  roundTerm(Lc,"_replace",[Tr,Ky,Vl],T).
macroMapEntries(Lc,[E|L],T) :-
  reportError("invalid entry in map %s",[E],Lc),
  macroMapEntries(Lc,L,T).

fieldInFace(faceType(Fields,_),Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfKnown(T,Tp,Env,Ev,Exp,_) :-
  isIden(T,Lc,Nm),
  isVar(Nm,Env,Spec),!,
  typeOfVar(Lc,Nm,Tp,Spec,Env,Ev,Exp).
typeOfKnown(T,Tp,Env,Env,v(Lc,Nm),_) :-
  isIden(T,Lc,Nm),
  reportError("variable %s not declared, expecting a %s",[Nm,Tp],Lc).
typeOfKnown(T,Tp,Env,Ev,Exp,Path) :-
  typeOfExp(T,Tp,Env,Ev,Exp,Path).

typeOfTerms([],[],Env,Env,_,[],_).
typeOfTerms([],[T|_],Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfTerms([A|_],[],Env,Env,_,[],_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfTerms([A|As],[ETp|ElTypes],Env,Ev,_,[Term|Els],Path) :-
  deRef(ETp,ElTp),
  typeOfExp(A,ElTp,Env,E0,Term,Path),
  locOfAst(A,Lc),
  typeOfTerms(As,ElTypes,E0,Ev,Lc,Els,Path).

typeOfPtns([],[],Env,Env,_,[],_).
typeOfPtns([],[T|_],Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfPtns([A|_],[],Env,Env,_,[],_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfPtns([A|As],[ETp|ElTypes],Env,Ev,_,[Term|Els],Path) :-
  deRef(ETp,ElTp),
  typeOfPtn(A,ElTp,Env,E0,Term,Path),
  locOfAst(A,Lc),
  typeOfPtns(As,ElTypes,E0,Ev,Lc,Els,Path).

% Analyse a list term to try to disambiguate maps from lists.

squareTupleExp(Lc,Els,Tp,Env,Ev,Exp,Path) :-
  (isMapSequence(Els) ; isMapType(Tp,Env)) ->
    macroMapEntries(Lc,Els,Trm),
    newTypeVar("k",KT),
    newTypeVar("v",VT),
    findType("map",Lc,Env,MapOp),
    MapTp = tpExp(tpExp(MapOp,KT),VT),
    checkType(Lc,MapTp,Tp,Env),
    typeOfExp(Trm,Tp,Env,Ev,Exp,Path);
  macroListEntries(Lc,Els,Trm,nilGen,consGen,appndGen),
  typeOfExp(Trm,Tp,Env,Ev,Exp,Path).

nilGen(Lc,name(Lc,"_nil")).

consGen(Lc,L,R,Trm) :-
  binary(Lc,"_cons",L,R,Trm).

appndGen(Lc,L,R,Trm) :-
  binary(Lc,"_apnd",L,R,Trm).

isMapSequence([E|_]) :-
  isBinary(E,_,"->",_,_).

isMapType(Tp,Env) :-
  isType("map",Env,tpDef(_,MpTp,_)),!,
  deRef(Tp,tpExp(MpOp,_)),
  moveQuants(MpTp,_,tpExp(MapOp,_)),
  deRef(MpOp,MapOp).

isListSequence([E|_]) :-
  \+isBinary(E,_,"->",_,_).

isListType(Tp,Env) :-
  isType("list",Env,tpDef(_,LstTp,_)),!,
  deRef(Tp,tpExp(LsOp,_)),
  moveQuants(LstTp,_,tpExp(LstOp,_)),
  deRef(LsOp,LstOp).

macroOfTerm(Lbl,Lc,Tpl,Trm) :-
  squareTerm(Lc,Lbl,[name(Lc,"_")],Tp),
  binary(Lc,":",Tpl,Tp,Trm).

macroSquarePtn(Lc,Els,Ptn) :-
  macroListEntries(Lc,Els,Ptn,genEofTest,genHedTest,genTailTest).

genEofTest(Lc,Trm) :-
  mkWhere(Lc,"_eof",Trm).

genEndTest(Lc,name(Lc,"_")).

genHedTest(Lc,L,R,Trm) :-
  mkWherePtn(Lc,tuple(Lc,"()",[L,R]),name(Lc,"_hdtl"),Trm).

genTailTest(Lc,L,R,Trm) :-
  mkWherePtn(Lc,tuple(Lc,"()",[L,R]),name(Lc,"_back"),Trm).

macroListEntries(Lc,[],Trm,End,_,_) :-
  call(End,Lc,Trm).
macroListEntries(_,[Cns],Trm,_,_,Tail) :-
  isLConsTerm(Cns,_,H,T),
  macroLListEntries(T,H,Trm,Tail).
macroListEntries(_,[Cns],Trm,_,Hed,_) :-
  isConsTerm(Cns,Lc,H,T),
  call(Hed,Lc,H,T,Trm).
macroListEntries(Lc,[E|L],Trm,End,Hed,Tail) :-
  macroListEntries(Lc,L,Tr,End,Hed,Tail),
  call(Hed,Lc,E,Tr,Trm).

macroLListEntries(T,S,Trm,Tail) :-
  isBinary(T,Lc,",",H,Tl),!,
  call(Tail,Lc,S,H,TT),
  macroLListEntries(Tl,TT,Trm,Tail).
macroLListEntries(T,S,Trm,Tail) :-
  locOfAst(T,Lc),
  call(Tail,Lc,S,T,Trm).

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Lc,S,T,_) :-
  reportError("%s not consistent with expected type %s",[S,T],Lc).

computeExport([],_,_,[],[],[],[]).
computeExport([Def|Defs],Fields,Public,Exports,Types,Cons,Impls) :-
  exportDef(Def,Fields,Public,Exports,Ex,Types,Tx,Cons,Cx,Impls,Ix),!,
  computeExport(Defs,Fields,Public,Ex,Tx,Cx,Ix).

exportDef(funDef(_,Nm,_,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(typeDef(_,Nm,_,FRule),_,Public,Ex,Ex,[(Nm,FRule)|Tx],Tx,Cx,Cx,Impl,Impl) :-
  isPublicType(Nm,Public).
exportDef(varDef(_,Nm,_,_,Tp,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(cnsDef(_,Nm,_,Tp),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(vdefn(_,Nm,_,Tp,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl):-
  isPublicVar(Nm,Fields,Public).
exportDef(Con,_,Public,Ex,Ex,Types,Types,[Con|Cx],Cx,Impl,Impl) :-
  isPublicContract(Con,Public).
exportDef(implDef(_,INm,IName,Spec),_,Public,Ex,Ex,Tps,Tps,Cx,Cx,
    [imp(IName,Spec)|Ix],Ix) :-
  is_member(imp(INm),Public),!.
exportDef(_,_,_,Ex,Ex,Tps,Tps,Cx,Cx,Impls,Impls).

isPublicVar(Nm,_,Public) :-
  is_member(var(Nm),Public),!.
isPublicVar(Nm,Fields,_) :-
  is_member((Nm,_),Fields),!.

isPublicType(Nm,Public) :-
  is_member(tpe(Nm),Public),!.

isPublicContract(conDef(Nm,_,_),Public) :-
  is_member(con(Nm),Public),!.
