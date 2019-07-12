:- module(checker,[checkProgram/5]).

:- use_module(abstract).
:- use_module(wff).
:- use_module(macro).
:- use_module(do).
:- use_module(dependencies).
:- use_module(freshen).
:- use_module(freevars).
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
:- use_module(cnc).
:- use_module(vartypes).

checkProgram(Prog,Pkg,Repo,_Opts,
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

thetaEnv(Pkg,Repo,Lc,Stmts,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  collectDefinitions(Stmts,Dfs,Public,Annots,Imps,Otrs),
  (noErrors ->
    dependencies(Dfs,Groups,Annots),
    processImportGroup(Imps,Imports,Repo,Base,IBase),
    pushFace(Fields,Lc,IBase,Env),
    checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Pkg),
    checkOthers(Otrs,Others,TheEnv,Pkg);
    Defs=[],Others=[],Imports=[],TheEnv=Base).
%  dispDefs(Defs).

recordEnv(Path,Repo,_Lc,Stmts,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  collectDefinitions(Stmts,Dfs,Public,Annots,Imps,Otrs),
  processImportGroup(Imps,Imports,Repo,Base,TmpEnv),
  parseAnnotations(Dfs,Fields,Annots,TmpEnv,Path,Face),
  checkGroup(Dfs,Defs,[],TmpEnv,TheEnv,Face,Path),
  checkOthers(Otrs,Others,TheEnv,Path).
%  dispDefs(Defs).

processImportGroup(Stmts,ImportSpecs,Repo,Env,Ex) :-
  findAllImports(Stmts,Imports),
  importAll(Imports,Repo,AllImports),
  importAllDefs(AllImports,ImportSpecs,Repo,Env,Ex),!.

findAllImports([],[]).
findAllImports([St|More],[Spec|Imports]) :-
  findImport(St,private,Spec),
  findAllImports(More,Imports).

findImport(St,_,Spec) :-
  isPrivate(St,_Lc,I),
  findImport(I,private,Spec).
findImport(St,_,Spec) :-
  isPublic(St,_,I),
  findImport(I,public,Spec).
findImport(St,Viz,import(Lc,Viz,Pkg)) :-
  isImport(St,Lc,P),
  pkgName(P,Pkg).

importAll(Imports,Repo,AllImports) :-
  closure(Imports,[],checker:notAlreadyImported,checker:importMore(Repo),AllImports).

importMore(Repo,import(Lc,Viz,Pkg),SoFar,[import(Lc,Viz,Pkg)|SoFar],Inp,More) :-
  importPkg(Pkg,Lc,Repo,spec(_,_,_,_,_,Imports)),
  addPublicImports(Imports,Inp,More).
importMore(_,import(Lc,_,Pkg),SoFar,SoFar,Inp,Inp) :-
  reportError("could not import package %s",[Pkg],Lc).

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
  typeTemplate(Nm,Rule,Type),
  declareType(Nm,tpDef(Lc,Type,Rule),Env,E0),
  importTypes(More,Lc,E0,Ex).

importAllDefs([],[],_,Env,Env).
importAllDefs([import(Lc,Viz,Pkg)|More],
      [import(Viz,Pkg,Exported,Classes,Cons,Impls)|Specs],Repo,Env,Ex) :-
  importPkg(Pkg,Lc,Repo,Spec),
  Spec = spec(_,Exported,Classes,Cons,Impls,_),
  importDefs(Spec,Lc,Env,Ev0),
  importAllDefs(More,Specs,Repo,Ev0,Ex).

importContracts([],_,Env,Env).
importContracts([C|L],Lc,E,Env) :-
  C = conDef(Nm,_,_),
  defineContract(Nm,Lc,C,E,E0),
  importContracts(L,Lc,E0,Env).

notAlreadyImported(import(_,_,Pkg),SoFar) :-
  \+ is_member(import(_,_,Pkg),SoFar),!.

addPublicImports([],Imp,Imp).
addPublicImports([import(Lc,public,Pkg)|I],Rest,[import(Lc,transitive,Pkg)|Out]) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(_,private,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(_,transitive,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).

findImportedImplementations([import(_,_,_,_,_,Impls)|Specs],D,OverDict) :-
  rfold(Impls,checker:declImpl,D,D1),
  findImportedImplementations(Specs,D1,OverDict).
findImportedImplementations([],D,D).

declImpl(imp(_,FullNm,Spec),SoFar,[(FullNm,Spec)|SoFar]).

importImplementations(Impls,Ev,Evx) :-
  rfold(Impls,checker:declImplementation,Ev,Evx).

declImplementation(imp(ImplNm,FullNm,Spec),Ev,Evx) :-
  declareImplementation(ImplNm,FullNm,Spec,Ev,Evx).

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
  checkGoal(C,Env,_,Cond,Path).
checkOther(St,[show(Lc,Vl)|More],More,Env,Path) :-
  isShow(St,Lc,C),!,
  findType("string",Lc,Env,StrTp),
  unary(Lc,"_coerce",C,LT),
  binary(Lc,":",LT,name(Lc,"string"),NT),
  typeOfExp(NT,StrTp,Env,_,Vl,Path).

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
    declareCns(Lc,Nm,Tp,Env,Ev), ConVr=cons(Lc,Nm,Tp) ;
    faceOfType(Tp,Env,FcTp),
    declareEnum(Lc,Nm,Tp,FcTp,Env,Ev), ConVr=enm(Lc,Nm,Tp)).

parseAnnotations(Defs,Fields,Annots,Env,Path,faceType(F,T)) :-
  parseAnnots(Defs,Fields,Annots,Env,[],F,[],T,Path).

parseAnnots([],_,_,_,Face,Face,Tps,Tps,_) :-!.
parseAnnots([(var(Nm),Lc,Stmts)|More],Fields,Annots,Env,F0,Face,T,Tps,Path) :-
  parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,F0,F1),
  parseAnnots(More,Fields,Annots,Env,F1,Face,T,Tps,Path).
parseAnnots([(tpe(N),Lc,[Stmt])|More],Fields,Annots,Env,F,Face,T,Tps,Path) :-
  defineType(N,Lc,Stmt,Env,T,T1,Path),
  parseAnnots(More,Fields,Annots,Env,F,Face,T1,Tps,Path).
parseAnnots([_|More],Fields,Annots,Env,F,Face,T,Tps,Path) :-
  parseAnnots(More,Fields,Annots,Env,F,Face,T,Tps,Path).

parseAnnotation(Nm,_,_,_,Annots,Env,F,[(Nm,Tp)|F]) :-
  is_member((Nm,T),Annots),!,
  parseType(T,Env,Tp).
parseAnnotation(N,_,_,faceType(Fields,_),_,_,F,[(N,Tp)|F]) :-
  is_member((N,Tp),Fields),!.
parseAnnotation(N,_,_,_,_,Env,F,[(N,Tp)|F]) :-
  isVar(N,Env,vrEntry(_,_,Tp,_)),!.
parseAnnotation(N,Lc,Stmts,_,_,_,F,[(N,Tp)|F]) :-
  guessStmtType(Stmts,N,Lc,Tp).
parseAnnotation(_,_,_,_,_,_,Face,Face).

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
  formDefn(Rules,N,LclName,E,Ev,Tp,Cx,Defs,Dx).
					%  reportMsg("type of %s:%s",[N,ProgramType]).

formDefn([Eqn|Eqns],Nm,LclNm,Env,Ev,Tp,Cx,[funDef(Lc,Nm,LclNm,Tp,Cx,[Eqn|Eqns])|Dx],Dx) :-
  Eqn = equation(Lc,_,_,_),
  declareVr(Lc,Nm,Tp,Env,Ev).
formDefn([varDef(Lc,_,_,_,_,Value)],Nm,LclNm,Env,Ev,Tp,Cx,
    [varDef(Lc,Nm,LclNm,Cx,Tp,Value)|Dx],Dx) :-
  faceOfType(Tp,Env,Face),
  freshen(Face,Env,_,VFace),
  declareVr(Lc,Nm,Tp,VFace,Env,Ev).



processStmts([],_,Defs,Defs,Dflts,Dflts,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Df,Dfx,Env,Path) :-
  processStmt(St,ProgramType,Defs,D0,Df,Df0,Env,Path),!,
  processStmts(More,ProgramType,D0,Dx,Df0,Dfx,Env,Path).

processStmt(St,Tp,Defs,Defx,Df,Dfx,Env,Path) :-
  isCurriedRule(St,Lc,Hd,Cond,Body),!,
  checkEquation(Lc,Hd,Cond,Body,Tp,Defs,Defx,Df,Dfx,Env,Path).
processStmt(St,ProgramType,Defs,Defx,Df,Dfx,E,Path) :-
  isEquation(St,Lc,L,Cond,R),!,
  checkEquation(Lc,L,Cond,R,ProgramType,Defs,Defx,Df,Dfx,E,Path).
processStmt(St,Tp,[Def|Defs],Defs,Df,Df,Env,Path) :-
  isDefn(St,Lc,L,R),
  checkDefn(Lc,L,R,Tp,Def,Env,Path),!.
processStmt(St,Tp,Defs,Dfsx,Df,Df,Env,Path) :-
  isAssignment(St,Lc,L,R),!,
  checkVarDefn(Lc,L,R,Tp,Defs,Dfsx,Env,Path).
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
guessType(St,_,_,refType(GTp)) :-
  isAssignment(St,_,_,_),!,
  newTypeVar("_",GTp).
guessType(_,N,Lc,GTp) :- !,
  reportError("%s not declared",[N],Lc),
  newTypeVar("_",GTp).

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
  checkGoal(C,E0,E1,Cond,Path),
  typeOfExp(R,RT,E1,_E2,Exp,Path),
  processIterable(Env,Path,Exp,Reslt),
  Eqn = equation(Lc,Args,Cond,Reslt),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[Eqn|Dfx]; Defs=[Eqn|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("equation not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,Tp,varDef(Lc,Nm,ExtNm,[],Tp,Reslt),Env,Path) :-
  splitHead(L,Nm,none,_),
  pushScope(Env,E),
  typeOfExp(R,Tp,E,_E2,Value,Path),
  processIterable(Env,Path,Value,Reslt),
  packageVarName(Path,Nm,ExtNm).

checkVarDefn(Lc,L,R,refType(Tp),[varDef(Lc,Nm,ExtNm,[],refType(Tp),cell(Lc,Reslt))|Defs],Defs,Env,Path) :-
  splitHead(L,Nm,none,_),
  pushScope(Env,E1),
  typeOfExp(R,Tp,E1,_E2,Value,Path),
  processIterable(Env,Path,Value,Reslt),
  packageVarName(Path,Nm,ExtNm).
checkVarDefn(Lc,L,_,Tp,Defs,Defs,_,_) :-
  reportError("expecting an assignable type, not %s for %s",[Tp,L],Lc).

checkThetaBody(Tp,Lc,Els,Env,OEnv,Defs,Others,Types,Face2,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  moveConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Cx,E0,BaseEnv),
  thetaEnv(Path,nullRepo,Lc,Els,Face,BaseEnv,OEnv,Defs,Public,_Imports,Others),
  faceOfType(ETp,Env,FaceTp2),
  moveConstraints(FaceTp2,_,Face2),
  computeExport(Defs,Face2,Public,_,Types,[],[]),!.

checkRecordBody(Tp,Lc,Els,Env,OEnv,Defs,Others,Types,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,Face),
  moveConstraints(Face,Cx,CFace),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Cx,E0,BaseEnv),
  recordEnv(Path,nullRepo,Lc,Els,CFace,BaseEnv,OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,Face,Public,_,Types,[],[]),!.

checkLetExp(Tp,Lc,Th,Ex,Env,letExp(Lc,theta(Lc,ThPath,true,Defs,Others,[],faceType([],[])),Bound),Path):-
  isBraceTuple(Th,_,Els),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(ThPath,nullRepo,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public,_Imports,Others),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path).
checkLetExp(Tp,Lc,Th,Ex,Env,letExp(Lc,record(Lc,ThPath,true,Defs,Others,[],faceType([],[])),Bound),Path):-
  isQBraceTuple(Th,_,Els),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(ThPath,nullRepo,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public,_Imports,Others),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path).
checkLetExp(Tp,Lc,Th,Ex,Env,letExp(Lc,Inner,Bound),Path):-
  newTypeVar("_",ThTp),
  typeOfExp(Th,ThTp,Env,_,Inner,Path),
  faceOfType(ThTp,Env,Face),
  pushFace(Face,Lc,Env,EnvI),
  typeOfExp(Ex,Tp,EnvI,_,Bound,Path).

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

checkImplementation(Stmt,INm,[Impl,ImplDef|Dfs],Dfs,Env,Ex,_,_Path) :-
  isImplementationStmt(Stmt,Lc,Quants,Cons,Sq,IBody),
  parseContractConstraint(Quants,Cons,Sq,Env,ConNm,ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
  moveConstraints(CnSpec,AC,contractExists(Spec,IFace)),
  declareTypeVars(IQ,Lc,Env,ThEnv),
  implementationName(Spec,ImplName),
  typeOfExp(IBody,IFace,ThEnv,_ThEv,ImplTerm,ImplName),
  Impl = implDef(INm,ConNm,ImplName,ConSpec),
  (AC=[] ->
    rfold(IQ,checker:pickBoundType,IFace,ImplTp),
    ImplDef = varDef(Lc,INm,ImplName,AC,ImplTp,ImplTerm);
    moveConstraints(ITp,AC,funType(tupleType([]),IFace)),
    rfold(IQ,checker:pickBoundType,ITp,ImplTp),
    ImplDef = funDef(Lc,INm,ImplName,ImplTp,AC,
		     [equation(Lc,tple(Lc,[]),enm(Lc,"true",type("star.core*boolean")),ImplTerm)])),
  declareImplementation(ConNm,ImplName,ConSpec,Env,Ex),!.
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_) :-
  locOfAst(Stmt,Lc),
  reportError("could not check implementation statement %s",[Stmt],Lc).

pickBoundType((_,Tv),Tp,allType(Tv,Tp)).

sameLength(L1,L2,_) :- length(L1,L), length(L2,L),!.
sameLength(L1,_,Lc) :-
  length(L1,L),
  reportError("expecting %s elements",[L],Lc).

% Patterns are very similarly checked to expressions, except that fewer forms
typeOfArgPtn(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  checkType(T,tupleType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgPtn(T,Tp,Env,Ev,Exp,Path) :-
  typeOfPtn(T,Tp,Env,Ev,Exp,Path).

typeOfPtn(V,Tp,Env,Env,v(Lc,N,Tp),_Path) :-
  isIden(V,Lc,"_"),!,
  genstr("_",N).
typeOfPtn(V,Tp,Env,Ev,Term,Path) :-
  isIden(V,Lc,N),
  isVar(N,Env,Spec),!,
  (isEnumVr(Lc,Tp,Spec) ->
    typeOfVar(Lc,N,Tp,Spec,Env,Ev,Term);
    mkWhereEquality(V,TT),
    typeOfPtn(TT,Tp,Env,Ev,Term,Path)).
typeOfPtn(V,Tp,Ev,Env,v(Lc,N,Tp),_Path) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,Ev,Env).
typeOfPtn(Trm,Tp,Env,Env,intLit(Ix,IntTp),_) :-
  isLiteralInteger(Trm,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  checkType(Trm,IntTp,Tp,Env).
typeOfPtn(T,Tp,Env,Env,floatLit(Dx,FltTp),_Path) :- 
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  checkType(T,FltTp,Tp,Env).
typeOfPtn(string(Lc,Ix),Tp,Env,Env,stringLit(Ix,StrTp),_Path) :- !,
  findType("string",Lc,Env,StrTp),
  checkType(string(Lc,Ix),StrTp,Tp,Env).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,_,L,R),!,
  parseType(R,Env,RT),
  checkType(Term,RT,Tp,Env),
  typeOfPtn(L,Tp,Env,Ev,Exp,Path).
typeOfPtn(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),
  typeOfPtn(L,Tp,Env,E0,Ptn,Path),
  checkGoal(C,E0,Ex,Cond,Path).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isSquareTuple(Term,Lc,Els),
  \+isListAbstraction(Term,_,_,_), !,
  macroSquarePtn(Lc,Els,Ptn),
  typeOfPtn(Ptn,Tp,Env,Ev,Exp,Path).
typeOfPtn(Trm,Tp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfPtn(Inner,Tp,Env,Ev,Exp,Path).
typeOfPtn(Trm,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Trm,tupleType(ArgTps),Tp,Env),
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
  Exp = apply(Lc,Fun,Args,Tp).
typeOfPtn(Term,Tp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal pattern: %s, expecting a %s",[Term,Tp],Lc).

typeOfArgTerm(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  checkType(T,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgTerm(T,Tp,Env,Ev,Exp,Path) :-
  typeOfExp(T,Tp,Env,Ev,Exp,Path).

typeOfExp(V,Tp,Env,Ev,Term,_Path) :-
  isIden(V,Lc,N),
  (isVar(N,Env,Spec) ->
   typeOfVar(Lc,V,Tp,Spec,Env,Ev,Term);
   reportError("variable '%s' not defined, expecting a %s",[V,Tp],Lc),
   Term=void).
typeOfExp(T,Tp,Env,Env,intLit(Ix,IntTp),_Path) :-
  isLiteralInteger(T,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  checkType(T,IntTp,Tp,Env).
typeOfExp(T,Tp,Env,Env,floatLit(Dx,FltTp),_Path) :-
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  checkType(T,FltTp,Tp,Env).
typeOfExp(string(Lc,Ix),Tp,Env,Env,stringLit(Ix,StrTp),_Path) :- !,
  findType("string",Lc,Env,StrTp),
  checkType(string(Lc,Ix),StrTp,Tp,Env).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,_,L,R),!,
  parseType(R,Env,RT),
  checkType(Term,RT,Tp,Env),
  typeOfExp(L,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  binary(Lc,":",LT,R,NT),
  typeOfExp(NT,Tp,Env,Ev,Exp,Path).
typeOfExp(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),
  typeOfExp(L,Tp,Env,E0,Ptn,Path),
  checkGoal(C,E0,Ex,Cond,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  recordAccessExp(Lc,Rc,Fld,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,cond(Lc,Test,Then,Else,Tp),Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  checkGoal(Tst,Env,E0,Test,Path),
  typeOfExp(Th,Tp,E0,E1,Then,Path),
  typeOfExp(El,Tp,Env,E2,Else,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isActionTerm(Term,Lc,Stmts),!,
  findType("action",Lc,Env,ActionTp),
  newTypeVar("E",ErTp),
  newTypeVar("X",ElTp),
  mkTypeExp(ActionTp,[ErTp,ElTp],MTp),
  checkType(Term,MTp,Tp,Env),
  checkDo(Lc,Stmts,Env,Ev,Tp,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isTaskTerm(Term,Lc,Stmts),!,
  findType("task",Lc,Env,TaskTp), % action type is just the core
  newTypeVar("E",ErTp),
  newTypeVar("X",ElTp),
  checkType(Term,tpExp(tpExp(TaskTp,ErTp),ElTp),Tp,Env),
  checkDo(Lc,Stmts,Env,Ev,Tp,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isDoTerm(Term,Lc,Stmts),!,
  checkDo(Lc,Stmts,Env,Ev,Tp,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSquareTuple(Term,Lc,Els),
  \+isListAbstraction(Term,_,_,_), !,
  squareTupleExp(Lc,Els,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,theta(Lc,ThPath,true,Defs,Others,Types,Tp),Path) :-
  isBraceTuple(Term,Lc,Els),
  \+isAbstraction(Term,_,_,_),
  genNewName(Path,"θ",ThPath),
  checkThetaBody(Tp,Lc,Els,Env,_,Defs,Others,Types,Face,ThPath),
  checkType(Term,Face,Tp,Env).
typeOfExp(Term,Tp,Env,Env,record(Lc,ThPath,true,Defs,Others,Types,Tp),Path) :-
  isQBraceTuple(Term,Lc,Els),
  genNewName(Path,"ρ",ThPath),
  checkRecordBody(Tp,Lc,Els,Env,_,Defs,Others,Types,ThPath),
  compExport(Defs,[],[],Fields,Tps,_,_,misc:bin_nop),
  checkType(Term,faceType(Fields,Tps),Tp,Env).
typeOfExp(Term,Tp,Env,Env,theta(Lc,ThPath,false,Defs,Others,Types,Tp),Path) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfKnown(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  thetaName(Path,Lbl,ThPath),
  deRef(FnTp,BTp),
  checkThetaBody(BTp,Lc,Els,E0,_,Defs,Others,Types,_,ThPath).
typeOfExp(Term,Tp,Env,Env,record(Lc,ThPath,false,Defs,Others,Types,Tp),Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("R",FnTp),
  typeOfKnown(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  thetaName(Path,Lbl,ThPath),
  checkRecordBody(FnTp,Lc,Els,E0,_,Defs,Others,Types,ThPath).
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
  checkType(Trm,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isUnary(Term,Lc,"-",Arg), % handle unary minus
  unary(Lc,"__minus",Arg,Sub),
  typeOfExp(Sub,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSearch(Term,Lc,L,R),!,
  typeOfSearch(Lc,L,R,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isAbstraction(Term,Lc,B,G),!,
  checkAbstraction(Term,Lc,B,G,Tp,Env,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isListAbstraction(Term,Lc,B,G),!,
  findType("list",Lc,Env,LTp),
  newTypeVar("_El",ElTp),
  checkType(Term,tpExp(LTp,ElTp),Tp,Env),
  checkAbstraction(Term,Lc,B,G,Tp,Env,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isValof(Term,Lc,Ex),!,
  unary(Lc,"_perform",Ex,VV),
  typeOfExp(VV,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isIndexTerm(Term,Lc,F,A),!,
  typeOfIndex(Lc,F,A,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  (hasPromotion(Term) ->
    promoteOption(Term,NT),
    typeOfExp(NT,Tp,Env,Ev,Exp,Path);
    typeOfRoundTerm(Lc,F,A,Tp,Env,Ev,Exp,Path)).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isOfTerm(Term,Lc,Lbl,Tpl),!,
  macroOfTerm(Lbl,Lc,Tpl,Trm),
  typeOfExp(Trm,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Lam,Path) :-
  isEquation(Term,_Lc,_H,_C,_R),
  typeOfLambda(Term,Tp,Env,Lam,Path).
typeOfExp(Term,Tp,Env,Ex,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Term,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,disj(Lc,Lhs,Rhs),Path) :-
  isDisjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Term,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,Env,E2,Rhs,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,Env,Env,implies(Lc,Lhs,Rhs),Path) :-
  isForall(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Term,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,_Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Env,neg(Lc,Rhs),Path) :-
  isNegation(Term,Lc,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Term,LogicalTp,Tp,Env),
  typeOfExp(R,LogicalTp,Env,_Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Term,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,Env,E0,Lhs,Path),
  typeOfExp(E,TV,E0,Ev,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isOptionMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  checkType(Term,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  unary(Lc,"some",P,SP),
  typeOfPtn(SP,TV,Env,E0,Lhs,Path),
  typeOfExp(E,TV,E0,Ev,Rhs,Path).

typeOfExp(Term,Tp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

funLbl(over(_,T,_,_),L) :- funLbl(T,L).
funLbl(v(_,L,_),L).
funLbl(cons(_,Nm,_),Nm).
funLbl(enm(_,Nm,_),Nm).
funLbl(mtd(_,Nm,_),Nm).

typeOfRoundTerm(Lc,F,A,Tp,Env,Ev,apply(Lc,Fun,Args,Tp),Path) :-
  newTypeVar("F",FnTp),
  genTpVars(A,Vrs),
  At = tupleType(Vrs),
  typeOfKnown(F,FnTp,Env,E0,Fun,Path),
  simplifyType(FnTp,Env,_,[],FTp),
  evidence(At,E0,_,AT),
  (sameType(funType(At,Tp),FTp,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args,Path);
   sameType(consType(At,Tp),FTp,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args,Path);
   reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FTp,At,Tp],Lc),
   Args = tple(Lc,[]),
   Env=Ev).

typeOfSearch(Lc,L,R,Tp,Env,Ev,search(Lc,Ptn,Src,Iterator),Path) :-
  findType("boolean",Lc,Env,LogicalTp),
  checkType(L,LogicalTp,Tp,Env),
  newTypeFun("_m",1,MTp),
  newTypeVar("_x",StTp),
  newTypeVar("_Sr",SrTp),
  newTypeVar("_El",ElTp),
  MdTp = tpExp(MTp,StTp),
  ActFnTp = funType(tupleType([ElTp,StTp]),MdTp),
  ItrFnTp = funType(tupleType([SrTp,MdTp,ActFnTp]),MdTp),
  typeOfKnown(name(Lc,"_iter"),ItrFnTp,Env,E0,Iterator,Path),
  typeOfPtn(L,ElTp,E0,E1,Ptn,Path),
  typeOfExp(R,SrTp,E1,Ev,Src,Path).
%  reportMsg("search %s:%s",[search(Lc,Ptn,Src,Iterator),Tp]).

typeOfLambda(Term,Tp,Env,lambda(Lc,equation(Lc,Args,Cond,Exp),Tp),Path) :-
  isEquation(Term,Lc,H,C,R),
  newTypeVar("_A",AT),
  typeOfArgPtn(H,AT,Env,E1,Args,Path),
  newTypeVar("_E",RT),
  checkType(Term,funType(AT,RT),Tp,Env),
  checkGoal(C,E1,E2,Cond,Path),
  typeOfExp(R,RT,E2,_,Exp,Path).

typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  isBinary(Arg,_,"->",Ky,Vl),!,
  ternary(Lc,"_put",Mp,Ky,Vl,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  isUnary(Arg,_,"\\+",Ky),!,
  binary(Lc,"_remove",Mp,Ky,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  binary(Lc,"_index",Mp,Arg,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).

pickupContract(Lc,Env,Nm,StTp,DpTps,Op) :-
  (getContract(Nm,Env,conDef(_,_,Con)) ->
   freshen(Con,Env,_,contractExists(conTract(Op,[StTp],DpTps),_));
   reportError("%s contract not defined",[Nm],Lc),
   newTypeVar("_St",StTp),
   DpTps=[]).

pickupIxContract(Lc,Env,Nm,StTp,DpTps,Op) :-
  (getContract(Nm,Env,conDef(_,_,Con)) ->
   freshen(Con,Env,_,contractExists(conTract(Op,[StTp],DpTps),_));
   reportError("%s contract not defined",[Nm],Lc),
   newTypeVar("_St",StTp),
   DpTps=[]).

checkGoal(G,Env,Ev,Goal,Path) :-
  locOfAst(G,Lc),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,Env,Ev,Cond,Path),
  (isIterableGoal(Cond) ->
     genIterableGl(Cond,Path,Goal);
%   reportMsg("iterable goal: %s",[Goal]);
   Cond=Goal).



checkAbstraction(Term,Lc,B,G,Tp,Env,Abstr,Path) :-
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,Env,E1,Cond,Path),
  pickupContract(Lc,Env,"sequence",StTp,[ElTp],Op),
  checkType(Term,Tp,StTp,Env),
  typeOfExp(B,ElTp,E1,_,Bnd,Path),
  pickupContract(Lc,Env,"execution",ExTp,_,Contract),
  findType("action",Lc,Env,ActionTp),
  newTypeVar("_Er",ErTp),
  checkType(Term,ActionTp,ExTp,Env),
  genReturn(Lc,over(Lc,mtd(Lc,"_nil",StTp),
		    true,[conTract(Op,[StTp],[ElTp])]),
	    ExTp,StTp,ErTp,Contract,Zed),
  Gen = over(Lc,mtd(Lc,"_cons",
		    funType(tupleType([ElTp,StTp]),StTp)),
	     true,[conTract(Op,[StTp],[ElTp])]),
%  reportMsg("Result type: %s, Gen:%s",[Tp,Gen]),
%  reportMsg("Action type: %s, monad: %s",[ExTp,Contract]),
  genCondition(Cond,Path,checker:genRtn(Lc,ExTp,StTp,ErTp,Contract),
	       checker:genSeq(Lc,Contract,ExTp,ErTp),
	       checker:genEl(Lc,Gen,Bnd,StTp,Contract,ExTp,ErTp),
	       lifted(Zed),ACond),
  genPerform(Lc,ACond,Tp,ExTp,Contract,Abstr).
%  reportMsg("abstraction %s ->\n%s",[Term,Abstr]).

genEl(Lc,Gen,Bnd,StTp,Contract,ExTp,ErTp,unlifted(St),Exp) :-
  Next  = apply(Lc,Gen,tple(Lc,[Bnd,St]),StTp),
  genReturn(Lc,Next,ExTp,ErTp,Contract,Exp).

genPut(Lc,Gen,Key,Value,StTp,Contract,ExTp,ErTp,unlifted(St),Exp) :-
  Next  = apply(Lc,Gen,tple(Lc,[St,Key,Value]),StTp),
  genReturn(Lc,Next,ExTp,ErTp,Contract,Exp).

genSeq(Lc,Contract,ExStTp,ErTp,St,Init,Reslt,Exp) :-
  typeOfCanon(St,ATp),
  MdTp = tpExp(ExStTp,ATp),
  LTp = funType(tupleType([ATp]),MdTp),
  Lam = lambda(Lc,equation(Lc,tple(Lc,[St]),
			   enm(Lc,"true",type("star.core*boolean")),Reslt),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([MdTp,LTp]),MdTp)),
	     true,[conTract(Contract,[ExStTp],[ErTp])]),
  genRtn(Lc,ExStTp,ErTp,Contract,Init,Initial),
  Exp = apply(Lc,Gen,tple(Lc,[Initial,Lam]),MdTp).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

checkDo(Lc,B,Env,Ev,Tp,EE,Path) :-
%  reportMsg("do type %s",[Tp]),
  newTypeVar("_x",ValTp),
  newTypeVar("_er",ErTp),
  (getContract("execution",Env,conDef(_,_,Con)) ->
   freshen(Con,Env,_,contractExists(conTract(Contract,[ExTp],[]),_)),
   mkTypeExp(ExTp,[ErTp,ValTp],MTp),
   checkType(B,Tp,MTp,Env);
%   reportMsg("execution type %s",[MTp]);
   reportError("execution contract not defined",[],Lc),
   newTypeVar("_t",ExTp)),
  checkAction(B,Env,Ev,Contract,ExTp,ValTp,ErTp,Body,Path),
%  reportMsg("Basic action %s",[doTerm(Lc,Body,ExTp,ValTp,ErTp)]),
  genAction(delayDo(Lc,Body,ExTp,ValTp,ErTp),Contract,noDo(Lc),EE,Path).
%  reportMsg("Action-> %s",[EE]).

checkAction(Term,Env,Env,_,_,_,_,noDo(Lc),_) :-
  isIden(Term,Lc,"nothing"),!.
checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,seqDo(Lc,A1,A2),Path) :-
  isActionSeq(Term,Lc,S1,S2),!,
  newTypeVar("_e",FV),
  checkAction(S1,Env,E1,Contract,ExTp,FV,ErTp,A1,Path),
  checkAction(S2,E1,Ev,Contract,ExTp,ValTp,ErTp,A2,Path).
checkAction(Term,Env,Ev,_Contract,ExTp,_,ErTp,bindDo(Lc,Ptn,Gen,ExTp),Path) :-
  isBind(Term,Lc,P,Ex),!,
  newTypeVar("_P",PT),
  mkTypeExp(ExTp,[ErTp,PT],HType),  % in a bind, the type of the value must be in the same monad
  typeOfExp(Ex,HType,Env,E1,Gen,Path),
  typeOfPtn(P,PT,E1,Ev,Ptn,Path).
checkAction(Term,Env,Ev,_,_,_,_,varDo(Lc,Ptn,Exp),Path) :-
  isDefn(Term,Lc,P,Ex),!,
  newTypeVar("_P",PT),
  typeOfPtn(P,PT,Env,Ev,Ptn,Path),
  typeOfExp(Ex,PT,Env,_,Exp,Path).
checkAction(Term,Env,Ev,_,_ExTp,_ValTp,_ErTp,varDo(Lc,Lhs,cell(Lc,Rhs)),Path) :-
  isAssignment(Term,Lc,L,R),
  isIden(L,_,Vr),
  \+isVar(Vr,Env,_),!,
  newTypeVar("_V",PT),
  typeOfPtn(L,refType(PT),Env,Ev,Lhs,Path),
  typeOfExp(R,PT,Env,_,Rhs,Path).
checkAction(Term,Env,Ev,_,ExTp,ValTp,ErTp,Act,Path) :-
  isAssignment(Term,Lc,L,R),!,
  checkAssignment(Lc,L,R,Env,Ev,ExTp,ValTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,
	    ifThenDo(Lc,Ts,Th,El,ExTp,ValTp,ErTp),Path) :-
  isIfThenElse(Term,Lc,T,H,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(T,LogicalTp,Env,Et,Ts,Path),
  checkAction(H,Et,E1,Contract,ExTp,ValTp,ErTp,Th,Path),
  checkAction(E,Env,E2,Contract,ExTp,ValTp,ErTp,El,Path),
  mergeDict(E1,E2,Env,Ev).
checkAction(Term,Env,Env,Contract,ExTp,ValTp,ErTp,
	    ifThenDo(Lc,Ts,Th,noDo(Lc),ExTp,ValTp,ErTp),Path) :-
  isIfThen(Term,Lc,T,H),!,
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(T,LogicalTp,Env,Et,Ts,Path),
  checkAction(H,Et,_E1,Contract,ExTp,ValTp,ErTp,Th,Path).
checkAction(Term,Env,Env,Contract,ExTp,_,ErTp,whileDo(Lc,Ts,Bdy,ExTp,ErTp),Path) :-
  isWhileDo(Term,Lc,T,B),!,
  findType("boolean",Lc,Env,LogicalTp),
  pushScope(Env,WEnv),
  typeOfExp(T,LogicalTp,WEnv,Et,Ts,Path),
  checkAction(B,Et,_,Contract,ExTp,tupleType([]),ErTp,Bdy,Path).
checkAction(Term,Env,Env,Contract,ExTp,_,ErTp,forDo(Lc,Ts,Bdy,ExTp,ErTp),Path) :-
  isForDo(Term,Lc,T,B),!,
  findType("boolean",Lc,Env,LogicalTp),
  pushScope(Env,FEnv),
  typeOfExp(T,LogicalTp,FEnv,Et,Ts,Path),
  checkAction(B,Et,_,Contract,ExTp,tupleType([]),ErTp,Bdy,Path).
checkAction(Term,Env,Env,Contract,ExTp,ValTp,ErTp,
	    tryCatchDo(Lc,Bdy,Hndlr,ExTp,ValTp,ErTp),Path) :-
  isTryCatch(Term,Lc,B,H),!,
  anonVar(Lc,Anon,BdErTp),
  checkAction(B,Env,_,Contract,ExTp,ValTp,BdErTp,Bdy,Path),
  checkCatch(H,Env,Contract,ExTp,ValTp,ErTp,BdErTp,Anon,Hndlr,Path).
checkAction(Term,Env,Env,_,ExTp,ValTp,ErTp,
	    throwDo(Lc,Exp,ExTp,ValTp,ErTp),Path) :-
  isThrow(Term,Lc,E),!,
  typeOfExp(E,ErTp,Env,_,Exp,Path).
checkAction(Term,Env,Env,_Contract,ExTp,ValTp,ErTp,
	    returnDo(Lc,Reslt,ExTp,ValTp,ErTp),Path) :-
  isReturn(Term,Lc,Ex),!,
  typeOfExp(Ex,ValTp,Env,_,Exp,Path),
  processIterable(Env,Path,Exp,Reslt).
checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,Exp,Path) :-
  isBraceTuple(Term,_,[Stmt]),!,
  checkAction(Stmt,Env,Ev,Contract,ExTp,ValTp,ErTp,Exp,Path).
checkAction(Term,Env,Ev,_,ExTp,ValTp,ErTp,simpleDo(Lc,Exp,ExTp),Path) :-
  locOfAst(Term,Lc),
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  typeOfExp(Term,MTp,Env,Ev,Exp,Path).

checkAssignment(Lc,L,R,Env,Ev,ExTp,ValTp,ErTp,simpleDo(Lc,Exp,ExTp),Path) :-
  (isIndexTerm(L,LLc,C,I) ->
     unary(LLc,"!",C,CC),
    ternary(LLc,"_put",CC,I,R,Repl),
    binary(Lc,":=",C,Repl,Term);
   binary(Lc,":=",L,R,Term)),
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  typeOfExp(Term,MTp,Env,Ev,Exp,Path).

checkCatch(Term,Env,Contract,ExTp,ValTp,ErTp,BdErTp,Anon,Hndlr,Path) :-
  isBraceTuple(Term,Lc,[St]),!,
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  Htype = funType(tupleType([BdErTp]),MTp),
  checkAction(St,Env,_,Contract,ExTp,ValTp,ErTp,H,Path),
  genAction(H,Contract,noDo(Lc),HH,Path),
  Hndlr = lambda(Lc,equation(Lc,tple(Lc,[Anon]),
			     enm(Lc,"true",type("star.core*boolean")),
			     HH),Htype).
checkCatch(Term,Env,_Contract,ExTp,ValTp,ErTp,BdErTp,_,Hndlr,Path) :-
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  Htype = funType(tupleType([BdErTp]),MTp),
  typeOfExp(Term,Htype,Env,_,Hndlr,Path).

recordFace(Trm,Env,Env,v(Lc,Nm,Face),Face,_) :-
  isIden(Trm,Lc,Nm),
  isVar(Nm,Env,vrEntry(_,_,_,Fce)),!,
  call(Fce,Env,Face).
recordFace(Trm,Env,Ev,Rec,Face,Path) :-
  newTypeVar("_R",AT),
  typeOfKnown(Trm,AT,Env,Ev,Rec,Path),
  faceOfType(AT,Env,Fc),
  freshen(Fc,Env,_,Face).

recordAccessExp(Lc,Rc,Fld,ET,Env,Ev,dot(Lc,Rec,Fld,Tp),Path) :-
  recordFace(Rc,Env,Ev,Rec,Fce,Path),
  deRef(Fce,Face),
  fieldInFace(Face,Fld,Lc,FTp),!,
  freshen(FTp,Env,_,Tp),
  checkType(Fld,Tp,ET,Env).

macroMapEntries(Lc,[],name(Lc,"_empty")).
macroMapEntries(_,[E|L],T) :-
  isBinary(E,Lc,"->",Ky,Vl),!,
  macroMapEntries(Lc,L,Tr),
  roundTerm(Lc,name(Lc,"_put"),[Tr,Ky,Vl],T).
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
typeOfKnown(T,Tp,Env,Env,v(Lc,Nm,Tp),_) :-
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
  macroListEntries(Lc,Els,Trm,nilGen,consGen,appndGen),
  typeOfExp(Trm,Tp,Env,Ev,Exp,Path).

/* Process any remaining iterable conditions */

processIterable(Env,Path,Cond,Goal) :-
  isIterableGoal(Cond),!,
  locOfCanon(Cond,Lc),
  pickupContract(Lc,Env,"execution",_,_,Contract),
  genIterableGl(Cond,Contract,Path,Goal),
  reportMsg("iterable exp -> %s",[Goal]).
processIterable(Env,Path,apply(Lc,Op,Arg,Tp),apply(Lc,NOp,NArg,Tp)) :-!,
  processIterable(Env,Path,Op,NOp),
  processIterable(Env,Path,Arg,NArg).
processIterable(Env,Path,dot(Lc,Rc,Fld,Tp),dot(Lc,NRc,Fld,Tp)) :-!,
  processIterable(Env,Path,Rc,NRc).
processIterable(Env,Path,tple(Lc,Els),tple(Lc,NEls)) :-!,
  map(Els,checker:processIterable(Env,Path),NEls).
processIterable(Env,Path,where(Lc,E,C),where(Lc,NE,NC)) :-!,
  processIterable(Env,Path,E,NE),
  processIterable(Env,Path,C,NC).
processIterable(Env,Path,conj(Lc,L,R),conj(Lc,NL,NR)) :-!,
  processIterable(Env,Path,L,NL),
  processIterable(Env,Path,R,NR).
processIterable(Env,Path,disj(Lc,L,R),disj(Lc,NL,NR)) :-!,
  processIterable(Env,Path,L,NL),
  processIterable(Env,Path,R,NR).
processIterable(Env,Path,implies(Lc,L,R),implies(Lc,NL,NR)) :-!,
  processIterable(Env,Path,L,NL),
  processIterable(Env,Path,R,NR).
processIterable(Env,Path,cond(Lc,T,L,R),cond(Lc,NT,NL,NR)) :-!,
  processIterable(Env,Path,T,NT),
  processIterable(Env,Path,L,NL),
  processIterable(Env,R,NR).
processIterable(Env,Path,neg(Lc,L),neg(Lc,NL)) :-!,
  processIterable(Env,Path,L,NL).
processIterable(Env,Path,match(Lc,L,R),match(Lc,NL,NR)) :-!,
  processIterable(Env,Path,L,NL),
  processIterable(Env,Path,R,NR).
processIterable(Env,Path,assign(Lc,L,R),assign(Lc,NL,NR)) :-!,
  processIterable(Env,Path,L,NL),
  processIterable(Env,Path,R,NR).
processIterable(Env,Path,cell(Lc,L),cell(Lc,NL)) :-!,
  processIterable(Env,Path,L,NL).
processIterable(_,_,T,T).

nilGen(Lc,name(Lc,"_nil")).

consGen(Lc,L,R,Trm) :-
  binary(Lc,"_cons",L,R,Trm).

appndGen(Lc,L,R,Trm) :-
  binary(Lc,"_apnd",L,R,Trm).

isMapSequence([E|_]) :-
  isBinary(E,_,"->",_,_).

isMapType(Tp,Env) :-
  isType("map",Env,tpDef(_,MpTp,_)),!,
  deRef(Tp,tpExp(TF1,_)),
  deRef(TF1,tpExp(MpTp,_)).

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
checkType(Ast,S,T,_) :-
  locOfAst(Ast,Lc),
  reportError("%s:%s not consistent with expected type\n%s",[Ast,S,T],Lc).

computeExport(Defs,Fields,Public,Exports,Types,Cons,Impls) :-
  compExport(Defs,Fields,Public,Exports,Types,Cons,Impls,misc:is_member).

compExport([],_,_,[],[],[],[],_).
compExport([Def|Defs],Fields,Public,Exports,Types,Cons,Impls,Check) :-
  exportDef(Def,Fields,Public,Exports,Ex,Types,Tx,Cons,Cx,Impls,Ix,Check),!,
  compExport(Defs,Fields,Public,Ex,Tx,Cx,Ix,Check).

exportDef(funDef(_,Nm,_,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl,Check) :-
  isPublicVar(Nm,Fields,Public,Check).
exportDef(typeDef(_,Nm,_,FRule),_,Public,Ex,Ex,[(Nm,FRule)|Tx],Tx,Cx,Cx,Impl,Impl,Check) :-
  isPublicType(Nm,Public,Check).
exportDef(varDef(_,Nm,_,_,Tp,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl,Check) :-
  isPublicVar(Nm,Fields,Public,Check).
exportDef(cnsDef(_,Nm,_,Tp),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl,Check) :-
  isPublicVar(Nm,Fields,Public,Check).
exportDef(Con,_,Public,Ex,Ex,Types,Types,[Con|Cx],Cx,Impl,Impl,Check) :-
  isPublicContract(Con,Public,Check).
exportDef(implDef(TmpNm,ConNm,ImplName,Spec),_,Public,Ex,Ex,Tps,Tps,Cx,Cx,
	  [imp(ConNm,ImplName,Spec)|Ix],Ix,Check) :-
  isPublicImpl(TmpNm,Public,Check),!.
exportDef(_,_,_,Ex,Ex,Tps,Tps,Cx,Cx,Impls,Impls,_).

isPublicVar(Nm,_,Public,Check) :-
  call(Check,var(Nm),Public),!.
isPublicVar(Nm,Fields,_,Check) :-
  call(Check,(Nm,_),Fields),!.

isPublicType(Nm,Public,Check) :-
  call(Check,tpe(Nm),Public),!.

isPublicContract(conDef(Nm,_,_),Public,Check) :-
  call(Check,con(Nm),Public),!.

isPublicImpl(Nm,Public,Check) :-
  call(Check,imp(Nm),Public),!.

mergeDict(D1,D2,Env,D3) :-
  length(D1,L1),
  length(D2,L1),!,
  mergeScopes(D1,D2,Env,D3).

mergeScopes([scope(Ts,Ns1,Cns,Impls,Cons)|O],
    [scope(Ts,Ns2,Cns,Impls,Cons)|O],Env,
    [scope(Ts,N3,Cns,Impls,Cons)|O]) :-
  dict_pairs(Ns1,_,N1),
  dict_pairs(Ns2,T,N2),
  mergeVDefs(N1,N2,Env,Ns),
  dict_pairs(N3,T,Ns).

mergeVDefs([],_,_,[]).
mergeVDefs(_,[],_,[]).
mergeVDefs([Vr-T1|D1],D2,Env,[Vr-T1|D3]) :-
  is_member(Vr-T2,D2),
  sameDesc(T1,T2,Env),
  mergeVDefs(D1,D2,Env,D3).
mergeVDefs([_|D1],D2,Env,D3) :-
  mergeVDefs(D1,D2,Env,D3).

sameDesc(vrEntry(_,C1,Tp1,_),vrEntry(_,C1,Tp2,_),Env) :-
  sameType(Tp1,Tp2,Env).

