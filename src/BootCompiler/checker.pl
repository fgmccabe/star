:- module(checker,[checkProgram/4]).

:- use_module(abstract).
:- use_module(wff).
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

checkProgram(Prog,Vers,Repo,
    prog(pkg(Pkg,Vers),Imports,ODefs,OOthers,Exports,Types,Cons,Impls)) :-
  stdDict(Base),
  isBraceTerm(Prog,Lc,Pk,Els),
  packageName(Pk,Pkg),
  pushScope(Base,Env),
  thetaEnv(Pkg,Repo,Lc,Els,faceType([],[]),Env,OEnv,Defs,Public,Imports,Others),
  findImportedImplementations(Imports,[],OverDict),
  overload(Defs,OverDict,ODict,ODefs),
  overloadOthers(Others,ODict,OOthers),
  computeExport(ODefs,faceType([],[]),Public,Exports,Types,Cons,Impls),
  dischargeConstraints(Base,OEnv),!.

thetaEnv(Pkg,Repo,Lc,Els,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  macroRewrite(Els,Stmts),
  collectDefinitions(Stmts,Dfs,Public,Annots,Imps,Otrs),
  dependencies(Dfs,Groups,Annots),
  processImportGroup(Imps,Imports,Repo,Base,IBase),
  pushFace(Fields,Lc,IBase,Env),
  checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Pkg),
  checkOthers(Otrs,Others,TheEnv,Pkg),
  dispDefs(Defs).

recordEnv(Path,Repo,_Lc,Els,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  macroRewrite(Els,Stmts),
  collectDefinitions(Stmts,Dfs,Public,Annots,Imps,Otrs),
  processImportGroup(Imps,Imports,Repo,Base,TmpEnv),
  parseAnnotations(Dfs,Fields,Annots,TmpEnv,Path,Face),
  checkGroup(Dfs,Defs,[],TmpEnv,TheEnv,Face,Path),
  checkOthers(Otrs,Others,TheEnv,Path),
  dispDefs(Defs).

processImportGroup(Stmts,ImportSpecs,Repo,Env,Ex) :-
  findAllImports(Stmts,Lc,Imports),
  importAll(Imports,Repo,AllImports),
  importAllDefs(AllImports,Lc,ImportSpecs,Repo,Env,Ex).

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

importDefs(spec(_,faceType(Exported,Types),_,Cons,_,_),Lc,Env,Ex) :-
  declareFields(Exported,Lc,Env,E0),
  importTypes(Types,Lc,E0,E1),
  importContracts(Cons,Lc,E1,Ex).

declareFields([],_,Env,Env).
declareFields([(Nm,Tp)|More],Lc,Env,Ex) :-
  declareVr(Lc,Nm,Tp,Env,E0),
  declareFields(More,Lc,E0,Ex).

importTypes([],_,Env,Env).
importTypes([(Nm,Rule)|More],Lc,Env,Ex) :-
  pickTypeTemplate(Rule,Type),
  declareType(Nm,tpDef(Lc,Type,Rule),Env,E0),
  importTypes(More,Lc,E0,Ex).

pickTypeTemplate(allType(_,Tp),XTp) :-
  pickTypeTemplate(Tp,XTp).
pickTypeTemplate(typeExists(Lhs,_),Tmp) :-
  pickTypeTemplate(Lhs,Tmp).
pickTypeTemplate(typeFun(Lhs,_),Tmp) :-
  pickTypeTemplate(Lhs,Tmp).
pickTypeTemplate(constrained(Tp,_),Tmp) :-
  pickTypeTemplate(Tp,Tmp).
pickTypeTemplate(type(Nm),type(Nm)).
pickTypeTemplate(typeExp(Op,_),Op).

importAllDefs([],_,[],_,Env,Env).
importAllDefs([import(Viz,Pkg)|More],Lc,
      [import(Viz,Pkg,Exported,Types,Classes,Cons,Impls)|Specs],Repo,Env,Ex) :-
  importPkg(Pkg,Repo,Spec),
  Spec = spec(_,Exported,Types,Classes,Cons,Impls,_),
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
  importPkg(Pkg,Repo,spec(_,_,_,_,_,_,Imports)),
  addPublicImports(Imports,Inp,More).
importMore(_,import(_,Pkg),SoFar,SoFar,Inp,Inp) :-
  reportError("could not import package %s",[Pkg]).

addPublicImports([],Imp,Imp).
addPublicImports([import(public,Pkg)|I],Rest,[import(public,Pkg)|Out]) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(private,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).

findImportedImplementations([import(_,_,_,_,_,_,Impls)|Specs],D,OverDict) :-
  rfold(Impls,checker:declImpl,D,D1),
  findImportedImplementations(Specs,D1,OverDict).
findImportedImplementations([],D,D).

checkOthers([],[],_,_).
checkOthers([St|Stmts],Ass,Env,Path) :-
  checkOther(St,Ass,More,Env,Path),!,
  checkOthers(Stmts,More,Env,Path).
checkOthers([St|Stmts],Ass,Env,Path) :-
  locOfAst(St,Lc),
  reportError("cannot understand statement: %s",[St],[Lc]),
  checkOthers(Stmts,Ass,Env,Path).

checkOther(St,[assertion(Lc,Cond)|More],More,Env,_) :-
  isIntegrity(St,Lc,C),!,
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(C,LogicalTp,Env,_,Cond).
checkOther(St,[show(Lc,Show)|More],More,Env,_) :-
  isShow(St,Lc,E),!,
  unary(Lc,"disp",E,Ex),
  unary(Lc,"formatSS",Ex,FC), % create the call formatSS(disp(E))
  findType("string",Lc,Env,StringTp),
  typeOfTerm(FC,StringTp,Env,_,Show).
checkOther(St,[ignore(Lc,Exp)|More],More,Env,_) :-
  isIgnore(St,Lc,Trm),!,
  newTypeVar("_",V),
  typeOfTerm(Trm,V,Env,_,Exp).

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
  declareVr(Lc,Nm,MTp,Env,E0),
  formMethods(M,Lc,Q,Cx,Con,E0,Ev).

parseTypeDef(St,[typeDef(Lc,Nm,Type,FaceRule)|Dx],Dx,E,Ev,Path) :-
  isTypeExistsStmt(St,Lc,Quants,Ct,Hd,Body),
  parseBoundTpVars(Quants,[],Q),
  parseTypeHead(Hd,Q,Tp,Nm,Path),
  parseConstraints(Ct,E,Q,[],C0),
  parseType(Body,E,Q,C0,Cx,RTp),
  wrapConstraints(Cx,typeExists(Tp,RTp),Rl),
  reQuant(Q,Rl,FaceRule),
  reQuant(Q,Tp,Type),
  declareType(Nm,tpDef(Lc,Type,FaceRule),E,Ev).
parseTypeDef(St,[typeDef(Lc,Nm,Type,FaceRule)|Dx],Dx,E,Ev,Path) :-
  isTypeFunStmt(St,Lc,Quants,Ct,Hd,Bd),
  parseBoundTpVars(Quants,[],Q),
  parseConstraints(Ct,E,Q,[],C0),
  parseTypeHead(Hd,Q,Tp,Nm,Path),
  parseType(Bd,E,Q,C0,Cx,RpTp),
  wrapConstraints(Cx,typeLambda(Tp,RpTp),Rl),
  reQuant(Q,Rl,FaceRule),
  reQuant(Q,Tp,Type),
  declareType(Nm,tpDef(Lc,Type,FaceRule),E,Ev).

parseConstructor(Nm,Lc,T,Env,Ev,[conDef(Lc,Nm,ConVr,Tp)|Defs],Defs,Path) :-
  parseType(T,Env,Tp),
  marker(value,Marker),
  subPath(Path,Marker,Nm,CnNm),
  (isConType(Tp) ->
    declareCns(Lc,Nm,CnNm,Tp,Env,Ev), ConVr=cns(Lc,CnNm) ;
    declareEnum(Lc,Nm,CnNm,Tp,Env,Ev), ConVr=enm(Lc,CnNm)).

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
parseAnnotation(N,Lc,_,_,_,Face,Face) :-
  reportError("no type annotation for variable %s",[N],Lc).

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

checkVarRules(N,Lc,Stmts,Env,Ev,Defs,Dx,Face,Path) :-
  pickupVarType(N,Lc,Face,Tp),
  evidence(Tp,Env,Q,PT),
  declareTypeVars(Q,Lc,Env,SEnv),
  moveConstraints(PT,Cx,ProgramType),
  declareConstraints(Cx,SEnv,StmtEnv),
  processStmts(Stmts,ProgramType,Rules,[],StmtEnv,Path),
  collectPrograms(Rules,Env,Ev,Tp,Cx,Defs,Dx).

processStmts([],_,Defs,Defs,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Env,Path) :-
  processStmt(St,ProgramType,Defs,D0,Env,Path),!,
  processStmts(More,ProgramType,D0,Dx,Env,Path).

processStmt(St,ProgramType,Defs,Defx,E,_) :-
  isEquation(St,Lc,L,Cond,R),!,
  checkEquation(Lc,L,Cond,R,ProgramType,Defs,Defx,E).
processStmt(St,Tp,[Def|Defs],Defs,Env,_) :-
  isDefn(St,Lc,L,Cond,R),!,
  checkDefn(Lc,L,Cond,R,Tp,Def,Env).
processStmt(St,Tp,[Def|Defs],Defs,Env,_) :-
  isAssignment(St,Lc,L,Cond,R),!,
  checkVarDefn(Lc,L,Cond,R,Tp,Def,Env).
processStmt(St,Tp,Defs,Defx,E,_) :-
  isPtnRule(St,Lc,L,C,R),
  checkPtnRule(Lc,L,C,R,Tp,Defs,Defx,E).
processStmt(St,Tp,Defs,Dx,E,_) :-
  isGrammarRule(St,Lc,L,C,R),
  checkGrammarRule(Lc,L,C,R,Tp,Defs,Dx,E).
processStmt(St,Tp,Defs,Defs,_,_) :-
  locOfAst(St,Lc),
  reportError("Statement %s not consistent with expected type %s",[St,Tp],Lc).

pickupVarType(N,_,faceType(F,_),Tp) :-
  is_member((N,Tp),F),!.
pickupVarType(N,Lc,_,anonType) :- reportError("%s not declared",[N],Lc).

pickupThisType(Env,Tp) :-
  isVar("this",Env,vr(_,_,Tp)),!.
pickupThisType(_,voidType).

declareConstraints([],Env,Env).
declareConstraints([C|L],E,Ex) :-
  declareConstraint(C,E,E0),
  declareConstraints(L,E0,Ex).

findType(Nm,_,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,_)),!.
findType(Nm,Lc,_,anonType) :-
  reportError("type %s not known",[Nm],Lc).

checkEquation(Lc,H,C,R,funType(AT,RT),[equation(Lc,Nm,Args,Cond,Exp)|Defs],Defs,E) :-
  splitHead(H,Nm,A),
  pushScope(E,Env),
  typeOfArgTerm(A,AT,Env,E0,Args),
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(C,LogicalTp,E0,E1,Cond),
  typeOfTerm(R,RT,E1,E2,Exp),
  dischargeConstraints(E,E2).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,_) :-
  reportError("equation not consistent with expected type: %s",[ProgramType],Lc).

checkPtnRule(Lc,H,G,R,ptnType(AT,RT),[ptnRule(Lc,Nm,Args,Cond,Exp)|Defs],Defs,E) :-
  splitHead(H,Nm,A),
  pushScope(E,Env),
  typeOfArgTerm(A,AT,Env,E0,Args),
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(G,LogicalTp,E0,E1,Cond),
  typeOfTerm(R,RT,E1,E2,Exp),
  dischargeConstraints(E,E2).
checkPtnRule(Lc,_,_,_,ProgramType,Defs,Defs,_) :-
  reportError("pattern rule not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,C,R,Tp,defn(Lc,Nm,Cond,Value),Env) :-
  splitHead(L,Nm,none),
  pushScope(Env,E),
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(C,LogicalTp,E,E1,Cond),
  typeOfTerm(R,Tp,E1,E2,Value),
  dischargeConstraints(E,E2).

checkVarDefn(Lc,L,C,R,ref(Tp),vdefn(Lc,Nm,Cond,Value),Env) :-
  splitHead(L,Nm,none),
  pushScope(Env,E),
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(C,LogicalTp,E,E1,Cond),
  typeOfTerm(R,Tp,E1,E2,Value),
  dischargeConstraints(E,E2).

checkThetaBody(Tp,Lc,Els,Env,Defs,Others,Types,ClassPath) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  moveConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Cx,E0,BaseEnv),
  declareVr(Lc,"this",Tp,Face,BaseEnv,ThEnv),
  thetaEnv(ClassPath,nullRepo,Lc,Els,Face,ThEnv,OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,Face,Public,_,Types,[],[]),
  dischargeConstraints(Env,OEnv).

checkRecordBody(Tp,Lc,Els,Env,Defs,Others,Types,ClassPath) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,Face),
  moveConstraints(Face,Cx,CFace),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Cx,E0,BaseEnv),
  declareVr(Lc,"this",Tp,Face,BaseEnv,ThEnv),
  recordEnv(ClassPath,nullRepo,Lc,Els,CFace,ThEnv,OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,CFace,Public,_,Types,[],[]),
  dischargeConstraints(Env,OEnv).

splitHead(tuple(_,"()",[A]),Nm,Args) :-!,
  splitHd(A,Nm,Args).
splitHead(Term,Nm,Args) :-
  splitHd(Term,Nm,Args).

splitHd(Term,Nm,A) :-
  isRound(Term,_,Nm,A).
splitHd(Id,Nm,none) :-
  isIden(Id,_,Nm),!.
splitHd(Term,"()",Term) :-
  isTuple(Term,_).

collectPrograms([],Env,Env,_,_,Defs,Defs).
collectPrograms([Eqn|Stmts],Env,Ev,Tp,Cx,[funDef(Lc,Nm,Tp,Cx,[Eqn|Eqns])|Defs],Dx) :-
  Eqn = equation(Lc,Nm,_,_,_),
  collectEquations(Stmts,S0,Nm,Eqns),
  declareVr(Lc,Nm,Tp,Env,E0),
  collectPrograms(S0,E0,Ev,Tp,Cx,Defs,Dx).
collectPrograms([defn(Lc,Nm,Cond,Value)|Stmts],Env,Ev,Tp,Cx,
    [defn(Lc,Nm,Cx,Cond,Tp,Value)|Defs],Dx) :-
  faceOfType(Tp,Env,Face),
  freshen(Face,Env,_,VFace),
  declareVr(Lc,Nm,VFace,Tp,Env,E0),
  collectPrograms(Stmts,E0,Ev,Tp,Cx,Defs,Dx).
collectPrograms([vdefn(Lc,Nm,Cond,Value)|Stmts],Env,Ev,Tp,Cx,
    [vdefn(Lc,Nm,Cx,Cond,Tp,Value)|Defs],Dx) :-
  declareVr(Lc,Nm,refType(Tp),Env,E0),
  collectPrograms(Stmts,E0,Ev,Tp,Cx,Defs,Dx).
collectPrograms([Rl|Stmts],Env,Ev,Tp,Cx,[grDef(Lc,Nm,Tp,Cx,[Rl|Rules])|Defs],Dx) :-
  isGrammarRule(Rl,Lc,Nm),
  collectGrammarRules(Stmts,S0,Nm,Rules),
  declareVr(Lc,Nm,Tp,Env,E0),
  collectPrograms(S0,E0,Ev,Tp,Cx,Defs,Dx).

collectEquations([Eqn|Stmts],Sx,Nm,[Eqn|Ex]) :-
  Eqn = equation(_,Nm,_,_,_),
  collectEquations(Stmts,Sx,Nm,Ex).
collectEquations([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectEquations(Stmts,Sx,Nm,Eqns).
collectEquations([],[],_,[]).

collectGrammarRules([Rl|Stmts],Sx,Nm,[Rl|Ex]) :-
  isGrammarRule(Rl,_,Nm),
  collectGrammarRules(Stmts,Sx,Nm,Ex).
collectGrammarRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectGrammarRules(Stmts,Sx,Nm,Eqns).
collectGrammarRules([],[],_,[]).

isGrammarRule(grRule(Lc,Nm,_,_,_),Lc,Nm).

checkImplementation(Stmt,INm,[Impl|Dfs],Dfs,Env,Ex,_,Path) :-
  isImplementationStmt(Stmt,Lc,Quants,Cons,Sq,IBody),
  parseContractConstraint(Quants,Cons,Sq,Env,Nm,ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
  moveConstraints(CnSpec,AC,contractExists(Spec,IFace)),
  declareTypeVars(IQ,Lc,Env,ThEnv),
  (isBraceTuple(IBody,_,Els) ->
     thetaEnv(Path,nullRepo,Lc,Els,IFace,ThEnv,OEnv,ThDefs,Public,_,Others) ;
   isQBraceTuple(IBody,_,Els) ->
     recordEnv(Path,nullRepo,Lc,Els,IFace,ThEnv,OEnv,ThDefs,Public,_,Others) ;
   reportError("expecting better than this %s",[IBody],Lc)),
  computeExport(ThDefs,IFace,Public,BodyDefs,Types,[],[]),
  implementationName(Spec,ImplName),
  Impl = implDef(Lc,INm,ImplName,Spec,AC,ThDefs,BodyDefs,Types,Others),
  declareImplementation(Nm,Impl,Env,Ex),
  dischargeConstraints(Env,OEnv),!.
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_) :-
  locOfAst(Stmt,Lc),
  reportError("could not check implementation statement",[Lc]).

sameLength(L1,L2,_) :- length(L1,L), length(L2,L),!.
sameLength(L1,_,Lc) :-
  length(L1,L),
  reportError("expecting %s elements",[L],Lc).

declImpl(imp(ImplNm,Spec),SoFar,[(ImplNm,Spec)|SoFar]).

typeOfArgTerm(T,Tp,Env,Ev,tple(Lc,Els)) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els).
typeOfArgTerm(T,Tp,Env,Ev,Exp) :-
  typeOfTerm(T,Tp,Env,Ev,Exp).

typeOfTerm(V,_,Env,Env,v(Lc,N)) :-
  isIden(V,Lc,"_"),!,
  genstr("_",N).
typeOfTerm(V,Tp,Env,Ev,Term) :-
  isIden(V,Lc,N),
  isVar(N,Env,Spec),!,
  typeOfVar(Lc,N,Tp,Spec,Env,Ev,Term).
typeOfTerm(V,Tp,Ev,Env,v(Lc,N)) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,Ev,Env).
typeOfTerm(integer(Lc,Ix),Tp,Env,Env,intLit(Ix)) :- !,
  findType("integer",Lc,Env,IntTp),
  checkType(Lc,IntTp,Tp,Env).
typeOfTerm(float(Lc,Ix),Tp,Env,Env,floatLit(Ix)) :- !,
  findType("float",Lc,Env,FltTp),
  checkType(Lc,FltTp,Tp,Env).
typeOfTerm(string(Lc,Ix),Tp,Env,Env,stringLit(Ix)) :- !,
  findType("string",Lc,Env,StrTp),
  checkType(Lc,StrTp,Tp,Env).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  checkType(Lc,RT,Tp,Env),
  typeOfTerm(L,RT,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  binary(Lc,":",LT,R,NT),
  typeOfTerm(NT,Tp,Env,Ev,Exp).
typeOfTerm(P,Tp,Env,Ex,where(Lc,Ptn,Cond)) :-
  isWhere(P,Lc,L,C),
  typeOfTerm(L,Tp,Env,E0,Ptn),
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(C,LogicalTp,E0,Ex,Cond).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  recordAccessExp(Lc,Rc,Fld,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,cond(Lc,Test,Then,Else)) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(Tst,LogicalTp,Env,E0,Test),
  typeOfTerm(Th,Tp,E0,E1,Then),
  typeOfTerm(El,Tp,E1,Ev,Else).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isSquareTuple(Term,Lc,Els), !,
  checkSquareTuple(Lc,Els,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Env,theta(Path,Defs,Others,Types)) :-
  isBraceTuple(Term,Lc,Els),
  genstr("theta",Path),
  checkThetaBody(Tp,Lc,Els,Env,Defs,Others,Types,Path).
typeOfTerm(Term,Tp,Env,Env,record(Path,Defs,Others,Types)) :-
  isQBraceTuple(Term,Lc,Els),
  genstr("record",Path),
  checkRecordBody(Tp,Lc,Els,Env,Defs,Others,Types,Path).
typeOfTerm(Term,Tp,Env,Env,theta(Lbl,Defs,Others,Types)) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfKnown(F,consType(FnTp,Tp),Env,E0,Fun),
  funLbl(Fun,Lbl),
  checkThetaBody(FnTp,Lc,Els,E0,Defs,Others,Types,Lbl).
typeOfTerm(Term,Tp,Env,Env,record(Lbl,Defs,Others,Types)) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("R",FnTp),
  typeOfKnown(F,consType(FnTp,Tp),Env,E0,Fun),
  funLbl(Fun,Lbl),
  checkRecordBody(FnTp,Lc,Els,E0,Defs,Others,Types,Lbl).
typeOfTerm(Trm,Tp,Env,Ev,Exp) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfTerm(Inner,Tp,Env,Ev,Exp).
typeOfTerm(Trm,Tp,Env,Ev,tple(Lc,Els)) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  checkType(Lc,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
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
  binary(Lc,"-",name(Lc,"zero"),Arg,Sub),
  typeOfTerm(Sub,Tp,Env,Ev,Exp)).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("F",FnTp),
  newTypeVar("A",At),
  typeOfKnown(F,FnTp,Env,E0,Fun),
  (sameType(funType(At,Tp),FnTp,E0) ->
    evidence(At,E0,_,AT),
    typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args),
    Exp = apply(Fun,Args) ;
   sameType(consType(At,Tp),FnTp,E0) ->
    evidence(At,E0,_,AT),
    typeOfArgTerm(tuple(Lc,"()",A),AT,E0,Ev,Args),
    Exp = cons(Lc,Fun,Args);
   reportError("invalid function %s in call",[Fun],Lc)).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isSquareTerm(Term,Lc,F,[A]),!,
  typeOfIndex(Lc,F,A,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Env,lambda(equation(Lc,"$",Args,Cond,Exp))) :-
  isEquation(Term,Lc,H,C,R),
  isTuple(H,_,A),
  genTpVars(A,AT),
  newTypeVar("_E",RT),
  checkType(Lc,funType(tupleType(AT),RT),Tp,Env),
  typeOfTerms(A,AT,Env,E1,Lc,Args),
  findType("logical",Lc,Env,LogicalTp),
  typeOfTerm(C,LogicalTp,E1,E2,Cond),
  typeOfTerm(R,RT,E2,_,Exp).
typeOfTerm(Term,Tp,Env,Ex,conj(Lc,Lhs,Rhs)) :-
  isConjunct(Term,Lc,L,R),!,
  findType("logical",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  typeOfTerm(L,LogicalTp,Env,E1,Lhs),
  typeOfTerm(R,LogicalTp,E1,Ex,Rhs).
typeOfTerm(Term,Tp,Env,Ex,disj(Lc,Lhs,Rhs)) :-
  isDisjunct(Term,Lc,L,R),!,
  findType("logical",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  typeOfTerm(L,LogicalTp,Env,E1,Lhs),
  typeOfTerm(R,LogicalTp,E1,Ex,Rhs).
typeOfTerm(Term,Tp,Env,Ex,neg(Lc,Rhs)) :-
  isNegation(Term,Lc,R),!,
  findType("logical",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  typeOfTerm(R,LogicalTp,Env,Ex,Rhs).
typeOfTerm(Term,Tp,Env,Ev,match(Lc,Lhs,Rhs)) :-
  isMatch(Term,Lc,P,E),!,
  findType("logical",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  typeOfTerm(P,TV,Env,E0,Lhs),
  typeOfTerm(E,TV,E0,Ev,Rhs).
typeOfTerm(Term,Tp,Env,Ev,phrase(Lc,NT,Strm,Rest)) :-
  isBinary(Term,Lc,"%%",L,R),
  isBinary(R,"~",S,M),!,
  findType("logical",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_S",StrmTp),
  newTypeVar("_E",ElTp),
  checkGrammarType(Lc,Env,StrmTp,ElTp),
  typeOfTerm(S,StrmTp,Env,E0,Strm),
  typeOfTerm(M,StrmTp,E0,E1,Rest),
  currentVar("stream",E1,OV),
  declareVr(Lc,"stream",StrmTp,E1,E2),
  checkNonTerminal(L,StrmTp,ElTp,E2,E3,NT),
  restoreVar("stream",E3,OV,Ev).
typeOfTerm(Term,Tp,Env,Ev,phrase(Lc,NT,Strm)) :-
  isBinary(Term,Lc,"%%",L,R),
  findType("logical",Lc,Env,LogicalTp),
  checkType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_S",StrmTp),
  newTypeVar("_E",ElTp),
  checkGrammarType(Lc,Env,StrmTp,ElTp),
  typeOfTerm(R,StrmTp,Env,E1,Strm),
  binary(Lc,",",L,name(Lc,"eof"),Phrase),
  currentVar("stream",E1,OV),
  declareVr(Lc,"stream",StrmTp,E1,E2),
  checkNonTerminal(Phrase,StrmTp,ElTp,E2,E3,NT),
  restoreVar("stream",E3,OV,Ev).
typeOfTerm(Term,Tp,Env,Env,void) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

funLbl(over(_,T,_),L) :- funLbl(T,L).
funLbl(v(_,L),L).
funLbl(cns(_,Nm),Nm).
funLbl(enm(_,Nm),Nm).
funLbl(mtd(_,Nm),Nm).

typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp) :-
  isBinary(Arg,"->",Ky,Vl),!,
  ternary(Lc,"_put",Mp,Ky,Vl,Term),
  typeOfTerm(Term,Tp,Env,Ev,Exp).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp) :-
  isUnary(Arg,"\\+",Ky),!,
  binary(Lc,"_remove",Mp,Ky,Term),
  typeOfTerm(Term,Tp,Env,Ev,Exp).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp) :-
  binary(Lc,"find",Mp,Arg,Term),
  typeOfTerm(Term,Tp,Env,Ev,Exp).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

recordFace(Trm,Env,Env,v(Lc,Nm),Face) :-
  isIden(Trm,Lc,Nm),
  isVar(Nm,Env,vr(_,_,Face,_)),!.
recordFace(Trm,Env,Ev,Rec,Face) :-
  newTypeVar("_R",AT),
  typeOfKnown(Trm,AT,Env,Ev,Rec),
  faceOfType(AT,Env,Fc),
  freshen(Fc,Env,_,Face).

recordAccessExp(Lc,Rc,Fld,ET,Env,Ev,dot(Rec,Fld)) :-
  recordFace(Rc,Env,Ev,Rec,Fce),
  deRef(Fce,Face),
  fieldInFace(Face,Fld,Lc,FTp),!,
  freshen(FTp,Env,_,Tp),
  checkType(Lc,Tp,ET,Env).

macroMapEntries(Lc,[],name(Lc,"_empty")).
macroMapEntries(_,[E|L],T) :-
  isBinary(E,Lc,"->",Ky,Vl),!,
  macroMapEntries(Lc,L,Tr),
  roundTerm(Lc,"_put",[Tr,Ky,Vl],T).
macroMapEntries(Lc,[E|L],T) :-
  reportError("invalid entry in map %s",[E],Lc),
  macroMapEntries(Lc,L,T).

fieldInFace(faceType(Fields,_),Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfKnown(T,Tp,Env,Ev,Exp) :-
  isIden(T,Lc,Nm),
  isVar(Nm,Env,Spec),!,
  typeOfVar(Lc,Nm,Tp,Spec,Env,Ev,Exp).
typeOfKnown(T,Tp,Env,Env,v(Lc,Nm)) :-
  isIden(T,Lc,Nm),
  reportError("variable %s not declared, expecting a %s",[Nm,Tp],Lc).
typeOfKnown(T,Tp,Env,Ev,Exp) :-
  typeOfTerm(T,Tp,Env,Ev,Exp).

typeOfTerms([],[],Env,Env,_,[]).
typeOfTerms([],[T|_],Env,Env,Lc,[]) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfTerms([A|_],[],Env,Env,_,[]) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfTerms([A|As],[ETp|ElTypes],Env,Ev,_,[Term|Els]) :-
  deRef(ETp,ElTp),
  typeOfTerm(A,ElTp,Env,E0,Term),
  locOfAst(A,Lc),
  typeOfTerms(As,ElTypes,E0,Ev,Lc,Els).

% Analyse a list term to try to disambiguate maps from lists.

checkSquareTuple(Lc,Els,Tp,Env,Ev,Exp) :-
  (isMapSequence(Els) ; isMapType(Tp,Env)) ->
    macroMapEntries(Lc,Els,Trm),
    newTypeVar("k",KT),
    newTypeVar("v",VT),
    findType("map",Lc,Env,MapOp),
    MapTp = typeExp(MapOp,[KT,VT]),
    checkType(Lc,MapTp,Tp,Env),
    typeOfTerm(Trm,Tp,Env,Ev,Exp);
  (isListSequence(Els) ; isListType(Tp,Env)) ->
    findType("list",Lc,Env,ListOp),
    newTypeVar("L",ElTp),
    ListTp = typeExp(ListOp,[ElTp]),
    checkType(Lc,ListTp,Tp,Env),
    typeOfListTerm(Els,Lc,ElTp,Tp,Env,Ev,Exp);
  checkSequenceTerm(Lc,Els,Tp,Env,Ev,Exp).

isMapSequence([E|_]) :-
  isBinary(E,_,"->",_,_).

isMapType(Tp,Env) :-
  isType("map",Env,tpDef(_,MpTp,_)),!,
  deRef(Tp,typeExp(MpOp,_)),
  deRef(MpOp,MpTp).

isListSequence([E|_]) :-
  \+isBinary(E,_,"->",_,_).

isListType(Tp,Env) :-
  isType("list",Env,tpDef(_,LstTp,_)),!,
  deRef(Tp,typeExp(LsOp,_)),
  deRef(LsOp,LstTp).

typeOfListTerm([],Lc,_,ListTp,Env,Ev,Exp) :-
  typeOfTerm(name(Lc,"[]"),ListTp,Env,Ev,Exp).
typeOfListTerm([Last],_,ElTp,ListTp,Env,Ev,apply(Op,tple(Lc,[Hd,Tl]))) :-
  isBinary(Last,Lc,",..",L,R),
  newTypeVar("_",LiTp),
  typeOfKnown(name(Lc,",.."),LiTp,Env,E0,Op),
  typeOfTerm(L,ElTp,E0,E1,Hd),
  typeOfTerm(R,ListTp,E1,Ev,Tl).
typeOfListTerm([El|More],_,ElTp,ListTp,Env,Ev,apply(Op,tple(Lc,[Hd,Tl]))) :-
  locOfAst(El,Lc),
  newTypeVar("_",LiTp),
  typeOfKnown(name(Lc,",.."),LiTp,Env,E0,Op),
  typeOfTerm(El,ElTp,E0,E1,Hd),
  typeOfListTerm(More,Lc,ElTp,ListTp,E1,Ev,Tl).

checkSequenceTerm(Lc,Els,Tp,Env,Ev,Exp) :-
  genIden(Lc,Seq),
  macroSequenceTerm(Els,Lc,Seq,Tsts),
  isTuple(Cond,Lc,Tsts),
  binary(Lc,"::",Seq,Cond,Term),
  typeOfTerm(Term,Tp,Env,Ev,Exp).

macroSequenceTerm([],Lc,V,[Last]) :-
  unary(Lc,"_eof",V,Last).
macroSequenceTerm([E|L],_,V,[F|M]) :-
  locOfAst(E,Lc),
  genIden(Lc,NX),
  ternary(Lc,"_hdtl",V,E,NX,F),
  macroSequenceTerm(L,Lc,NX,M).

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Lc,S,T,_) :-
  reportError("%s not consistent with expected type %s",[S,T],Lc).

checkCallArgs(Lc,Pred,A,ArgTps,Env,Ev,apply(Pred,Args)) :-

  typeOfTerms(A,ArgTps,Env,Ev,Lc,Args).
checkCallArgs(Lc,Pred,A,ArgTps,Env,Env,void) :-
  reportError("arguments %s of %s not consistent with expected types %s",
      [A,Pred,tupleType(ArgTps)],Lc).

checkGrammarType(Lc,Env,Tp,ElTp) :-
  getContract("stream",Env,conDef(_,_,Spec)),
  freshen(Spec,Env,_,contractExists(conTract(_,[Arg],[Dep]),_)),
  checkType(Lc,Arg,Tp,Env),
  checkType(Lc,Dep,ElTp,Env).

checkGrammarRule(Lc,L,C,R,grammarType(AT,Tp),[grRule(Lc,Nm,Args,Cnd,Body)|Dfs],Dfs,E) :-
  splitHead(L,Nm,A),
  pushScope(E,E0),
  declareVr(Lc,"stream",Tp,E0,E1),
  newTypeVar("_E",ElTp),
  typeOfArgTerm(A,AT,E1,E2,Args),!,
  findType("logical",Lc,E,LogicalTp),
  typeOfterm(C,LogicalTp,E2,E3,Cnd),
  checkNonTerminal(R,Tp,ElTp,E2,E3,Body),
  dischargeConstraints(E,E3).

checkNonTerminal(tuple(Lc,"[]",Els),_,ElTp,E,Env,terminals(Lc,Terms)) :- !,
  checkTerminals(Els,"_hdtl",Terms,ElTp,E,Env).
checkNonTerminal(string(Lc,Text),_,ElTp,Env,Env,terminals(Lc,Terms)) :- !,
  explodeStringLit(Lc,Text,IntLits),
  checkTerminals(IntLits,"_hdtl",Terms,ElTp,Env,_).  % explode strings into code points
checkNonTerminal(tuple(Lc,"()",NT),Tp,ElTp,Env,Ex,GrNT) :-
  checkNonTerminals(NT,Lc,Tp,ElTp,Env,Ex,GrNT).
checkNonTerminal(Term,Tp,ElTp,Env,Ex,conj(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,",",L,R), !,
  checkNonTerminal(L,Tp,ElTp,Env,E1,Lhs),
  checkNonTerminal(R,Tp,ElTp,E1,Ex,Rhs).
checkNonTerminal(Term,Tp,ElTp,Env,Ex,cond(Lc,Test,Either,Or)) :-
  isConditional(Term,Lc,T,L,R),!,
  checkNonTerminal(T,Tp,ElTp,Env,E0,Test),
  checkNonTerminal(L,Tp,ElTp,E0,E1,Either),
  checkNonTerminal(R,Tp,ElTp,E1,Ex,Or).
checkNonTerminal(Term,Tp,ElTp,Env,Ex,disj(Lc,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),!,
  checkNonTerminal(L,Tp,ElTp,Env,E1,Either),
  checkNonTerminal(R,Tp,ElTp,E1,Ex,Or).
checkNonTerminal(Term,Tp,ElTp,Env,Ex,one(Lc,Test)) :-
  isUnary(Term,Lc,"!",N),!,
  checkNonTerminal(N,Tp,ElTp,Env,Ex,Test).
checkNonTerminal(Term,Tp,ElTp,Env,Env,neg(Lc,Test)) :-
  isUnary(Term,Lc,"\\+",N),!,
  checkNonTerminal(N,Tp,ElTp,Env,_,Test).
checkNonTerminal(Term,Tp,ElTp,Env,Env,ahead(Lc,Test)) :-
  isUnary(Term,Lc,"+",N),!,
  checkNonTerminal(N,Tp,ElTp,Env,_,Test).
checkNonTerminal(Term,_,_,Env,Ev,goal(Lc,unify(Lc,Lhs,Rhs))) :-
  isBinary(Term,Lc,"=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkNonTerminal(Term,_,_,Env,Ev,goal(Lc,neg(Lc,unify(Lc,Lhs,Rhs)))) :-
  isBinary(Term,Lc,"\\=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkNonTerminal(Term,Tp,_,Env,Ev,NT) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("_G",GrTp),
  typeOfKnown(F,GrTp,Env,E0,Pred),
  deRef(GrTp,GrType),
  checkGrCall(Lc,Pred,A,Tp,GrType,NT,E0,Ev).
checkNonTerminal(Term,_,_,Env,Env,goal(Lc,Cond)) :-
  isIden(Term,Lc,"eof"),
  unary(Lc,"_eof",name(Lc,"stream"),C),
  findType("logical",Lc,Env,LogicalTp),
  typeOfterm(C,LogicalTp,Env,_,Cond).
checkNonTerminal(Term,_,_,Env,Ex,goal(Lc,Cond)) :-
  isBraceTuple(Term,Lc,[C]),
  findType("logical",Lc,Env,LogicalTp),
  typeOfterm(C,LogicalTp,Env,Ex,Cond).

checkNonTerminals([],Lc,_,_,Env,Env,terminals(Lc,[])).
checkNonTerminals([N],_,Tp,ElTp,Env,Ev,NT) :- checkNonTerminal(N,Tp,ElTp,Env,Ev,NT).
checkNonTerminals([N|L],Lc,Tp,ElTp,Env,Ev,conj(Lc,Lhs,Rhs)) :-
  checkNonTerminal(N,Tp,ElTp,Env,E0,Lhs),
  locOfAst(N,LLc),
  checkNonTerminals(L,LLc,Tp,ElTp,E0,Ev,Rhs).

explodeStringLit(Lc,Str,Terms) :-
  string_codes(Str,Codes),
  map(Codes,checker:makeIntLit(Lc),Terms).

makeIntLit(Lc,C,integer(Lc,C)).

checkGrCall(Lc,Pred,A,Tp,grammarType(ArgTps,StrmTp),Call,Env,Ev) :-
  checkType(Lc,StrmTp,Tp,Env),
  checkCallArgs(Lc,Pred,A,ArgTps,Env,Ev,Call).
checkGrCall(Lc,Pred,_,StrmTp,Tp,terminals(Lc,[]),Env,Env) :-
  reportError("type of %s:%s not a grammar of right type %s",[Pred,StrmTp,Tp],Lc).

checkTerminals([],_,[],_,Env,Env) :- !.
checkTerminals([T|More],V,[term(Lc,Cond)|Out],ElTp,Env,Ex) :-
  locOfAst(T,Lc),
  ternary(Lc,V,name(Lc,"stream"),T,name(Lc,"stream"),C),
  findType("logical",Lc,Env,LogicalTp),
  typeOfterm(C,LogicalTp,Env,E1,Cond),
  checkTerminals(More,V,Out,ElTp,E1,Ex).

computeExport([],_,_,[],[],[],[]).
computeExport([Def|Defs],Fields,Public,Exports,Types,Cons,Impls) :-
  exportDef(Def,Fields,Public,Exports,Ex,Types,Tx,Cons,Cx,Impls,Ix),!,
  computeExport(Defs,Fields,Public,Ex,Tx,Cx,Ix).

exportDef(funDef(_,Nm,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(typeDef(_,Nm,_,FRule),_,Public,Ex,Ex,[(Nm,FRule)|Tx],Tx,Cx,Cx,Impl,Impl) :-
  isPublicType(Nm,Public).
exportDef(defn(_,Nm,_,_,Tp,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(vdefn(_,Nm,_,_,Tp,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl):-
  isPublicVar(Nm,Fields,Public).
exportDef(grDef(_,Nm,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(Con,_,Public,Ex,Ex,Types,Types,[Con|Cx],Cx,Impl,Impl) :-
  isPublicContract(Con,Public).
exportDef(implDef(_,INm,IName,Spec,_,_,_,_,_),_,Public,Ex,Ex,Tps,Tps,Cx,Cx,
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

dischargeConstraints(_,_).
