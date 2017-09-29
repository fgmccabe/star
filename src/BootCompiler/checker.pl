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

checkProgram(Prog,Vers,Repo,prog(pkg(Pkg,Vers),Imports,ODefs,OOthers,Exports,Types,Contracts,Impls)) :-
  stdDict(Base),
  isBraceTerm(Prog,Lc,Pk,Els),
  packageName(Pk,Pkg),
  pushScope(Base,Env),
  thetaEnv(Pkg,Repo,Lc,Els,[],Env,OEnv,Defs,Public,Imports,Others),
  findImportedImplementations(Imports,[],OverDict),
  overload(Defs,OverDict,ODict,ODefs),
  overloadOthers(Others,ODict,OOthers),
  computeExport(ODefs,[],Public,Exports,Types,Contracts,Impls),
  dischargeConstraints(Base,OEnv),!.

thetaEnv(Pkg,Repo,Lc,Els,Fields,Base,TheEnv,Defs,Public,Imports,Others) :-
  macroRewrite(Els,Stmts),
  displayAll(Stmts),
  dependencies(Stmts,Groups,Public,Annots,Imps,Otrs),
  processImportGroup(Imps,Imports,Repo,Base,IBase),
  pushFace(Fields,Lc,IBase,Env),
  checkGroups(Groups,Fields,Annots,Defs,Env,TheEnv,Pkg),
  checkOthers(Otrs,Others,TheEnv,Pkg).

processImportGroup(Stmts,ImportSpecs,Repo,Env,Ex) :-
  findAllImports(Stmts,Lc,Imports),
  importAll(Imports,Repo,AllImports),
  importAllDefs(AllImports,Lc,ImportSpecs,Repo,Env,Ex).

findAllImports([],_,[]).
findAllImports([St|More],Lc,[Spec|Imports]) :-
  findImport(St,Lc,private,Spec),
  findAllImports(More,_,Imports).

findImport(St,Lc,_,Spec) :-
  isUnary(St,Lc,"private",I),
  findImport(I,_,private,Spec).
findImport(St,Lc,_,Spec) :-
  isUnary(St,Lc,"public",I),
  findImport(I,_,public,Spec).
findImport(St,Lc,Viz,import(Viz,Pkg)) :-
  isUnary(St,Lc,"import",P),
  pkgName(P,Pkg).

importAll(Imports,Repo,AllImports) :-
  closure(Imports,[],checker:notAlreadyImported,checker:importMore(Repo),AllImports).

importDefs(spec(_,faceType(Exported),faceType(Types),_,Cons,_,_),Lc,Env,Ex) :-
  declareFields(Exported,Lc,Env,E0),
  importTypes(Types,Lc,E0,E1),
  importContracts(Cons,Lc,E1,Ex).

declareFields([],_,Env,Env).
declareFields([(Nm,Tp)|More],Lc,Env,Ex) :-
  declareVar(Nm,vr(Nm,Lc,Tp),Env,E0),
  declareFields(More,Lc,E0,Ex).

importTypes([],_,Env,Env).
importTypes([(Nm,Rule)|More],Lc,Env,Ex) :-
  pickTypeTemplate(Rule,Type),
  declareType(Nm,tpDef(Lc,Type,Rule),Env,E0),
  importTypes(More,Lc,E0,Ex).

pickTypeTemplate(univType(_,Tp),XTp) :-
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
      [import(Viz,Pkg,Exported,Types,Classes,Contracts,Impls)|Specs],Repo,Env,Ex) :-
  importPkg(Pkg,Repo,Spec),
  Spec = spec(_,Exported,Types,Classes,Contracts,Impls,_),
  importDefs(Spec,Lc,Env,Ev0),
  importAllDefs(More,Lc,Specs,Repo,Ev0,Ex).

importContracts([],_,Env,Env).
importContracts([C|L],Lc,E,Env) :-
  C = contract(Nm,_,_,_,_),
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
  isUnary(St,Lc,"assert",C),!,
  checkCond(C,Env,_,Cond).
checkOther(St,[show(Lc,Show)|More],More,Env,_) :-
  isUnary(St,Lc,"show",E),!,
  unary(Lc,"disp",E,Ex),
  unary(Lc,"formatSS",Ex,FC), % create the call formatSS(disp(E))
  findType("string",Lc,Env,StringTp),
  typeOfTerm(FC,StringTp,Env,_,Show).

checkGroups([],_,_,[],E,E,_).
checkGroups([Gp|More],Fields,Annots,Defs,Env,E,Path) :-
  groupType(Gp,GrpType),
  checkGroup(Gp,GrpType,Fields,Annots,Defs,D0,Env,E0,Path),!,
  checkGroups(More,Fields,Annots,D0,E0,E,Path).

groupType([(var(_),_,_)|_],var).
groupType([(cns(_),_,_)|_],cns).
groupType([(tpe(_),_,_)|_],tpe).
groupType([(con(_),_,_)|_],con).
groupType([(imp(_),_,_)|_],imp).

checkGroup(Grp,tpe,_,_,Defs,Dx,Env,Ex,Path) :-
  typeGroup(Grp,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,var,Fields,Annots,Defs,Dx,Env,Ex,Path) :-
  varGroup(Grp,Fields,Annots,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,cns,Fields,Annots,Defs,Dx,Env,Ex,Path) :-
  cnsGroup(Grp,Fields,Annots,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,con,_,_,Defs,Dx,Env,Ex,Path) :-
  contractGroup(Grp,Defs,Dx,Env,Ex,Path).
checkGroup(Grp,imp,_,_,Defs,Dx,Env,Ex,Path) :-
  implementationGroup(Grp,Defs,Dx,Env,Ex,Path).

contractGroup([(con(N),Lc,[ConStmt])|_],[Contract|Defs],Defs,Env,Ex,Path) :-
  parseContract(ConStmt,Env,Path,Contract),
  defineContract(N,Lc,Contract,Env,Ex).

defineContract(N,Lc,Contract,E0,Ex) :-
  declareContract(N,Contract,E0,E1),
  declareMethods(Contract,Lc,E1,Ex).

declareMethods(contract(_,_,Spec,_,MtdsTp),Lc,Env,Ev) :-
  moveQuants(Spec,Q,Con),
  moveQuants(MtdsTp,_,faceType(Methods)),
  formMethods(Methods,Lc,Q,Con,Env,Ev).

formMethods([],_,_,_,Env,Env).
formMethods([(Nm,Tp)|M],Lc,Q,Con,Env,Ev) :-
  moveQuants(Tp,FQ,QTp),
  merge(FQ,Q,MQ),
  moveQuants(MTp,MQ,constrained(QTp,Con)),
  declareVar(Nm,mtd(Lc,Nm,MTp),Env,E0),
  formMethods(M,Lc,Q,Con,E0,Ev).

% This is very elaborate - to support mutual recursion amoung types.
typeGroup(Grp,Defs,Dx,Env,Ex,Path) :-
  defineTypes(Grp,Env,TmpEnv,Path),
  parseTypeDefs(Grp,TpDefs,[],TmpEnv,Path),
  declareTypes(TpDefs,TpDefs,Defs,Dx,Env,Ex).

defineTypes([],Env,Env,_).
defineTypes([(tpe(N),Lc,[Stmt])|More],Env,Ex,Path) :-
  defineType(N,Lc,Stmt,Env,E0,Path),
  defineTypes(More,E0,Ex,Path).
defineType([(tpe(N),Lc,[_|_])|More],Env,Ex,Path) :-
  reportError("multiple type definition statement for %s",[N],Lc),
  defineTypes(More,Env,Ex,Path).

defineType(N,Lc,_,Env,Env,_) :-
  isType(N,Env,tpDef(_,OLc,_)),!,
  reportError("type %s already defined at %s",[N,OLc],Lc).
defineType(N,Lc,St,Env,Ex,Path) :-
  parseTypeCore(St,Type,Path),
  declareType(N,tpDef(Lc,Type,faceType([])),Env,Ex).
defineType(_,Lc,St,Env,Env,_) :-
  reportError("cannot parse type statement %s",[St],Lc).

parseTypeDefs([],Defs,Defs,_,_).
parseTypeDefs([(_,_,[Stmt])|More],[Def|D0],Dx,TmpEnv,Path) :-
  parseTypeDef(Stmt,TmpEnv,Path,Def),
  parseTypeDefs(More,D0,Dx,TmpEnv,Path).
parseTypeDefs([_|More],Defs,Dx,TmpEnv,Path) :-
  parseTypeDefs(More,Defs,Dx,TmpEnv,Path).

declareTypes([],_,Defs,Defs,Env,Env).
declareTypes([typeDef(Lc,N,Type,FaceRule)|More],TpDefs,[typeDef(Lc,N,Type,FaceRule)|Defs],Dx,Env,Ex) :-
  declareType(N,tpDef(Lc,Type,FaceRule),Env,E0),
  declareTypes(More,TpDefs,Defs,Dx,E0,Ex).

cnsGroup(Grp,Fields,Annots,Defs,Dx,Base,Env,Path) :-
  parseAnnotations(Grp,Fields,Annots,Base,Env,Path),!,
  checkConstructors(Grp,Env,Defs,Dx,Path).

checkConstructors([],_,Defs,Defs,_).
checkConstructors([(cns(N),Lc,Stmts)|More],Env,Defs,Dx,Path) :-
  pickupVarType(N,Lc,Env,Tp),
  pickupThisType(Env,ThisType),
  evidence(Tp,ThisType,Q,PT),
  declareTypeVars(Q,Lc,Env,SEnv),
  moveConstraints(PT,Cx,ProgramType),
  declareConstraints(Cx,SEnv,StmtEnv),
  processCnsStmts(Stmts,ProgramType,Rules,[],StmtEnv,Path),
  collectPrograms(Rules,Env,Cx,Defs,D0),
  checkConstructors(More,Env,D0,Dx,Path).

processCnsStmts([],_,Defs,Defs,_,_).
processCnsStmts([St|More],ProgramType,Defs,Dx,Env,Path) :-
  processCnsStmt(St,ProgramType,Defs,D0,Env,Path),!,
  processCnsStmts(More,ProgramType,D0,Dx,Env,Path).

processCnsStmt(St,ClassTp,[labelRule(Lc,Nm,Hd,Cond,Repl,SuperFace)|Defs],Defs,E,_) :-
  isConstructorStmt(St,Lc,L,Cnd,R),
  pushScope(E,E0),
  checkClassHead(L,ClassTp,_,E0,E1,Nm,Hd),!,
  checkCond(Cnd,E1,E2,Cond),
  newTypeVar("Supr",SuperTp),
  typeOfTerm(R,SuperTp,E2,E3,Repl),
  generateClassFace(SuperTp,E,SuperFace),
  dischargeConstraints(E,E3).
processCnsStmt(St,Tp,Defs,Defs,_,_) :-
  locOfAst(St,Lc),
  reportError("Constructor %s not consistent with expected type %s",[St,Tp],Lc).

checkClassHead(Term,Tp,Tp,Env,Env,Nm,enum(Lc,Nm)) :-
  isIden(Term,Lc,Nm),!.
checkClassHead(Term,consType(AT,Tp),Tp,E0,Ex,Nm,apply(v(Lc,Nm),Args)) :-
  splitHead(Term,Nm,A),!,
  locOfAst(Term,Lc),
  typeOfTerms(A,AT,E0,Ex,Lc,Args).

varGroup(Grp,Fields,Annots,Defs,Dx,Base,Env,Path) :-
  parseAnnotations(Grp,Fields,Annots,Base,Env,Path),!,
  checkVarRules(Grp,Env,Defs,Dx,Path).

parseAnnotations([],_,_,Env,Env,_) :-!.
parseAnnotations([(var(Nm),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  parseAnnotation(Nm,Lc,Fields,Annots,Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).
parseAnnotations([(cns(Nm),Lc,_)|More],Fields,Annots,Env,Ex,Path) :-
  parseAnnotation(Nm,Lc,Fields,Annots,Env,E0),
  parseAnnotations(More,Fields,Annots,E0,Ex,Path).

parseAnnotation(Nm,_,_,Annots,Env,Ex) :-
  is_member((Nm,Annot),Annots),!,
  isBinary(Annot,Lc,":",_,T),
  parseType(T,Env,Tp),
  declareVar(Nm,vr(Nm,Lc,Tp),Env,Ex).
parseAnnotation(N,Lc,Fields,_,Env,Ex) :-
  is_member((N,Tp),Fields),!,
  declareVar(N,vr(N,Lc,Tp),Env,Ex).
parseAnnotation(N,Lc,_,_,Env,Env) :-
  reportError("no type annotation for variable %s",[N],Lc).

checkVarRules([],_,Defs,Defs,_).
checkVarRules([(var(N),Lc,Stmts)|More],Env,Defs,Dx,Path) :-
  pickupVarType(N,Lc,Env,Tp),
  pickupThisType(Env,ThisType),
  evidence(Tp,ThisType,Q,PT),
  declareTypeVars(Q,Lc,Env,SEnv),
  moveConstraints(PT,Cx,ProgramType),
  declareConstraints(Cx,SEnv,StmtEnv),
  processStmts(Stmts,ProgramType,Rules,[],StmtEnv,Path),
  collectPrograms(Rules,Env,Cx,Defs,D0),
  checkVarRules(More,Env,D0,Dx,Path).

pickupVarType(N,_,Env,Tp) :-
  isVar(N,Env,vr(_,_,Tp)),!.
pickupVarType(N,Lc,_,anonType) :- reportError("%s not declared",[N],Lc).

pickupThisType(Env,Tp) :-
  isVar("this",Env,vr(_,_,Tp)),!.
pickupThisType(_,voidType).

checkEvidenceBinding(_,_).

declareTypeVars([],_,Env,Env).
declareTypeVars([(thisType,_)|Vars],Lc,Env,Ex) :- !,
  declareTypeVars(Vars,Lc,Env,Ex).
declareTypeVars([(Nm,Tp)|Vars],Lc,Env,Ex) :-
  declareType(Nm,tpDef(Lc,Tp,voidType),Env,E0),
  declareTypeVars(Vars,Lc,E0,Ex).

declareConstraints([],Env,Env).
declareConstraints([C|L],E,Ex) :-
  declareConstraint(C,E,E0),
  declareConstraints(L,E0,Ex).

findType(Nm,_,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,_)),!.
findType(Nm,Lc,_,anonType) :-
  reportError("type %s not known",[Nm],Lc).

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
  isVarDefn(St,Lc,L,Cond,R),!,
  checkVarDefn(Lc,L,Cond,R,Tp,Def,Env).
processStmt(St,Tp,Defs,Defx,E,_) :-
  isPtnRule(St,Lc,L,C,R),
  checkPtnRule(Lc,L,C,R,Tp,Defs,Defx,E).
processStmt(St,Tp,Defs,Dx,E,_) :-
  isGrammarRule(St,Lc,L,P,R),
  processGrammarRule(Lc,L,P,R,Tp,Defs,Dx,E).
processStmt(St,Tp,Defs,Defs,_,_) :-
  locOfAst(St,Lc),
  reportError("Statement %s not consistent with expected type %s",[St,Tp],Lc).

checkEquation(Lc,H,C,R,funType(AT,RT),[equation(Lc,Nm,Args,Cond,Exp)|Defs],Defs,E) :-
  splitHead(H,Nm,A),
  pushScope(E,Env),
  typeOfTerms(A,AT,Env,E0,Lc,Args),
  checkCond(C,E0,E1,Cond),
  typeOfTerm(R,RT,E1,E2,Exp),
  dischargeConstraints(E,E2).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,_) :-
  reportError("equation not consistent with expected type: %s",[ProgramType],Lc).

checkPtnRule(Lc,H,G,R,ptnType(AT,RT),[ptnRule(Lc,Nm,Args,Cond,Exp)|Defs],Defs,E) :-
  splitHead(H,Nm,A),
  pushScope(E,Env),
  typeOfTerms(A,AT,Env,E0,Lc,Args),
  checkCond(G,E0,E1,Cond),
  typeOfTerm(R,RT,E1,E2,Exp),
  dischargeConstraints(E,E2).
checkPtnRule(Lc,_,_,_,ProgramType,Defs,Defs,_) :-
  reportError("pattern rule not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,C,R,Tp,defn(Lc,Nm,Cond,Value),Env) :-
  splitHead(L,Nm,none),
  pushScope(Env,E),
  checkCond(C,E,E1,Cond),
  typeOfTerm(R,Tp,E1,E2,Value),
  dischargeConstraints(E,E2).

checkVarDefn(Lc,L,C,R,ref(Tp),vdefn(Lc,Nm,Cond,Value),Env) :-
  splitHead(L,Nm,none),
  pushScope(Env,E),
  checkCond(C,E,E1,Cond),
  typeOfTerm(R,Tp,E1,E2,Value),
  dischargeConstraints(E,E2).

checkClassBody(ClassTp,Lc,Els,Env,Defs,Others,Types,ClassPath) :-
  getTypeFace(ClassTp,Env,Face),
  moveConstraints(Face,Cx,faceType(Fields)),
  pushScope(Env,Base),
  declareConstraints(Cx,Base,BaseEnv),
  declareVar("this",vr("this",Lc,ClassTp),BaseEnv,ThEnv),
  thetaEnv(ClassPath,nullRepo,Lc,Els,Fields,ThEnv,OEnv,Defs,Public,_Imports,Others),
  computeExport(Defs,Fields,Public,_,Types,[],[]),
  dischargeConstraints(Env,OEnv).

splitHead(tuple(_,"()",[A]),Nm,Args) :-!,
  splitHd(A,Nm,Args).
splitHead(Term,Nm,Args) :-
  splitHd(Term,Nm,Args).

splitHd(Term,Nm,Args) :-
  isRound(Term,Nm,Args).
splitHd(Id,Nm,none) :-
  isIden(Id,_,Nm),!.
splitHd(Term,"()",Args) :-
  isTuple(Term,Args).

collectPrograms([],_,_,Defs,Defs).
collectPrograms([Eqn|Stmts],Env,Cx,[function(Lc,Nm,Tp,Cx,[Eqn|Eqns])|Defs],Dx) :-
  Eqn = equation(Lc,Nm,_,_,_),
  collectEquations(Stmts,S0,Nm,Eqns),
  pickupVarType(Nm,Lc,Env,Tp),
  collectPrograms(S0,Env,Cx,Defs,Dx).
collectPrograms([defn(Lc,Nm,Cond,Value)|Stmts],Env,Cx,[defn(Lc,Nm,Cx,Cond,Tp,Value)|Defs],Dx) :-
  pickupVarType(Nm,Lc,Env,Tp),!,
  collectPrograms(Stmts,Env,Cx,Defs,Dx).
collectPrograms([varDefn(Lc,Nm,Cond,Value)|Stmts],Env,Cx,[varDefn(Lc,Nm,Cx,Cond,Tp,Value)|Defs],Dx) :-
  pickupVarType(Nm,Lc,Env,Tp),!,
  collectPrograms(Stmts,Env,Cx,Defs,Dx).
collectPrograms([Cl|Stmts],Env,Cx,[enum(Lc,Nm,Tp,Cx,[Cl|Rules],Face)|Defs],Dx) :-
  isRuleForEnum(Cl,Lc,Nm),!,
  collectEnumRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generateClassFace(Tp,Env,Face),
  collectPrograms(S0,Env,Cx,Defs,Dx).
collectPrograms([Cl|Stmts],Env,Cx,[class(Lc,Nm,Tp,Cx,[Cl|Rules],Face)|Defs],Dx) :-
  isRuleForClass(Cl,Lc,Nm),!,
  collectClassRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  generateClassFace(Tp,Env,Face),
  collectPrograms(S0,Env,Cx,Defs,Dx).
collectPrograms([Rl|Stmts],Env,Cx,[grammar(Lc,Nm,Tp,Cx,[Rl|Rules])|Defs],Dx) :-
  isGrammarRule(Rl,Lc,Nm),
  collectGrammarRules(Stmts,S0,Nm,Rules),
  pickupVarType(Nm,Lc,Env,Tp),
  collectPrograms(S0,Env,Cx,Defs,Dx).

collectClauses([],[],_,[]).
collectClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = clause(_,Nm,_,_,_),!,
  collectMoreClauses(Stmts,Sx,Nm,Ex).

collectMoreClauses([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  Cl = clause(_,Nm,_,_,_),!,
  collectMoreClauses(Stmts,Sx,Nm,Ex).
collectMoreClauses([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectMoreClauses(Stmts,Sx,Nm,Eqns).
collectMoreClauses([],[],_,[]).

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

isGrammarRule(grammarRule(Lc,Nm,_,_,_),Lc,Nm).

collectClassRules([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  isRuleForClass(Cl,_,Nm),!,
  collectClassRules(Stmts,Sx,Nm,Ex).
collectClassRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectClassRules(Stmts,Sx,Nm,Eqns).
collectClassRules([],[],_,[]).

isRuleForClass(labelRule(Lc,Nm,_,_,_,_),Lc,Nm).

collectEnumRules([Cl|Stmts],Sx,Nm,[Cl|Ex]) :-
  isRuleForEnum(Cl,_,Nm),!,
  collectEnumRules(Stmts,Sx,Nm,Ex).
collectEnumRules([Rl|Stmts],[Rl|Sx],Nm,Eqns) :-
  collectEnumRules(Stmts,Sx,Nm,Eqns).
collectEnumRules([],[],_,[]).

isRuleForEnum(labelRule(Lc,Nm,enum(_,_),_,_,_),Lc,Nm).

implementationGroup([(imp(Nm),_,[Stmt])],Defs,Dfs,E,Env,Path) :-
  buildImplementation(Stmt,Nm,Defs,Dfs,E,Env,Path).

buildImplementation(Stmt,INm,[Impl|Dfs],Dfs,Env,Ex,Path) :-
  isUnary(Stmt,Lc,"implementation",I),
  isBinary(I,"<=",Sq,Body),
  isBraceTuple(Body,_,Els),
  parseContractConstraint(Sq,Env,Nm,Spec),
  getContract(Nm,Env,contract(_,CNm,_,FullSpec,ConFace)),
  % We have to unify the implemented contract and the contract (spec)ification
  moveQuants(FullSpec,_,Qcon),
  moveConstraints(Qcon,OC,conTract(ConNm,OArgs,ODeps)),
  moveQuants(Spec,SQ,ASpec),
  moveConstraints(ASpec,AC,conTract(ConNm,AArgs,ADeps)),
  sameLength(OArgs,AArgs,Lc),
  sameLength(ODeps,ADeps,Lc),
  % match up the type variables of the original contract with the actual implemented contract
  bindAT(OArgs,AArgs,[],AQ),
  bindAT(ODeps,ADeps,AQ,QQ),
  rewriteConstraints(OC,QQ,[],OCx),% OCx will become additional contract requirements
  moveQuants(ConFace,_,Face),
  rewriteType(Face,QQ,EffType),
  pushScope(Env,E0),
  moveQuants(FaceType,SQ,EffType),
  evidence(FaceType,voidType,IQ,faceType(Fields)),
  declareTypeVars(IQ,Lc,E0,ThEnv),
  thetaEnv(Path,nullRepo,Lc,Els,Fields,ThEnv,OEnv,ThDefs,Public,_,Others),
  computeExport(ThDefs,Fields,Public,BodyDefs,Types,[],[]),
  implementationName(conTract(CNm,AArgs,ADeps),ImplName),
  Impl = implementation(Lc,INm,ImplName,Spec,OCx,AC,ThDefs,BodyDefs,Types,Others),
  declareImplementation(Nm,Impl,Env,Ex),
  dischargeConstraints(Env,OEnv),!.
buildImplementation(Stmt,_,Defs,Defs,Env,Env,_) :-
  locOfAst(Stmt,Lc),
  reportError("could not check implementation statement",[Lc]).

declImpl(imp(ImplNm,Spec),SoFar,[(ImplNm,Spec)|SoFar]).

typeOfTerm(V,_,Env,Env,v(Lc,N)) :-
  isIden(V,Lc,"_"),!,
  genstr("_",N).
typeOfTerm(V,Tp,Env,Ev,Term) :-
  isIden(V,Lc,N),
  isVar(N,Env,Spec),!,
  typeOfVar(Lc,N,Tp,Spec,Env,Ev,Term).
typeOfTerm(V,Tp,Ev,Env,v(Lc,N)) :-
  isIden(V,Lc,N),
  declareVar(N,vr(Lc,N,Tp),Ev,Env).
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
  isBinary(Term,Lc,":",L,R), !,
  parseType(R,Env,RT),
  checkType(Lc,RT,Tp,Env),
  typeOfTerm(L,RT,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isBinary(Term,Lc,"::",L,R), !,
  unary(Lc,"_coerce",L,LT),
  binary(Lc,":",LT,R,NT),
  typeOfTerm(NT,Tp,Env,Ev,Exp).
typeOfTerm(P,Tp,Env,Ex,where(Ptn,Cond)) :-
  isBinary(P,"@@",L,R),
  typeOfTerm(L,Tp,Env,E0,Ptn),
  checkCond(R,E0,Ex,Cond).
typeOfTerm(Call,Tp,Env,Ev,where(V,Cond)) :-
  isUnary(Call,Lc,"?",Test), % ?Test = NV @@ NV.Test where NV is a new name
  isRoundTerm(Test,_,_,_),
  genstr("_",NV),
  typeOfTerm(name(Lc,NV),Tp,Env,E0,V),
  binary(Lc,".",name(Lc,NV),Test,TT),
  checkCond(TT,E0,Ev,Cond).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isBinary(Term,Lc,".",L,F), !,
  isIden(F,Fld),
  recordAccessExp(Lc,L,Fld,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,conditional(Lc,Test,Then,Else)) :-
  isCondExpr(Term,Lc,Tst,Th,El),!,
  checkCond(Tst,Env,E0,Test),
  typeOfTerm(Th,Tp,E0,E1,Then),
  typeOfTerm(El,Tp,E1,Ev,Else).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isSquareTuple(Term,Lc,Els), !,
  checkSquareTuple(Lc,Els,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Env,theta(Path,Defs,Others,Types)) :-
  isBraceTuple(Term,Lc,Els),
  genstr("theta",Path),
  checkClassBody(Tp,Lc,Els,Env,Defs,Others,Types,Path).
typeOfTerm(tuple(_,"()",[Inner]),Tp,Env,Ev,Exp) :-
  \+ isTuple(Inner,_), !,
  typeOfTerm(Inner,Tp,Env,Ev,Exp).
typeOfTerm(tuple(Lc,"()",A),Tp,Env,Ev,tuple(Lc,Els)) :-
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
  typeOfKnown(F,FnTp,Env,E0,Fun),
  deRef(FnTp,FTp),
  typeOfCall(Lc,Fun,A,FTp,Tp,E0,Ev,Exp).
typeOfTerm(Term,Tp,Env,Ev,Exp) :-
  isSquareTerm(Term,Lc,F,[A]),!,
  typeOfIndex(Lc,F,A,Tp,Env,Ev,Exp).
typeOfTerm(Term,Tp,Env,Env,lambda(equation(Lc,"$",Args,Cond,Exp))) :-
  isBinary(Term,Lc,":-",Hd,C),
  isBinary(Hd,_,"=>",H,R),
  isTuple(H,_,A),
  genTpVars(A,AT),
  newTypeVar("_E",RT),
  checkType(Lc,funType(AT,RT),Tp,Env),
  typeOfTerms(A,AT,Env,E1,Lc,Args),
  checkCond(C,E1,E2,Cond),
  typeOfTerm(R,RT,E2,_,Exp).
typeOfTerm(Term,Tp,Env,Env,lambda(equation(Lc,"$",Args,true(Lc),Exp))) :-
  isBinary(Term,Lc,"=>",H,R),
  isTuple(H,_,A),
  genTpVars(A,AT),
  newTypeVar("_E",RT),
  checkType(Lc,funType(AT,RT),Tp,Env),
  typeOfTerms(A,AT,Env,E1,Lc,Args),
  typeOfTerm(R,RT,E1,_,Exp).
typeOfTerm(Term,Tp,Env,Env,lambda(clause(Lc,"$",Args,true(Lc),Body))) :-
  isBinary(Term,Lc,":-",H,R),
  isTuple(H,_,A),
  genTpVars(A,AT),
  checkType(Lc,predType(AT),Tp,Env),
  typeOfTerms(A,AT,Env,E1,Lc,Args),
  checkCond(R,E1,_,Body).
typeOfTerm(Term,Tp,Env,Env,void) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

typeOfCall(Lc,Fun,A,funType(ArgTps,FnTp),Tp,Env,Ev,apply(Fun,Args)) :-
  checkType(Lc,FnTp,Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Args).
typeOfCall(Lc,Fun,A,consType(ArgTps,FnTp),Tp,Env,Ev,apply(Fun,Args)) :-
  checkType(Lc,FnTp,Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Args). % small but critical difference

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

recordAccessExp(Lc,Rc,Fld,ET,Env,Ev,dot(Rec,Fld)) :-
  newTypeVar("_R",AT),
  typeOfKnown(Rc,AT,Env,Ev,Rec),
  getTypeFace(AT,Env,Face),
  moveConstraints(Face,_,faceType(Fields)),
  fieldInFace(Fields,AT,Fld,Lc,FTp),!,
  freshen(FTp,AT,[],_,Tp), % the record is this to the right of dot.
  checkType(Lc,Tp,ET,Env).

macroMapEntries(Lc,[],name(Lc,"_empty")).
macroMapEntries(_,[E|L],T) :-
  isBinary(E,Lc,"->",Ky,Vl),!,
  macroMapEntries(Lc,L,Tr),
  roundTerm(Lc,"_put",[Tr,Ky,Vl],T).
macroMapEntries(Lc,[E|L],T) :-
  reportError("invalid entry in map %s",[E],Lc),
  macroMapEntries(Lc,L,T).

fieldInFace(Fields,_,Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(_,Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfVar(Lc,Nm,Tp,vr(_,_,VT),Env,Ev,Exp) :-
  pickupThisType(Env,ThisType),
  freshen(VT,ThisType,_,VrTp),
  manageConstraints(VrTp,[],Lc,v(Lc,Nm),MTp,Exp,Env,Ev),
  checkType(Lc,MTp,Tp,Env).
typeOfVar(Lc,Nm,Tp,mtd(_,_,MTp),Env,Ev,Exp) :-
  pickupThisType(Env,ThisType),
  freshen(MTp,ThisType,[],_,VrTp),
  manageConstraints(VrTp,[],Lc,mtd(Lc,Nm),MtTp,Exp,Env,Ev),
  checkType(Lc,MtTp,Tp,Env).

manageConstraints(constrained(Tp,implementsFace(TV,Fc)),Cons,Lc,V,MTp,Exp,Env,Ev) :- !,
  declareConstraint(implementsFace(TV,Fc),Env,E0),
  manageConstraints(Tp,Cons,Lc,V,MTp,Exp,E0,Ev).
manageConstraints(constrained(Tp,Con),Cons,Lc,V,MTp,Exp,Env,Ev) :- !,
  manageConstraints(Tp,[Con|Cons],Lc,V,MTp,Exp,Env,Ev).
manageConstraints(Tp,[],_,V,Tp,V,Env,Env) :- !.
manageConstraints(Tp,RCons,Lc,V,Tp,over(Lc,V,Cons),Env,Env) :- reverse(RCons,Cons).

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
typeOfTerms([A|As],[ElTp|ElTypes],Env,Ev,_,[Term|Els]) :-
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
typeOfListTerm([Last],_,ElTp,ListTp,Env,Ev,apply(Op,[Hd,Tl])) :-
  isBinary(Last,Lc,",..",L,R),
  newTypeVar("_",LiTp),
  typeOfKnown(name(Lc,",.."),LiTp,Env,E0,Op),
  typeOfTerm(L,ElTp,E0,E1,Hd),
  typeOfTerm(R,ListTp,E1,Ev,Tl).
typeOfListTerm([El|More],_,ElTp,ListTp,Env,Ev,apply(Op,[Hd,Tl])) :-
  locOfAst(El,Lc),
  newTypeVar("_",LiTp),
  typeOfKnown(name(Lc,",.."),LiTp,Env,E0,Op),
  typeOfTerm(El,ElTp,E0,E1,Hd),
  typeOfListTerm(More,Lc,ElTp,ListTp,E1,Ev,Tl).

checkSequenceTerm(Lc,Els,Tp,Env,Ev,Exp) :-
  genIden(Lc,Seq),
  macroSequenceTerm(Els,Lc,Seq,Tsts),
  isTuple(Cond,Lc,Tsts),
  binary(Lc,"@@",Seq,Cond,Term),
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

checkCond(Term,Env,Env,true(Lc)) :-
  isIden(Term,Lc,"true") ,!.
checkCond(Term,Env,Env,false(Lc)) :-
  isIden(Term,Lc,"false") ,!.
checkCond(Term,Env,Ex,conj(Lhs,Rhs)) :-
  isBinary(Term,",",L,R), !,
  checkCond(L,Env,E1,Lhs),
  checkCond(R,E1,Ex,Rhs).
checkCond(Term,Env,Ex,conditional(Lc,Test,Either,Or)) :-
  isCondExpr(Term,Lc,T,Th,El),!,
  checkCond(T,Env,E0,Test),
  checkCond(Th,E0,E1,Either),
  checkCond(El,E1,Ex,Or).
checkCond(Term,Env,Ex,disj(Lc,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),!,
  checkCond(L,Env,E1,Either),
  checkCond(R,E1,Ex,Or).
checkCond(Term,Env,Env,neg(Lc,Test)) :-
  isUnary(Term,Lc,"\\+",N),!,
  checkCond(N,Env,_,Test).
checkCond(Term,Env,Ex,Cond) :-
  isTuple(Term,C),!,
  checkConds(C,Env,Ex,Cond).
checkCond(Term,Env,Ev,unify(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,"=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,match(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,".=",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(L,TV,Env,E0,Lhs),
  typeOfTerm(R,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,match(Lc,Rhs,Lhs)) :-
  isBinary(Term,Lc,"=.",L,R),!,
  newTypeVar("_#",TV),
  typeOfTerm(R,TV,Env,E0,Lhs),
  typeOfTerm(L,TV,E0,Ev,Rhs).
checkCond(Term,Env,Ev,phrase(Lc,NT,Strm,Rest)) :-
  isBinary(Term,Lc,"%%",L,R),
  isBinary(R,"~",S,M),!,
  newTypeVar("_S",StrmTp),
  newTypeVar("_E",ElTp),
  checkGrammarType(Lc,Env,StrmTp,ElTp),
  typeOfTerm(S,StrmTp,Env,E0,Strm),
  typeOfTerm(M,StrmTp,E0,E1,Rest),
  currentVar("stream",E1,OV),
  declareVar("stream",vr("stream",Lc,StrmTp),E1,E2),
  checkNonTerminal(L,StrmTp,ElTp,E2,E3,NT),
  restoreVar("stream",E3,OV,Ev).
checkCond(Term,Env,Ev,Goal) :-
  isBinary(Term,Lc,"%%",L,R),
  checkInvokeGrammar(Lc,L,R,Env,Ev,Goal).
checkCond(Term,Env,Ev,Call) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("_P",PrTp),
  typeOfKnown(F,PrTp,Env,E0,Pred),
  deRef(PrTp,PredTp),
  checkCondCall(Lc,Pred,A,PredTp,Call,E0,Ev).
checkCond(Term,Env,Ev,isTrue(Lc,Exp)) :-
  locOfAst(Term,Lc),
  findType("logical",Lc,Env,LogicalTp),
  typeOfKnown(Term,LogicalTp,Env,Ev,Exp).

checkCondCall(Lc,Pred,A,predType(ArgTps),Call,Env,Ev) :-
  checkCallArgs(Lc,Pred,A,ArgTps,Env,Ev,Call).
checkCondCall(Lc,Pred,_,Tp,true(Lc),Env,Env) :-
  reportError("type of %s:%s not a predicate",[Pred,Tp],Lc).

checkCallArgs(Lc,Pred,A,ArgTps,Env,Ev,call(Lc,Pred,Args)) :-
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Args).
checkCallArgs(Lc,Pred,A,ArgTps,Env,Env,true(Lc)) :-
  reportError("arguments %s of %s not consistent with expected types %s",[A,Pred,tupleType(ArgTps)],Lc).

checkInvokeGrammar(Lc,L,R,Env,Ev,phrase(Lc,NT,Strm)) :-
  newTypeVar("_S",StrmTp),
  newTypeVar("_E",ElTp),
  checkGrammarType(Lc,Env,StrmTp,ElTp),
  typeOfTerm(R,StrmTp,Env,E1,Strm),
  binary(Lc,",",L,name(Lc,"eof"),Phrase),
  currentVar("stream",E1,OV),
  declareVar("stream",vr("stream",Lc,StrmTp),E1,E2),
  checkNonTerminal(Phrase,StrmTp,ElTp,E2,E3,NT),
  restoreVar("stream",E3,OV,Ev).

checkGrammarType(Lc,Env,Tp,ElTp) :-
  getContract("stream",Env,contract(_,_,Spec,_,_)),
  pickupThisType(Env,ThisType),
  freshenContract(Spec,ThisType,_,conTract(_,[Arg],[Dep])),
  checkType(Lc,Arg,Tp,Env),
  checkType(Lc,Dep,ElTp,Env).

checkConds([C],Env,Ex,Cond) :-
  checkCond(C,Env,Ex,Cond).
checkConds([C|More],Env,Ex,conj(L,R)) :-
  checkCond(C,Env,E0,L),
  checkConds(More,E0,Ex,R).

processGrammarRule(Lc,L,P,R,grammarType(AT,Tp),[grammarRule(Lc,Nm,Args,PB,Body)|Defs],Defs,E) :-
  splitHead(L,Nm,A),
  pushScope(E,E0),
  declareVar("stream",vr("stream",Lc,Tp),E0,E1),
  newTypeVar("_E",ElTp),
  typeOfTerms(A,AT,E1,E2,Lc,Args),!,
  checkNonTerminal(R,Tp,ElTp,E2,E3,Body),
  checkTerminals(P,"_hdtl",PB,ElTp,E3,E4),
  dischargeConstraints(E,E4).

checkNonTerminal(tuple(Lc,"[]",Els),_,ElTp,E,Env,terminals(Lc,Terms)) :- !,
  checkTerminals(Els,"_hdtl",Terms,ElTp,E,Env).
checkNonTerminal(string(Lc,Text),_,ElTp,Env,Env,terminals(Lc,Terms)) :- !,
  explodeStringLit(Lc,Text,IntLits),
  checkTerminals(IntLits,"_hdtl",Terms,ElTp,Env,_).  % strings are exploded into code points
checkNonTerminal(tuple(Lc,"()",NT),Tp,ElTp,Env,Ex,GrNT) :-
  checkNonTerminals(NT,Lc,Tp,ElTp,Env,Ex,GrNT).
checkNonTerminal(Term,Tp,ElTp,Env,Ex,conj(Lc,Lhs,Rhs)) :-
  isBinary(Term,Lc,",",L,R), !,
  checkNonTerminal(L,Tp,ElTp,Env,E1,Lhs),
  checkNonTerminal(R,Tp,ElTp,E1,Ex,Rhs).
checkNonTerminal(Term,Tp,ElTp,Env,Ex,conditional(Lc,Test,Either,Or)) :-
  isBinary(Term,Lc,"|",L,R),
  isBinary(L,"?",T,Th),!,
  checkNonTerminal(T,Tp,ElTp,Env,E0,Test),
  checkNonTerminal(Th,Tp,ElTp,E0,E1,Either),
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
checkNonTerminal(Term,_,_,Env,Env,eof(Lc,Op)) :-
  isIden(Term,Lc,"eof"),
  unary(Lc,"_eof",name(Lc,"stream"),EO),
  checkCond(EO,Env,_,call(_,Op,_)).
checkNonTerminal(Term,_,_,Env,Ex,goal(Lc,Cond)) :-
  isBraceTuple(Term,Lc,Els),
  checkConds(Els,Env,Ex,Cond).

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
checkTerminals([T|More],V,[term(Lc,Op,TT)|Out],ElTp,Env,Ex) :-
  locOfAst(T,Lc),
  ternary(Lc,V,name(Lc,"stream"),T,name(Lc,"stream"),C),
  checkCond(C,Env,E1,call(_,Op,[_,TT,_])),
  checkTerminals(More,V,Out,ElTp,E1,Ex).

computeExport([],_,_,[],[],[],[]).
computeExport([Def|Defs],Fields,Public,Exports,Types,Contracts,Impls) :-
  exportDef(Def,Fields,Public,Exports,Ex,Types,Tx,Contracts,Cx,Impls,Ix),!,
  computeExport(Defs,Fields,Public,Ex,Tx,Cx,Ix).

exportDef(function(_,Nm,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(predicate(_,Nm,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(class(_,Nm,Tp,_,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(enum(_,Nm,Tp,_,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(typeDef(_,Nm,_,FaceRule),_,Public,Exports,Exports,[(Nm,FaceRule)|Tx],Tx,Cons,Cons,Impl,Impl) :-
  isPublicType(Nm,Public).
exportDef(defn(_,Nm,_,_,Tp,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(grammar(_,Nm,Tp,_,_),Fields,Public,[(Nm,Tp)|Ex],Ex,Types,Types,Cons,Cons,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(Con,_,Public,Ex,Ex,Types,Types,[Con|Cons],Cons,Impl,Impl) :-
  isPublicContract(Con,Public).
exportDef(impl(_,INm,ImplName,_,Spec,_,_,_,_),_,Public,Ex,Ex,Tps,Tps,Cons,Cons,[imp(ImplName,Spec)|Ix],Ix) :-
  is_member(imp(INm),Public),!.
exportDef(_,_,_,Ex,Ex,Tps,Tps,Cons,Cons,Impls,Impls).

isPublicVar(Nm,_,Public) :-
  is_member(var(Nm),Public),!.
isPublicVar(Nm,Fields,_) :-
  is_member((Nm,_),Fields),!.

isPublicType(Nm,Public) :-
  is_member(tpe(Nm),Public),!.

isPublicContract(contract(Nm,_,_,_,_),Public) :-
  is_member(con(Nm),Public),!.

matchTypes(type(Nm),type(Nm),[]) :-!.
matchTypes(typeExp(T1,L),typeExp(T2,R),Binding) :-
  matchTypes(T1,T2,Binding),
  matchArgTypes(L,R,Binding).

matchArgTypes([],[],[]).
matchArgTypes([kVar(Nm)|L],[kVar(Nm)|R],Binding) :- !,
  matchArgTypes(L,R,Binding).
matchArgTypes([Tp|L],[kVar(Ot)|R],[(Ot,Tp)|Binding]) :-
  matchArgTypes(L,R,Binding).

generateClassFace(Tp,Env,Face) :-
  freshen(Tp,voidType,[],Q,FTp),
  moveConstraints(FTp,Cx,Plate),
  (Plate = consType(_,T); T=Plate),
  getTypeFace(T,Env,F),
  moveConstraints(ClTp,Cx,F),
  freezeType(ClTp,Q,Face),!.

dischargeConstraints(_,_).
