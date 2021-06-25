:- module(checker,[checkProgram/5]).

:- use_module(abstract).
:- use_module(astdisp).
:- use_module(display).
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
:- use_module(operators).
:- use_module(macro).
:- use_module(import).
:- use_module(transitive).
:- use_module(resolve).
:- use_module(cnc).
:- use_module(vartypes).

checkProgram(Prog,Pkg,Repo,Opts,Canon) :-
  stdDict(Base),
%  dispEnv(Base),
  isBraceTerm(Prog,Lc,_,El0),
  build_main(El0,Els),
  Pkg = pkg(Pk,_),
  collectImports(Els,Imports,Stmts),
  importAll(Imports,Repo,AllImports),
  importAllDefs(AllImports,Repo,Base,Env0),
  dispEnv(Env0),
  thetaEnv(Pk,Lc,Stmts,faceType([],[]),Env0,OEnv,Defs,Public),
%  dispEnv(OEnv),
  overload(Lc,Defs,OEnv,_Dict,ODefs),
  completePublic(Public,Public,FllPb,Pk),
  packageExport(ODefs,FllPb,ExportDecls,LDecls,XDefs),
  mkBoot(OEnv,Lc,Pk,XDefs,PkgDefs),
  Canon=prog(Pkg,Imports,ExportDecls,LDecls,PkgDefs),
  (is_member(showTCCode,Opts) -> displayln(canon:ssCanonProg(Canon));true).

findExportedDefs(Lc,Flds,Els) :-
  map(Flds,checker:mkFieldArg(Lc),Els).

mkFieldArg(Lc,(Nm,Tp),v(Lc,Nm,Tp)).

thetaEnv(Pkg,Lc,Stmts,Fields,Base,TheEnv,Defs,Public) :-
  collectDefinitions(Stmts,Dfs,Public,Annots),
  dependencies(Dfs,Groups,Annots),
  pushFace(Fields,Lc,Base,Env),
  checkGroups(Groups,Fields,Annots,Defs,[],Env,TheEnv,Pkg).

recordEnv(Path,_Lc,Stmts,Fields,Base,TheEnv,Defs,Public) :-
  collectDefinitions(Stmts,Dfs,Public,Annots),
  parseAnnotations(Dfs,Fields,Annots,Base,Path,Face),
  checkGroup(Dfs,Defs,[],Base,TheEnv,Face,Path).

collectDefinitions([St|Stmts],Defs,P,A) :-
  collectDefinition(St,Stmts,S0,Defs,D0,P,P0,A,A0,checker:nop),
  collectDefinitions(S0,D0,P0,A0).
collectDefinitions([],[],[],[]).

collectDefinition(St,Stmts,Stmts,[(cns(V),Lc,[Tp])|Defs],Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  (isIden(L,V),Ex=Export; isPrivate(L,_,V1),isIden(V1,V),Ex=checker:nop),
  isConstructorType(T,_,Tp),!,
  call(Ex,var(V),P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  (isIden(L,V) ->
   call(Export,var(V),P,Px) ;
   isPrivate(L,_,V1), isIden(V1,V) ->
   call(checker:nop,var(V),P,Px);
   reportError("cannot understand type annotation %s",[ast(St)],Lc),
   P=Px).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,P,A,Ax,_) :-
  isPrivate(St,_,Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,_,A,Ax,checker:nop).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,_) :-
  isPublic(St,_,Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,checker:export).
collectDefinition(St,Stmts,Stmts,[(con(Nm),Lc,[St])|Defs],Defs,P,Px,A,Ax,Export) :-
  isContractStmt(St,Lc,Quants,Constraints,Con,Els),
  generateAnnotations(Els,Quants,[Con|Constraints],A,Ax),
  isSquare(Con,Nm,_),
  call(Export,con(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,[(imp(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isImplementationStmt(St,Lc,_,_,N,_),
  implementedContractName(N,Nm),
  call(Export,imp(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,Px,Px,A,A,_) :-
  isBinary(St,_,"@",_,_).
collectDefinition(St,Stmts,Stmts,Defs,Defs,Px,Px,A,A,_) :-
  isUnary(St,_,"@",_).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isTypeExistsStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,tpe(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isTypeFunStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,tpe(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Dfx,P,Px,A,Ax,Export) :-
  isAlgebraicTypeStmt(St,Lc,Quants,Constraints,Head,Body),
  typeName(Head,Nm),
  call(Export,tpe(Nm),P,P0),
  collectConstructors(Body,Quants,Constraints,Head,Defs,Dfx,P0,Px,A,Ax,Export).
collectDefinition(St,Stmts,Stx,[(Nm,Lc,[St|Defn])|Defs],Defs,P,Px,A,A,Export) :-
  ruleName(St,Nm,Kind),
  locOfAst(St,Lc),
  collectDefines(Stmts,Kind,Stx,Nm,Defn),
  call(Export,Nm,P,Px).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,P,A,A,_) :-
  locOfAst(St,Lc),
  reportError("Cannot fathom %s",[St],Lc).

collectDefines([St|Stmts],Kind,OSt,Nm,[St|Defn]) :-
  ruleName(St,Nm,Kind),
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines([St|Stmts],Kind,[St|OSt],Nm,Defn) :-
  collectDefines(Stmts,Kind,OSt,Nm,Defn).
collectDefines(Stmts,_,Stmts,_,[]).

% project contract members out
generateAnnotations([],_,_,Ax,Ax).
generateAnnotations([Def|Els],Quants,Constraints,[(Nm,MTp)|A],Ax) :-
  isTypeAnnotation(Def,_,N,Tp),
  isIden(N,_,Nm),
  reConstrain(Constraints,Tp,CTp),
  reUQuant(Quants,CTp,MTp),
  generateAnnotations(Els,Quants,Constraints,A,Ax).
generateAnnotations([_|Els],Quants,Constraints,A,Ax) :- % ignore things like assertions
  generateAnnotations(Els,Quants,Constraints,A,Ax).

export(Nm,[Nm|P],P).
nop(_,P,P).

collectConstructors(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,Export) :-
  isBinary(C,_,"|",L,R),!,
  collectConstructors(L,Quants,Constraints,Tp,Defs,Df0,P,P0,A,A0,Export),
  collectConstructors(R,Quants,Constraints,Tp,Df0,Dfx,P0,Px,A0,Ax,Export).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Enm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isIden(C,Lc,Enm),!,
  roundTuple(Lc,[],Hd),
  binary(Lc,"<=>",Hd,Tp,CnTp),
  reConstrain(Constraints,CnTp,Rl),
  reUQuant(Quants,Rl,St),
  call(Export,var(Enm),P,Px).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Enm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isEnum(C,Lc,E),
  isIden(E,_,Enm),!,
  roundTuple(Lc,[],Hd),
  binary(Lc,"<=>",Hd,Tp,CnTp),
  reConstrain(Constraints,CnTp,Rl),
  reUQuant(Quants,Rl,St),
  call(Export,var(Enm),P,Px).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Nm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isRoundCon(C,_,_,Lc,Nm,Args),!,
  call(Export,var(Nm),P,Px),
  roundTuple(Lc,Args,Hd),
  binary(Lc,"<=>",Hd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St),
  call(Export,var(Nm),P,Px).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Nm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isBraceCon(C,XQ,XC,Lc,Nm,Els),!,
  call(Export,var(Nm),P,Px),
  pullOthers(Els,Entries,_Asserts,_Defaults),
  braceTuple(Lc,Entries,Hd),
  reConstrain(XC,Hd,XHd),
  reXQuant(XQ,XHd,QHd),
  binary(Lc,"<=>",QHd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St).
collectConstructor(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,_Export) :-
  isPrivate(C,_,I),
  collectConstructor(I,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,checker:nop).

pullOthers([],[],[],[]).
pullOthers([St|Els],Entries,[St|Asserts],Deflts) :-
  isIntegrity(St,_,_),!,
  pullOthers(Els,Entries,Asserts,Deflts).
pullOthers([St|Els],Entries,Asserts,[St|Deflts]) :-
  isDefault(St,_,_,_),!,
  pullOthers(Els,Entries,Asserts,Deflts).
pullOthers([St|Els],[St|Entries],Asserts,Deflts) :-
  pullOthers(Els,Entries,Asserts,Deflts).

declareImportedVar(Lc,Nm,pkg(Pkg,_),PkgVr,_Enums,Tp,Env,E0) :-
  consName(Pkg,Nm,LclNm),
  (isConType(Tp,Ar) ->
   (Ar=0 ->
    declareEnum(Lc,Nm,LclNm,Tp,Env,E0) ;
    declareCns(Lc,Nm,LclNm,Tp,Env,E0)) ;
   declareField(Lc,PkgVr,Nm,Tp,Env,E0)).

importTypes([],_,Env,Env).
importTypes([(Nm,Rule)|More],Lc,Env,Ex) :-
%  reportMsg("import type %s with rule %s",[Nm,Rule]),
  typeTemplate(Nm,Rule,Type),
  declareType(Nm,tpDef(Lc,Type,Rule),Env,E0),
  importTypes(More,Lc,E0,Ex).

importAllDefs([],_,Env,Env).
importAllDefs([importPk(Lc,_Viz,Pkg)|More],Repo,Env,Ex) :-
  importPkg(Pkg,Repo,spec(_,_,Decls)),
  rfold(Decls,checker:importDecl(Lc),Env,Ev0),
  importAllDefs(More,Repo,Ev0,Ex).

importDecl(Lc,impDec(ImplNm,FullNm,ImplTp),Ev,Evx) :-
  declareVr(Lc,FullNm,ImplTp,Ev,Ev0),
  declareImplementation(ImplNm,FullNm,ImplTp,Ev0,Evx).
importDecl(_,accDec(Tp,Fld,FnNm,AccTp),Ev,Evx) :-
  declareFieldAccess(Tp,Fld,FnNm,AccTp,Ev,Evx).
importDecl(Lc,contractDec(Nm,CnNm,CnTp,Rule),Ev,Evx) :-
  defineContract(Nm,Lc,conDef(Nm,CnNm,CnTp,Rule),Ev,Evx).
importDecl(Lc,typeDec(Nm,Tp,Rule),Env,Evx) :-
  declareType(Nm,tpDef(Lc,Tp,Rule),Env,Evx).
importDecl(Lc,varDec(Nm,_FullNm,Tp),Env,Evx) :-
  declareVr(Lc,Nm,Tp,Env,Evx).
importDecl(Lc,cnsDec(Nm,FullNm,Tp),Env,Evx) :-
  (isConType(Tp,0) ->
   declareEnum(Lc,Nm,FullNm,Tp,Env,Evx) ;
   declareCns(Lc,Nm,FullNm,Tp,Env,Evx)).
importDecl(Lc,funDec(Nm,Tp),Env,Evx) :-
  declareVr(Lc,Nm,Tp,Env,Evx).
importDecl(Lc,Entry,Env,Env) :-
  reportError("(internal) cannot figure out import entry %s",[Entry],Lc).

importContracts([],_,Env,Env).
importContracts([C|L],Lc,E,Env) :-
  C = conDef(Nm,_,_,_),
  defineContract(Nm,Lc,C,E,E0),
  importContracts(L,Lc,E0,Env).

notAlreadyImported(import(_,_,Pkg),SoFar) :-
  \+ is_member(import(_,_,Pkg),SoFar),!.

checkGroups([],_,_,Defs,Defs,E,E,_).
checkGroups([Gp|More],Fields,Annots,Defs,Dfx,E,Ev,Path) :-
  parseAnnotations(Gp,Fields,Annots,E,Path,Face),!,
  groupLc(Gp,Lc),
  pushFace(Face,Lc,E,E0),
  checkGroup(Gp,Defs,Df2,E0,E3,Face,Path),!,
  checkGroups(More,Fields,Annots,Df2,Dfx,E3,Ev,Path).

groupLc([(_,Lc,_)|_],Lc).

checkGroup([(con(N),Lc,[ConStmt])|More],[Contract|Defs],Dx,Env,Ex,Face,Path) :-
  parseContract(ConStmt,Env,E0,Path,[Contract|Defs],Df0),
  defineContract(N,Lc,Contract,E0,E1),
  checkGroup(More,Df0,Dx,E1,Ex,Face,Path).
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
checkGroup([(open(_),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Path) :-
  checkOpenStmt(Stmt,Env,Ev0,Path),
  checkGroup(More,Defs,Dx,Ev0,Ex,Face,Path).
checkGroup([],Defs,Defs,Env,Env,_,_).

defineContract(N,Lc,Contract,E0,Ex) :-
  declareContract(N,Contract,E0,E1),
  declareMethods(Contract,Lc,E1,Ex).

declareMethods(conDef(_,_,_,ConEx),Lc,Env,Ev) :-
  moveQuants(ConEx,Q,C1),
  getConstraints(C1,Cx,contractExists(CTract,faceType(Methods,[]))),
  formMethods(Methods,Lc,Q,Cx,CTract,Env,Ev).

formMethods([],_,_,_,_,Env,Env).
formMethods([(Nm,Tp)|M],Lc,Q,Cx,Con,Env,Ev) :-
  moveQuants(Tp,FQ,QTp),
  merge(FQ,Q,MQ),
  putConstraints(Cx,constrained(QTp,Con),CC),
  moveQuants(MTp,MQ,CC),
  declareMtd(Lc,Nm,MTp,Env,E0),
  formMethods(M,Lc,Q,Cx,Con,E0,Ev).

parseConstructor(Nm,Lc,T,Env,Ev,[cnsDef(Lc,Nm,ConVr)|Defs],Defs,Path) :-
  parseType(T,Env,Tp),
  localName(Path,class,Nm,FullNm),
  (isConType(Tp,0) ->
   declareEnum(Lc,Nm,FullNm,Tp,Env,Ev), ConVr=enm(Lc,FullNm,Tp) ;
   declareCns(Lc,Nm,FullNm,Tp,Env,Ev), ConVr=cons(Lc,FullNm,Tp)).

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
  isVar(N,Env,vrEntry(_,_,Tp)),!.
parseAnnotation(N,Lc,Stmts,_,_,_,F,[(N,Tp)|F]) :-
  guessStmtType(Stmts,N,Lc,Tp).
parseAnnotation(_,_,_,_,_,_,Face,Face).

defineType(N,_,_,Env,T,[(N,Tp)|T],_) :-
  isType(N,Env,tpDef(_,Tp,_)),!.
defineType(N,_,St,Env,T,[(N,Type)|T],Path) :-
  parseTypeCore(St,Type,Env,Path).
defineType(_,Lc,St,_,T,T,_) :-
  reportError("cannot parse type statement %s",[St],Lc).

parseTypeAnnotation(N,_,faceType(_,Types),_,_,F,[(N,Tp)|F]) :-
  is_member((N,Tp),Types),!.
parseTypeAnnotation(N,Lc,_,_,_,Face,Face) :-
  reportError("no type annotation for variable %s",[N],Lc).

checkOpenStmt(Stmt,Env,Ev,Path) :-
  isOpen(Stmt,Lc,Vr),!,
  newTypeVar("_",VrTp),
  typeOfExp(Vr,VrTp,Env,E0,Rc,Path),
  faceOfType(VrTp,E0,faceType(Flds,Tps)),
  declareExported(Flds,Rc,Lc,Env,E1),
  declareExportedTypes(Tps,Rc,Lc,E1,Ev).

declareExported([],_,_,Env,Env).
declareExported([(Nm,Tp)|More],Rc,Lc,Env,Ev) :-
  declareField(Lc,Rc,Nm,Tp,Env,Ev0),
  declareExported(More,Rc,Lc,Ev0,Ev).

declareExportedTypes(_,_,_,Env,Env).

checkVarRules(N,Lc,Stmts,E,Ev,Defs,Dx,Face,Path) :-
  pickupDefnType(N,Lc,Face,Stmts,E,E0,Tp),
  evidence(Tp,E,Q,ETp),
  getConstraints(ETp,Cx,ProgramType),
  declareTypeVars(Q,Lc,E0,E1),
  declareConstraints(Lc,Cx,E1,E2),
  processStmts(Stmts,ProgramType,Rules,Deflts,Deflts,[],E2,Path),
  packageVarName(Path,N,LclName),
  formDefn(Rules,N,LclName,E,Ev,Tp,Cx,Defs,Dx).
%  reportMsg("type of %s:%s",[N,ProgramType]).

formDefn([Eqn|Eqns],Nm,LclNm,Env,Ev,Tp,Cx,[funDef(Lc,Nm,LclNm,Tp,Cx,[Eqn|Eqns])|Dx],Dx) :-
  Eqn = equation(Lc,_,_,_),
  declareVr(Lc,Nm,Tp,Env,Ev).
formDefn([varDef(Lc,_,_,_,_,Value)],Nm,LclNm,Env,Ev,Tp,Cx,
    [varDef(Lc,Nm,LclNm,Cx,Tp,Value)|Dx],Dx) :-
  declareVr(Lc,Nm,Tp,Env,Ev).

processStmts([],_,Defs,Defs,Dflts,Dflts,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Df,Dfx,E0,Path) :-
  processStmt(St,ProgramType,Defs,D0,Df,Df0,E0,Path),!,
  processStmts(More,ProgramType,D0,Dx,Df0,Dfx,E0,Path).

processStmt(St,ProgramType,Defs,Defx,Df,Dfx,E,Path) :-
  isEquation(St,Lc,L,Cond,R),!,
%  reportMsg("check equation %s",[St],Lc),
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

pickupDefnType(N,_,faceType(F,_),_,Ev,Ev,Tp) :-
  is_member((N,Tp),F),!.
pickupDefnType(Nm,Lc,_,Stmts,E,Ev,Tp) :-
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

findType(Nm,_,Env,Tp) :-
  isType(Nm,Env,tpDef(_,Tp,_)),!.
findType(Nm,Lc,_,anonType) :-
  reportError("type %s not known",[Nm],Lc).

checkEquation(Lc,H,C,R,funType(AT,RT),Defs,Defsx,Df,Dfx,E,Path) :-
  splitHead(H,_,A,IsDeflt),
  pushScope(E,Env),
  typeOfArgPtn(A,AT,Env,E0,Args,Path),
  checkGuard(C,E0,E1,Guard,Path),
  typeOfExp(R,RT,E1,_E2,Exp,Path),
  processIterable(Env,Path,Exp,Reslt),
  Eqn = equation(Lc,Args,Guard,Reslt),
%  reportMsg("equation %s",[Eqn],Lc),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[Eqn|Dfx]; Defs=[Eqn|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("equation not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,Tp,varDef(Lc,Nm,ExtNm,[],Tp,Reslt),Env,Path) :-
  isIden(L,_,Nm),
  pushScope(Env,E),
  typeOfExp(R,Tp,E,_E2,Value,Path),
  processIterable(Env,Path,Value,Reslt),
  packageVarName(Path,Nm,ExtNm).

checkVarDefn(Lc,L,R,refType(Tp),[varDef(Lc,Nm,ExtNm,[],refType(Tp),cell(Lc,Reslt))|Defs],Defs,Env,Path) :-
  isIden(L,_,Nm),
  pushScope(Env,E1),
  typeOfExp(R,Tp,E1,_E2,Value,Path),
  processIterable(Env,Path,Value,Reslt),
  packageVarName(Path,Nm,ExtNm).
checkVarDefn(Lc,L,_,Tp,Defs,Defs,_,_) :-
  reportError("expecting an assignable type, not %s for %s",[Tp,L],Lc).

checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  thetaEnv(Path,Lc,Els,Face,BaseEnv,_,Defs,Public),
  faceOfType(ETp,Env,TpFace),
  getConstraints(TpFace,_,faceType(Fs,_)),
  completePublic(Public,Public,FullPublic,Path),
  computeThetaExport(Defs,Fs,FullPublic,Decls,Defns),!,
  formTheta(Lc,Lbl,Decls,Defns,Fs,Tp,Val).

formTheta(Lc,Lbl,Decls,Defs,Flds,Tp,letRec(Lc,Decls,Defs,Exp)) :-
  sort(Flds,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,Flds,Args),
  project1(SortedFlds,ElTps),
  Exp = apply(Lc,cons(Lc,Lbl,consType(tupleType(ElTps),Tp)),
	      tple(Lc,Args),Tp).

checkRecordBody(Tp,Lbl,Lc,Els,Env,Val,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  getConstraints(FaceTp,Cx,faceType(Fs,Ts)),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  recordEnv(Path,Lc,Els,faceType(Fs,Ts),BaseEnv,_,Defs,Public),
  completePublic(Public,Public,FullPublic,Path),
  computeThetaExport(Defs,Fs,FullPublic,Decls,Defns),!,
  formRecord(Lc,Lbl,Decls,Defns,Fs,Tp,Val).

formRecord(Lc,Lbl,Decls,Defs,Flds,Tp,letExp(Lc,Decls,Defs,Exp)) :-
  sort(Flds,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,Flds,Args),
  project1(SortedFlds,ElTps),
  Exp = apply(Lc,cons(Lc,Lbl,consType(tupleType(ElTps),Tp)),
	      tple(Lc,Args),Tp).

checkLetRec(Tp,Lc,Els,Ex,Env,letRec(Lc,Decls,XDefs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  computeLetExport(Defs,[],Decls,XDefs),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path).

checkLetExp(Tp,Lc,Els,Ex,Env,letExp(Lc,Decls,XDefs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  computeLetExport(Defs,[],Decls,XDefs),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path).

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
splitHd(Id,Nm,none,notDeflt) :-
  isIden(Id,_,Nm),!.
splitHd(Term,"()",Term,notDeflt) :-
  isTuple(Term,_).

checkImplementation(Stmt,INm,[Impl,ImplVar|Dfs],Dfs,Env,Evx,_,Path) :-
  isImplementationStmt(Stmt,Lc,Quants,Cons,Sq,IBody),
  parseContractConstraint(Quants,Cons,Sq,Env,ConNm,ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
  getConstraints(CnSpec,AC,contractExists(Spec,_)),
  declareTypeVars(IQ,Lc,Env,E1),
  declareConstraints(Lc,AC,E1,ThEnv),
  implementationName(Spec,ImplName),
  localName(Path,value,ImplName,ImplVrNm),
  contractType(Spec,CnType),
  labelImplExp(IBody,ConNm,ImpBody),
%  reportMsg("implementation %s, expected type %s",[ast(ImpBody),tpe(CnType)]),
  typeOfExp(ImpBody,CnType,ThEnv,_ThEv,ImplTerm,ImplName),
  putConstraints(AC,CnType,SS1),
  reQuantTps(SS1,IQ,ImpType),
  ImplVar = varDef(Lc,ImplVrNm,ImplVrNm,AC,ImpType,ImplTerm),
  Impl = implDef(INm,ImplName,ImplVrNm,ImpType),
  declareVr(Lc,ImplVrNm,ImpType,Env,Ev0),
  declareImplementation(ImplName,ImplVrNm,ImpType,Ev0,Evx),!.
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_) :-
  locOfAst(Stmt,Lc),
  reportError("could not check implementation statement %s",[Stmt],Lc).

labelImplExp(Trm,Nm,Term) :-
  isBraceTuple(Trm,Lc,Els),
  braceTerm(Lc,name(Lc,Nm),Els,Term).
labelImplExp(Trm,Nm,Term) :-
  isQBraceTuple(Trm,Lc,Els),
  qbraceTerm(Lc,name(Lc,Nm),Els,Term).
labelImplExp(Trm,Nm,Term) :-
  isLetDef(Trm,Lc,Els,Ex),
  labelImplExp(Ex,Nm,IExp),
  mkLetDef(Lc,Els,IExp,Term).
labelImplExp(Trm,Nm,Term) :-
  isLetRec(Trm,Lc,Els,Ex),
  labelImplExp(Ex,Nm,IExp),
  mkLetRec(Lc,Els,IExp,Term).
labelImplExp(Trm,_,Trm) :-
  locOfAst(Trm,Lc),
  reportError("invalid form of implementation %s",[Trm],Lc).

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
  isVar(N,Env,_),!,
  mkWhereEquality(Lc,V,TT),
  typeOfPtn(TT,Tp,Env,Ev,Term,Path).
typeOfPtn(V,Tp,Ev,Env,v(Lc,N,Tp),_Path) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,Ev,Env).
typeOfPtn(Trm,Tp,Env,Ev,Term,Path) :-
  isEnum(Trm,_,_),
  typeOfExp(Trm,Tp,Env,Ev,Term,Path).
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
  checkGuard(some(C),E0,Ex,some(Cond),Path).
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
  typeOfExp(F,consType(At,Tp),Env,E0,Fun,Path),
  evidence(At,E0,_,AT),
  typeOfArgPtn(tuple(Lc,"()",A),AT,E0,Ev,Args,Path),
  Exp = apply(Lc,Fun,Args,Tp).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  (isBraceTerm(Term,Lc,F,Args);isQBraceTerm(Term,Lc,F,Args)),
  typeOfRecordPtn(Lc,Tp,F,Args,Env,Ev,Exp,Path).
typeOfPtn(Term,Tp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal pattern: %s, expecting a %s",[Term,Tp],Lc).

typeOfRecordPtn(Lc,Tp,F,Args,Env,Ev,Exp,Path) :-
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(E0,E1),
  declareTypeVars(Q,Lc,E1,E2),
  declareConstraints(Lc,Cx,E2,BaseEnv),
  typeOfElementPtns(Args,Face,BaseEnv,Ev,PtnDefs,[],Path),
  fillinElementPtns(PtnDefs,Lc,FaceTp,ArgPtns),
  Exp = apply(Lc,Fun,tple(Lc,ArgPtns),Tp).

typeOfElementPtns([],_,Env,Env,Defs,Defs,_Path).
typeOfElementPtns([E|Els],Face,Env,Ev,Defs,Dfx,Path) :-
  elementPtn(E,Face,Env,E0,Defs,Df0,Path),
  typeOfElementPtns(Els,Face,E0,Ev,Df0,Dfx,Path).

elementPtn(E,Face,Env,Ev,[(Nm,Ptn)|Defs],Defs,Path) :-
  isDefn(E,Lc,Lhs,Rhs),
  isIden(Lhs,_,Nm),
  fieldInFace(Face,Nm,Lc,Tp),
  typeOfPtn(Rhs,Tp,Env,Ev,Ptn,Path).

fillinElementPtns(Els,Lc,faceType(Flds,_),Args) :-
  rfold(Flds,checker:fillinElementPtn(Lc),Els,NEls),
  sort(NEls,checker:cmpPair,Elements),
  project1(Elements,Args).

cmpPair((N1,_),(N2,_)) :-
  str_lt(N1,N2).

fillinElementPtn(_,(Nm,_),Els,Els) :-
  is_member((Nm,_),Els) ,!.
fillinElementPtn(Lc,(Nm,Tp),Els,[(Nm,v(Lc,"_",Tp))|Els]).
  
typeOfArgTerm(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  checkType(T,tupleType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgTerm(T,Tp,Env,Ev,Exp,Path) :-
  typeOfExp(T,Tp,Env,Ev,Exp,Path).

typeOfExp(V,Tp,Env,Ev,Term,_Path) :-
  isIden(V,Lc,N),!,
  (getVar(Lc,N,Env,Ev,Term) ->
   typeOfCanon(Term,VTp),
   checkType(V,Tp,VTp,Ev);
   reportError("variable '%s' not defined, expecting a %s",[V,Tp],Lc),
   Term=void).
typeOfExp(V,Tp,Env,Ev,Term,_Path) :-
  isIden(V,Lc,N),!,
  (isVar(N,Env,Spec) ->
   typeOfVar(Lc,N,Tp,Spec,Env,Ev,Term);
   reportError("variable '%s' not defined, expecting a %s",[V,Tp],Lc),
   Term=void,
   Env=Ev).
typeOfExp(T,Tp,Env,Ev,Term,Path) :-
  isEnum(T,_,N),
  typeOfExp(N,Tp,Env,Ev,Term,Path),!.
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
  unary(Lc,"_optval",LT,OLT),
  binary(Lc,":",OLT,R,NT),
  typeOfExp(NT,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isOptCoerce(Term,Lc,L,R),!,
  unary(Lc,"_coerce",L,LT),
  sqUnary(Lc,"option",R,OR),
  binary(Lc,":",LT,OR,NT),
  typeOfExp(NT,Tp,Env,Ev,Exp,Path).
typeOfExp(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),!,
  reportError("Unexpected where `%s' in expression",[ast(P)],Lc),
  typeOfExp(L,Tp,Env,E0,Ptn,Path),
  checkGuard(some(C),E0,Ex,some(Cond),Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  recordAccessExp(Lc,Rc,Fld,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,replace(Lc,LExp,RExp),Path) :-
  isRecordUpdate(Term,Lc,Lft,Rgt),!,
  typeOfExp(Lft,Tp,Env,Ev,LExp,Path),
  newTypeVar("_r",R),
  typeOfExp(Rgt,R,Env,_,RExp,Path).
typeOfExp(Term,Tp,Env,Ev,cond(Lc,Test,Then,Else,Tp),Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  checkGoal(Tst,Env,E0,Test,Path),
  typeOfExp(Th,Tp,E0,E1,Then,Path),
  typeOfExp(El,Tp,Env,E2,Else,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isCaseExp(Term,Lc,Bnd,Cases),
  checkCaseExp(Lc,Bnd,Cases,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isRef(Term,Lc,I),
  roundTerm(Lc,name(Lc,"_cell"),[I],R),
  typeOfExp(R,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,doTerm(Lc,Act,Tp),Path) :-
  isDoTerm(Term,Lc,Stmts),!,
  findType("action",Lc,Env,ActFnTp),
  newTypeVar("E",ErTp),
  newTypeVar("X",ElTp),
  mkTypeExp(ActFnTp,[ElTp,ErTp],MTp),
  checkType(Term,MTp,Tp,Env),
  checkAction(Stmts,Env,Ev,ActFnTp,ElTp,ErTp,Act,Path).
typeOfExp(Term,Tp,Env,Ev,taskTerm(Lc,Act,Tp),Path) :-
  isTaskTerm(Term,Lc,Stmts),!,
  findType("task",Lc,Env,TaskTp), % action type is just the core
  newTypeVar("E",ErTp),
  newTypeVar("X",ElTp),
  mkTypeExp(TaskTp,[ElTp,ErTp],TTp),
  checkType(Term,TTp,Tp,Env),
  checkAction(Stmts,Env,Ev,TaskTp,ElTp,ErTp,Act,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSquareTuple(Term,Lc,Els),
  \+isListAbstraction(Term,_,_,_), !,
  squareTupleExp(Lc,Els,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTuple(Term,Lc,Els),
  \+isAbstraction(Term,_,_,_),
  tpName(Tp,Lbl),
  checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTuple(Term,Lc,Els),
  tpName(Tp,Lbl),
  checkRecordBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  checkThetaBody(FnTp,Lbl,Lc,Els,E0,Val,Path).

typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("R",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  checkRecordBody(FnTp,Lbl,Lc,Els,E0,Val,Path).
typeOfExp(Term,Tp,Ev,Ev,LetExp,Path) :-
  isLetDef(Term,Lc,Els,Ex),
  checkLetExp(Tp,Lc,Els,Ex,Ev,LetExp,Path).
typeOfExp(Term,Tp,Ev,Ev,LetExp,Path) :-
  isLetRec(Term,Lc,Els,Ex),
  checkLetRec(Tp,Lc,Els,Ex,Ev,LetExp,Path).
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
  isUnaryMinus(Term,Lc,Arg), % handle unary minus
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
  findType("cons",Lc,Env,LTp),
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
  isSlice(Term,Lc,S,F,T),!,
  ternary(Lc,"_slice",S,F,T,Actual),
  typeOfExp(Actual,Tp,Env,Ev,Exp,Path).
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
  isEquation(Term,_Lc,_H,_R),
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
  typeOfExp(F,FnTp,Env,E0,Fun,Path),
%  reportMsg("round term fun %s has type %s",[can(Fun),tpe(FnTp)]),
  (sameType(funType(At,Tp),FnTp,E0) ->
%     reportMsg("type of %s:%s (%s)",[F,FnTp,Tp]),
     typeOfArgTerm(tuple(Lc,"()",A),At,E0,Ev,Args,Path);
   sameType(consType(At,Tp),FnTp,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),At,E0,Ev,Args,Path);
   reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FnTp,At,Tp],Lc),
   Args = tple(Lc,[]),
   Env=Ev).
%   reportMsg("after type of %s:%s (%s)",[F,FnTp,Tp]).

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
  typeOfExp(name(Lc,"_iter"),ItrFnTp,Env,E0,Iterator,Path),
  typeOfPtn(L,ElTp,E0,E1,Ptn,Path),
  typeOfExp(R,SrTp,E1,Ev,Src,Path).
%  reportMsg("search %s:%s",[search(Lc,Ptn,Src,Iterator),Tp]).

typeOfLambda(Term,Tp,Env,lambda(Lc,Lbl,equation(Lc,Args,Guard,Exp),Tp),Path) :-
%  reportMsg("expected type of lambda %s = %s",[Term,Tp]),
  isEquation(Term,Lc,H,C,R),
  newTypeVar("_A",AT),
  typeOfArgPtn(H,AT,Env,E0,Args,Path),
  checkGuard(C,E0,E1,Guard,Path),
  newTypeVar("_E",RT),
  checkType(Term,funType(AT,RT),Tp,Env),
					%  reportMsg("type after arg %s",[Tp]),
  lambdaLbl(Path,"_",Lbl),
  typeOfExp(R,RT,E1,_,Exp,Path).

typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  isBinary(Arg,_,"->",Ky,Vl),!,
  ternary(Lc,"_put",Mp,Ky,Vl,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  isNegation(Arg,_,Ky),!,
  binary(Lc,"_remove",Mp,Ky,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,Env,Ev,Exp,Path) :-
  binary(Lc,"_index",Mp,Arg,Term),
  typeOfExp(Term,Tp,Env,Ev,Exp,Path).

pickupContract(Lc,Env,Nm,StTp,DpTps,Op) :-
  (getContract(Nm,Env,conDef(_,_,_,Con)) ->
   freshen(Con,Env,_,contractExists(conTract(Op,[StTp],DpTps),_));
   reportError("%s contract not defined",[Nm],Lc),
   newTypeVar("_St",StTp),
   DpTps=[]).

checkGuard(none,Env,Env,none,_) :-!.
checkGuard(some(G),Env,Ev,some(Goal),Path) :-
  checkGoal(G,Env,Ev,Goal,Path).

checkGoal(G,Env,Ev,Goal,Path) :-
  locOfAst(G,Lc),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,Env,Ev,Cond,Path),
  (isIterableGoal(Cond) ->
   pickupContract(Lc,Env,"execution",_,_,ExContract),
   genIterableGl(Cond,ExContract,Path,Goal);
%   reportMsg("iterable goal: %s",[Goal]);
   Cond=Goal).

checkCaseExp(Lc,Bnd,Cases,Tp,Env,Env,case(Lc,Bound,Eqns,Tp),Path) :-
  newTypeVar("_L",LhsTp),
  typeOfExp(Bnd,LhsTp,Env,_,Bound,Path),
%  reportMsg("case governer: %s:%s",[Bound,LhsTp]),
  checkCases(Cases,LhsTp,Tp,Env,Eqns,Eqx,Eqx,[],Path),!.

checkCases([],_,_,_,Eqs,Eqs,Dfx,Dfx,_).
checkCases([C|Ss],LhsTp,Tp,Env,Eqns,Eqx,Df,Dfx,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkCase(Lc,L,G,R,LhsTp,Tp,Env,Eqns,Eqs,Df,Df0,Path),
  checkCases(Ss,LhsTp,Tp,Env,Eqs,Eqx,Df0,Dfx,Path).

checkCase(Lc,Lhs,G,R,LhsTp,Tp,Env,Eqns,Eqns,Df,Defx,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkCase(Lc,DLhs,G,R,LhsTp,Tp,Env,Df,Defx,_,_,Path).
checkCase(Lc,H,G,R,LhsTp,Tp,Env,
	  [equation(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Path) :-
  typeOfPtn(H,LhsTp,Env,E1,Arg,Path),
  checkGuard(G,E1,E2,Guard,Path),
  typeOfExp(R,Tp,E2,_,Exp,Path).

checkAbstraction(Term,Lc,B,G,Tp,Env,Abstr,Path) :-
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,Env,E1,Cond,Path),
  pickupContract(Lc,Env,"sequence",StTp,[ElTp],Op),
  checkType(Term,Tp,StTp,Env),
  typeOfExp(B,ElTp,E1,_,Bnd,Path),
  pickupContract(Lc,Env,"execution",ExTp,_,Contract),
  findType("action",Lc,Env,ActionTp),
  newTypeVar("_Er",ErTp),
  checkType(name(Lc,"action"),ActionTp,ExTp,Env),
  genReturn(Lc,over(Lc,mtd(Lc,"_nil",StTp),
		    true,[conTract(Op,[StTp],[ElTp])]),
	    ExTp,StTp,ErTp,Contract,Zed),
  Gen = over(Lc,mtd(Lc,"_cons",
		    funType(tupleType([ElTp,StTp]),StTp)),
	     true,[conTract(Op,[StTp],[ElTp])]),
  genCondition(Cond,Path,checker:genRtn(Lc,ExTp,StTp,ErTp,Contract),
	       checker:genSeq(Lc,Path,Contract,ExTp,ErTp),
	       checker:genEl(Lc,Gen,Bnd,StTp,Contract,ExTp,ErTp),
	       lifted(Zed),ACond),
  genPerform(Lc,ACond,Tp,ActionTp,Contract,Abstr).

genEl(Lc,Gen,Bnd,StTp,Contract,ExTp,ErTp,unlifted(St),Exp) :-
  Next  = apply(Lc,Gen,tple(Lc,[Bnd,St]),StTp),
  genReturn(Lc,Next,ExTp,StTp,ErTp,Contract,Exp).

genPut(Lc,Gen,Key,Value,StTp,Contract,ExTp,ErTp,unlifted(St),Exp) :-
  Next  = apply(Lc,Gen,tple(Lc,[St,Key,Value]),StTp),
  genReturn(Lc,Next,ExTp,StTp,ErTp,Contract,Exp).

genSeq(Lc,Path,Contract,ExecTp,ErTp,St,Init,Reslt,Exp) :-
  typeOfCanon(St,ATp),
  mkTypeExp(ExecTp,[ErTp,ATp],MdTp),
  LTp = funType(tupleType([ATp]),MdTp),
  Lam = lambda(Lc,LamLbl,equation(Lc,tple(Lc,[St]),none,Reslt),LTp),
  Gen = over(Lc,mtd(Lc,"_sequence",funType(tupleType([MdTp,LTp]),MdTp)),
	     true,[conTract(Contract,[ExecTp],[])]),
  genRtn(Lc,ExecTp,LTp,ErTp,Contract,Init,Initial),
  Exp = apply(Lc,Gen,tple(Lc,[Initial,Lam]),MdTp),
  lambdaLbl(Path,"sq",LamLbl).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

checkAction(Term,Env,Env,_,VlTp,_,noDo(Lc),_) :-
  isIden(Term,Lc,"nothing"),!,
  checkType(Term,tupleType([]),VlTp,Env).
checkAction(Term,Env,Env,_,VlTp,_ErTp,noDo(Lc),_Path) :-
  isBraceTuple(Term,Lc,[]),!,
  checkType(Term,tupleType([]),VlTp,Env).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,seqDo(Lc,A1,A2),Path) :-
  isActionSeq(Term,Lc,S1,S2),!,
  checkAction(S1,Env,E1,AcTp,tupleType([]),ErTp,A1,Path),
  checkAction(S2,E1,Ev,AcTp,VlTp,ErTp,A2,Path).
checkAction(Term,Env,Ev,_AcTp,VlTp,_ErTp,varDo(Lc,Ptn,Exp),Path) :-
  isMatch(Term,Lc,P,Ex),!,
  checkType(Term,tupleType([]),VlTp,Env),
  newTypeVar("_P",PT),
  typeOfPtn(P,PT,Env,Ev,Ptn,Path),
  typeOfExp(Ex,PT,Env,_,Exp,Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,Act,Path) :-
  isOptionMatch(Term,Lc,P,Exp),!,
  unary(Lc,"some",P,OP),
  binary(Lc,".=",OP,Exp,Term2),
  checkAction(Term2,Env,Ev,AcTp,VlTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,Act,Path) :-
  isSplice(Term,Lc,S,F,T,R),!,
  checkType(Term,tupleType([]),VlTp,Env),
  unary(Lc,"!",S,Src),
  nary(Lc,"_splice",[S,F,T,R],Rep),
  checkAssignment(Lc,Src,Rep,Env,Ev,AcTp,VlTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,Act,Path) :-
  isAssignment(Term,Lc,L,R),!,
  checkAssignment(Lc,L,R,Env,Ev,AcTp,VlTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,ifThenDo(Lc,Ts,Th,El),Path) :-
  isIfThenElse(Term,Lc,T,H,E),!,
  checkGuard(T,Env,Et,Ts,Path),
  checkAction(H,Et,E1,AcTp,VlTp,ErTp,Th,Path),
  checkAction(E,Env,E2,AcTp,VlTp,ErTp,El,Path),
  mergeDict(E1,E2,Env,Ev).
checkAction(Term,Env,Env,AcTp,VlTp,ErTp,ifThenDo(Lc,Ts,Th,noDo(Lc)),Path) :-
  isIfThen(Term,Lc,T,H),!,
  checkGuard(T,Env,Et,Ts,Path),
  checkType(Term,tupleType([]),VlTp,Env),
  checkAction(H,Et,_E1,AcTp,VlTp,ErTp,Th,Path).
checkAction(Term,Env,Env,AcTp,VlTp,ErTp,whileDo(Lc,Ts,Bdy),Path) :-
  isWhileDo(Term,Lc,T,B),!,
  checkType(Term,tupleType([]),VlTp,Env),
  checkGuard(T,Env,Et,Ts,Path),
  checkAction(B,Et,_,AcTp,tupleType([]),ErTp,Bdy,Path).
checkAction(Term,Env,Env,AcTp,VlTp,ErTp,untilDo(Lc,Ts,Bdy),Path) :-
  isUntilDo(Term,Lc,B,T),!,
  checkType(Term,tupleType([]),VlTp,Env),
  checkGuard(T,Env,_Et,Ts,Path),
  checkAction(B,Env,_,AcTp,tupleType([]),ErTp,Bdy,Path).
checkAction(Term,Env,Env,AcTp,VlTp,ErTp,forDo(Lc,Ts,Bdy),Path) :-
  isForDo(Term,Lc,T,B),!,
  checkType(Term,tupleType([]),VlTp,Env),
  checkGuard(T,Env,Et,Ts,Path),
  checkAction(B,Et,_,AcTp,tupleType([]),ErTp,Bdy,Path).
checkAction(Term,Env,Env,AcTp,VlTp,ErTp,tryCatchDo(Lc,Bdy,Hndlr),Path) :-
  isTryCatch(Term,Lc,B,H),!,
  anonVar(Lc,Anon,BdErTp),
  checkAction(B,Env,_,AcTp,VlTp,BdErTp,Bdy,Path),
  checkCatch(H,Env,AcTp,VlTp,ErTp,BdErTp,Anon,Hndlr,Path).
checkAction(Term,Env,Env,_AcTp,VlTp,ErTp,throwDo(Lc,Exp),Path) :-
  isThrow(Term,Lc,E),!,
  checkType(Term,tupleType([]),VlTp,Env),
  typeOfExp(E,ErTp,Env,_,Exp,Path).
checkAction(Term,Env,Env,_AcTp,VlTp,_ErTp,valisDo(Lc,Reslt),Path) :-
  isReturn(Term,Lc,Ex),!,
  typeOfExp(Ex,VlTp,Env,_,Exp,Path),
  processIterable(Env,Path,Exp,Reslt).
checkAction(Term,Env,Env,AcTp,VlTp,ErTp,caseDo(Lc,Gov,Cases),Path) :-
  isCaseExp(Term,Lc,Gv,Cs),!,
  newTypeVar("_G",GTp),
  typeOfExp(Gv,GTp,Env,_,Gov,Path),
  checkActionCases(Cs,GTp,Env,AcTp,VlTp,ErTp,Cases,Dflt,Dflt,[],Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,performDo(Lc,Act),Path) :-
  isPerform(Term,Lc,Arg),!,
  checkType(Term,tupleType([]),VlTp,Env),
  newTypeVar("_",ATp),
  checkAction(Arg,Env,Ev,AcTp,ATp,ErTp,Act,Path).
checkAction(Term,Env,Env,_AcTp,VlTp,_ErTp,varDo(Lc,Anon,Exp),Path) :-
  isIgnore(Term,Lc,Ex),!,
  checkType(Term,tupleType([]),VlTp,Env),
  newTypeVar("_P",PT),
  anonVar(Lc,Anon,PT),
  typeOfExp(Ex,PT,Env,_,Exp,Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,Exp,Path) :-
  isBraceTuple(Term,_,[Stmt]),!,
  checkAction(Stmt,Env,Ev,AcTp,VlTp,ErTp,Exp,Path).
checkAction(Term,Env,Ev,AcTp,VlTp,ErTp,simpleDo(Lc,Exp),Path) :-
  isRoundTerm(Term,Lc,_,_),!,
  mkTypeExp(AcTp,[VlTp,ErTp],MTp),
  typeOfExp(Term,MTp,Env,Ev,Exp,Path).
checkAction(Term,Env,Env,_,_,_,noDo(Lc),_) :-
  locOfAst(Term,Lc),
  reportError("invalid action: %s",[Term],Lc).

checkAssignment(Lc,L,R,Env,Ev,AcTp,VlTp,ErTp,simpleDo(Lc,Exp),Path) :-
  checkType(L,tupleType([]),VlTp,Env),
  (isIndexTerm(L,LLc,C,I) ->
     unary(LLc,"!",C,CC),
    ternary(LLc,"_put",CC,I,R,Repl),
    binary(Lc,":=",C,Repl,Term);
   binary(Lc,":=",L,R,Term)),
  mkTypeExp(AcTp,[VlTp,ErTp],MTp),
  typeOfExp(Term,MTp,Env,Ev,Exp,Path).

checkActionCases([],_,_,_,_,_,Cases,Cases,Dfx,Dfx,_).
checkActionCases([C|Ss],GTp,Env,AcTp,VlTp,ErTp,Cases,Cx,Df,Dfx,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkActionCase(Lc,L,G,R,GTp,Env,AcTp,VlTp,ErTp,Cases,C0,Df,Df0,Path),
  checkActionCases(Ss,GTp,Env,AcTp,VlTp,ErTp,C0,Cx,Df0,Dfx,Path).

checkActionCase(Lc,Lhs,G,R,GTp,Env,AcTp,VlTp,ErTp,Cases,Cases,Df,Dfx,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkActionCase(Lc,DLhs,G,R,GTp,Env,AcTp,VlTp,ErTp,Df,Dfx,_,_,Path).
checkActionCase(Lc,H,G,R,GTp,Env,AcTp,VlTp,ErTp,
		[equation(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Path) :-
  typeOfPtn(H,GTp,Env,E1,Arg,Path),
  checkGuard(G,E1,E2,Guard,Path),
  checkAction(R,E2,_,AcTp,VlTp,ErTp,Exp,Path).

/*
 assert C 
becomes
 try{
   assrt(()=>C,"failed: C",Loc)
 } catch(Err) => action{
   logMsg(Err)
   throw ()
  }
*/
createIntegrityAction(Lc,Cond,Act) :-
  ast2String(Cond,Msg),
  locOfAst(Cond,CLc),
  eqn(Lc,tuple(Lc,"()",[]),Cond,Lam),
  astOfLoc(CLc,Loc),
  roundTerm(Lc,name(Lc,"assrt"),[Lam,string(Lc,Msg),Loc],Assert),
  braceTuple(Lc,[Assert],B),
  genIden(Lc,Err),
  roundTerm(Lc,name(Lc,"logMsg"),[Err],Rep),
  roundTuple(Lc,[],Unit),
  unary(Lc,"throw",Unit,Thr),
  binary(Lc,";",Rep,Thr,CtBd),
  braceTerm(Lc,name(Lc,"action"),[CtBd],R1),
  eqn(Lc,tuple(Lc,"()",[Err]),R1,ELam),
  binary(Lc,"catch",B,ELam,R2),
  unary(Lc,"try",R2,Act).

/*
 show E 
becomes
  shwMsg(()=>E,"E",Lc)
*/
createShowAction(Lc,Exp,Act) :-
  ast2String(Exp,Txt),
  locOfAst(Exp,ELc),
  astOfLoc(ELc,Loc),
  eqn(Lc,tuple(Lc,"()",[]),Exp,Lam),
  roundTerm(Lc,name(Lc,"shwMsg"),[Lam,string(Lc,Txt),Loc],Act).

checkCatch(Term,Env,AcTp,VlTp,ErTp,BdErTp,Anon,Hndlr,Path) :-
  isBraceTuple(Term,Lc,[St]),!,
  mkTypeExp(AcTp,[VlTp,ErTp],MTp),
  Htype = funType(tupleType([BdErTp]),MTp),
  checkAction(St,Env,_,AcTp,VlTp,ErTp,HA,Path),
  Hndlr = lambda(Lc,LamLbl,equation(Lc,tple(Lc,[Anon]),none,HA),Htype),
  lambdaLbl(Path,"catch",LamLbl).
checkCatch(Term,Env,AcTp,VlTp,ErTp,BdErTp,_,Hndlr,Path) :-
  mkTypeExp(AcTp,[ErTp,VlTp],MTp),
  Htype = funType(tupleType([BdErTp]),MTp),
  typeOfExp(Term,Htype,Env,_,Hndlr,Path).

recordAccessExp(Lc,Rc,Fld,ET,Env,Ev,dot(Lc,Rec,Fld,Tp),Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,Env,Ev,Rec,Path),
  faceOfType(AT,Env,Fc),
  freshen(Fc,Env,_,Fce),
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

typeOfTerms([],[],Env,Env,_,[],_).
typeOfTerms([],[T|_],Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfTerms([A|_],[],Env,Env,_,[],_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfTerms([A|As],[ETp|ElTypes],Env,Ev,_,[Term|Els],Path) :-
  deRef(ETp,ElTp),
  typeOfExp(A,ElTp,Env,E0,Term,Path),
  % reportMsg("type of argument %s |= %s",[A,ETp]),
  % dispEnv(Env),
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

% Analyse a square tuple term to try to disambiguate maps from others.

squareTupleExp(Lc,Els,Tp,Env,Ev,Exp,Path) :-
  macroListEntries(Lc,Els,Trm,nilGen,consGen,appndGen),
  typeOfExp(Trm,Tp,Env,Ev,Exp,Path).

/* Process any remaining iterable conditions */

processIterable(Env,Path,Cond,Goal) :-
  isIterableGoal(Cond),!,
  locOfCanon(Cond,Lc),
  pickupContract(Lc,Env,"execution",_,_,Contract),
  genIterableGl(Cond,Contract,Path,Goal).
%  reportMsg("iterable exp -> %s",[Goal]).
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
macroListEntries(_,[Cns],Trm,_,Hed,_) :-
  isConsTerm(Cns,Lc,H,T),
  call(Hed,Lc,H,T,Trm).
macroListEntries(Lc,[E|L],Trm,End,Hed,Tail) :-
  macroListEntries(Lc,L,Tr,End,Hed,Tail),
  call(Hed,Lc,E,Tr,Trm).

checkType(_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Env).
checkType(Ast,S,T,_) :-
  locOfAst(Ast,Lc),
  reportError("%s:%s not consistent with expected type\n%s",[ast(Ast),tpe(S),tpe(T)],
	      Lc).

packageExport(Defs,Public,ExportDecls,LDecls,XDefs) :-
  genDecls(Defs,checker:isPkgPublic(Public),ExportDecls,[],LDecls,[],XDefs,[]).

isPkgPublic(Public,V) :-
  is_member(V,Public),!.

genDecls([],_,Exx,Exx,LDx,LDx,Dfx,Dfx).
genDecls([Def|Defs],Public,Exports,Exx,LDecls,LDx,Dfs,Dfx) :-
  genDecl(Def,Def,Public,Exports,Ex0,LDecls,LD0,Dfs,Df0),
  genDecls(Defs,Public,Ex0,Exx,LD0,LDx,Df0,Dfx).

genDecl(funDef(_,Nm,FullNm,Tp,_,_),Def,Public,
	[funDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,[Def|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(funDef(_,Nm,FullNm,Tp,_,_),Def,_,Ex,Ex,
	[funDec(Nm,FullNm,Tp)|Lx],Lx,[Def|Dfx],Dfx).
genDecl(typeDef(_,Nm,Tp,TpRule),Def,Public,
	[typeDec(Nm,Tp,TpRule)|Ex],Ex,Lx,Lx,[Def|Dfx],Dfx) :-
  call(Public,tpe(Nm)),!.
genDecl(typeDef(_,Nm,Tp,TpRule),Def,_,Ex,Ex,
	[typeDec(Nm,Tp,TpRule)|Lx],Lx,[Def|Dfx],Dfx).
genDecl(varDef(Lc,Nm,FullNm,[],Tp,lambda(_,_,Eqn),_),_,Public,
	 [funDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,
	 [funDef(Lc,Nm,FullNm,Tp,[],[Eqn])|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(varDef(Lc,Nm,FullNm,[],Tp,lambda(_,_,Eqn),_),_,Public,
	 Ex,Ex,[funDec(Nm,FullNm,Tp)|Lx],Lx,
	 [funDef(Lc,Nm,FullNm,Tp,[],[Eqn])|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(varDef(_,Nm,FullNm,_,Tp,_),Def,Public,
	[varDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,[Def|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(varDef(_,Nm,FullNm,_,Tp,_),Def,_,Ex,Ex,
	[varDec(Nm,FullNm,Tp)|Lx],Lx,[Def|Dfx],Dfx).
genDecl(cnsDef(_,Nm,Con),_,Public,[cnsDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  call(Public,var(Nm)),!,
  constructorName(Con,FullNm),
  typeOfCanon(Con,Tp).
genDecl(cnsDef(_,Nm,Con),_,_,Ex,Ex,[cnsDec(Nm,FullNm,Tp)|Lx],Lx,Dfx,Dfx) :-
  constructorName(Con,FullNm),
  typeOfCanon(Con,Tp).
genDecl(conDef(Nm,CnNm,CnTp,CnSpec),_,Public,
	[contractDec(Nm,CnNm,CnTp,CnSpec)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  call(Public,con(Nm)).
genDecl(conDef(Nm,CnNm,CnTp,CnSpec),_,_,Ex,Ex,
	[contractDec(Nm,CnNm,CnTp,CnSpec)|Lx],Lx,Dfx,Dfx).
genDecl(implDef(TmpNm,ImplName,ImplVrNm,Spec),_,Public,
	[impDec(ImplName,ImplVrNm,Spec)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  call(Public,imp(TmpNm)),!.
genDecl(implDef(_,ImplName,ImplVrNm,Spec),_,_,Ex,Ex,
	[impDec(ImplName,ImplVrNm,Spec)|Lx],Lx,Dfx,Dfx).
genDecl(accDef(Tp,Fld,AccFn,AccTp),_,Public,
	[accDec(Tp,Fld,AccFn,AccTp)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  exportAcc(Tp,Public).
genDecl(accDef(Tp,Fld,AccFn,AccTp),_,_,Ex,Ex,
	[accDec(Tp,Fld,AccFn,AccTp)|Lx],Lx,Dfx,Dfx).

computeThetaExport(Defs,Fields,Public,Decls,XDefs) :-
  genDecls(Defs,checker:isThetaPublic(Fields,Public),Decls,LDecls,LDecls,[],XDefs,[]).

isThetaPublic(Public,_,var(Nm)) :-
  is_member(var(Nm),Public),!.
isThetaPublic(_,Fields,var(Nm)) :-!,
  is_member((Nm,_),Fields),!.
isThetaPublic(Public,_,V) :-!,
  is_member(V,Public),!.

computeLetExport(Defs,Private,Decls,XDefs) :-
  genDecls(Defs,checker:isLetExport(Private),Decls,LDecls,LDecls,[],XDefs,[]).

isLetExport(Private,Nm) :-
  \+is_member(Nm,Private),!.

completePublic([],Pub,Pub,_).
completePublic([con(Nm)|List],Pub,Px,Path) :-
  contractName(Path,Nm,ConNm),
  completePublic(List,[tpe(ConNm),var(ConNm)|Pub],Px,Path).
completePublic([_|List],Pub,Px,Path) :-
  completePublic(List,Pub,Px,Path).

isPublicVar(Nm,_,Public) :-
  is_member(var(Nm),Public),!.
isPublicVar(Nm,Fields,_) :-
  is_member((Nm,_),Fields),!.

isPublicType(Nm,Public) :-
  is_member(tpe(Nm),Public),!.
  
isPublicContract(Nm,Public) :-
  is_member(con(Nm),Public),!.

isPublicImpl(Nm,Public) :-
  is_member(imp(Nm),Public),!.

exportAcc(Tp,Export) :-
  tpName(Tp,TpNm),
  (marker(type,TpMrkr),
   splitLocalName(TpNm,TpMrkr,_,Nm),
   call(Export,tpe(Nm));
   marker(conTract,CnMrkr),
   splitLocalName(TpNm,CnMrkr,_,Nm),
   call(Export,con(Nm))),!.

mkBoot(Env,Lc,Pkg,[BootDef|Dfs],Dfs) :-
  isVar("_main",Env,Spec),
  localName(Pkg,value,"_boot",BootNm),
  mkTypeExp(tpFun("star.core*consType",1),[type("star.core*string")],LSTp),
  CmdVr = v(Lc,"CmdVr",LSTp),
  UnitTp = tupleType([]),
  MnTp = funType(tupleType([LSTp]),UnitTp),
  typeOfVar(Lc,"_main",MnTp,Spec,Env,_Ev,MainTrm),
  BootEqn = equation(Lc,tple(Lc,[CmdVr]),none,
		     apply(Lc,MainTrm,
			   tple(Lc,[CmdVr]),UnitTp)),
  BootDef = funDef(Lc,"_boot",BootNm,funType(tupleType([LSTp]),UnitTp),[],[BootEqn]).
mkBoot(_Env,_,_,Dfs,Dfs).