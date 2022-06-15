:- module(checker,[checkProgram/6]).

:- use_module(abstract).
:- use_module(astdisp).
:- use_module(display).
:- use_module(wff).
:- use_module(macros).
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
:- use_module(import).
:- use_module(transitive).
:- use_module(resolve).

checkProgram(Prg,Pkg,Repo,_Opts,PkgDecls,Canon) :-
  stdDict(Base),
  isBraceTerm(Prg,Lc,_,Els),
%  reportMsg("checking package %s",[pk(Pkg)]),
  Pkg = pkg(Pk,_),
  collectImports(Els,Imports,Stmts),
  importAll(Imports,Repo,AllImports),
  collectImportDecls(AllImports,Repo,[],IDecls),
  declareAllDecls(IDecls,Lc,Base,Env0),
%  dispDecls(IDecls),
%  dispEnv(Env0),
  thetaEnv(Pk,Lc,Stmts,faceType([],[]),Env0,OEnv,Defs,Public),
%  dispEnv(OEnv),
  overload(Lc,Defs,OEnv,_Dict,ODefs),
  completePublic(Public,Public,FllPb,Pk),
  packageExport(ODefs,FllPb,EDecls,LDecls,XDefs),
  mkBoot(OEnv,Lc,Pk,XDefs,PkgDefs,EDecls,ExportDecls),
  Canon=prog(Pkg,Imports,ExportDecls,LDecls,PkgDefs),
  concat(ExportDecls,LDecls,D0),
  concat(D0,IDecls,PkgDecls).

findExportedDefs(Lc,Flds,Els) :-
  map(Flds,checker:mkFieldArg(Lc),Els).

mkFieldArg(Lc,(Nm,Tp),v(Lc,Nm,Tp)).

thetaEnv(Pkg,Lc,Stmts,Fields,Base,TheEnv,Defs,Public) :-
  collectDefinitions(Stmts,Dfs,Public,Annots),!,
  dependencies(Dfs,Groups,Annots),
  pushFace(Fields,Lc,Base,Env),
  checkGroups(Groups,Fields,Annots,Defs,[],Env,TheEnv,Pkg).

recordEnv(Path,_Lc,Stmts,Fields,Base,TheEnv,Defs,Public) :-
  collectDefinitions(Stmts,Dfs,Public,Annots),!,
  parseAnnotations(Dfs,Fields,Annots,Base,Path,Face),
  checkGroup(Dfs,Defs,[],Base,TheEnv,Face,Path).

collectDefinitions([St|Stmts],Defs,P,A) :-
  collectDefinition(St,Stmts,S0,Defs,D0,P,P0,A,A0,checker:nop),
  collectDefinitions(S0,D0,P0,A0).
collectDefinitions([],[],[],[]).

collectDefinition(St,Stmts,Stmts,[(cns(V),Lc,[T])|Defs],Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  (isIden(L,V),Ex=Export; isPrivate(L,_,V1),isIden(V1,V),Ex=checker:nop),
  isConstructorType(T,_,_,_,_,_),!,
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
  pullOthers(Els,Entries,_Asserts,_Defaults),
  call(Export,var(Nm),P,Px),
  braceTuple(Lc,Entries,Hd),
  reConstrain(XC,Hd,XHd),
  reXQuant(XQ,XHd,QHd),
  binary(Lc,"<=>",QHd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St).
collectConstructors(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,_Export) :-
  isPrivate(C,_,I),
  collectConstructors(I,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,checker:nop).

cmpAstPair(A1,A2) :-
  isTypeAnnotation(A1,_,N1,_),
  isTypeAnnotation(A2,_,N2,_),
  isIden(N1,_,I1),
  isIden(N2,_,I2),
  str_lt(I1,I2).

projectAstTps([],[]) :-!.
projectAstTps([A|As],[T|Ts]) :-
  isTypeAnnotation(A,_,_,T),!,
  projectAstTps(As,Ts).

pullOthers([],[],[],[]).
pullOthers([St|Els],Entries,[St|Asserts],Deflts) :-
  isIntegrity(St,_,_),!,
  pullOthers(Els,Entries,Asserts,Deflts).
pullOthers([St|Els],Entries,Asserts,[St|Deflts]) :-
  isDefault(St,_,_,_),!,
  pullOthers(Els,Entries,Asserts,Deflts).
pullOthers([St|Els],[St|Entries],Asserts,Deflts) :-
  pullOthers(Els,Entries,Asserts,Deflts).

collectImportDecls([],_,Decls,Decls) :-!.
collectImportDecls([importPk(_Lc,_Viz,Pkg)|More],Repo,Decls,Dcx) :-
  importPkg(Pkg,Repo,spec(_,_,PDecls)),
  concat(PDecls,Decls,Dc0),
  collectImportDecls(More,Repo,Dc0,Dcx).

declareAllDecls([],_,Env,Env).
declareAllDecls([D|More],Lc,Env,Evx) :-
  declareDecl(Lc,D,Env,E0),!,
  declareAllDecls(More,Lc,E0,Evx).

declareDecl(Lc,impDec(ImplNm,FullNm,ImplTp),Ev,Evx) :-
  declareVr(Lc,FullNm,ImplTp,none,Ev,Ev0),
  declareImplementation(ImplNm,FullNm,ImplTp,Ev0,Evx).
declareDecl(_,accDec(Tp,Fld,FnNm,AccTp),Ev,Evx) :-
  declareFieldAccess(Tp,Fld,FnNm,AccTp,Ev,Evx).
declareDecl(_,updDec(Tp,Fld,FnNm,AccTp),Ev,Evx) :-
  declareFieldUpdater(Tp,Fld,FnNm,AccTp,Ev,Evx).
declareDecl(Lc,contractDec(Nm,CnNm,Rule),Ev,Evx) :-
  defineContract(Nm,Lc,conDef(Nm,CnNm,Rule),Ev,Evx).
declareDecl(Lc,typeDec(Nm,Tp,Rule),Env,Evx) :-
  declareType(Nm,tpDef(Lc,Tp,Rule),Env,Evx).
declareDecl(Lc,varDec(Nm,_FullNm,Tp),Env,Evx) :-
  (varLoc(Nm,Env,VTp,VLc),
   \+sameType(Tp,VTp,Lc,Env) ->
   reportWarning("%s:%s already declared as %s at %s",[Nm,tpe(Tp),tpe(VTp),loc(VLc)],Lc);true),
  declareVr(Lc,Nm,Tp,none,Env,Evx).
declareDecl(Lc,cnsDec(Nm,FullNm,Tp),Env,Evx) :-
  (isConType(Tp,0) ->
   declareEnum(Lc,Nm,FullNm,Tp,Env,Evx) ;
   declareCns(Lc,Nm,FullNm,Tp,Env,Evx)).
declareDecl(Lc,funDec(Nm,_FullNm,Tp),Env,Evx) :-
  (varLoc(Nm,Env,VTp,VLc),
   \+sameType(Tp,VTp,Lc,Env) ->
   reportWarning("function %s:%s already declared as %s at %s",[Nm,tpe(Tp),tpe(VTp),loc(VLc)],Lc);true),
  declareVr(Lc,Nm,Tp,none,Env,Evx).
declareDecl(Lc,Entry,Env,Env) :-
  reportError("(internal) cannot figure out import entry %s",[Entry],Lc).

importContracts([],_,Env,Env).
importContracts([C|L],Lc,E,Env) :-
  C = conDef(Nm,_,_),
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

declareMethods(conDef(_,_,ConEx),Lc,Env,Ev) :-
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

parseConstructor(Nm,Lc,T,Env,Ev,Dfs,Defs,Path) :-
  parseType(T,Env,Tp),
  mangleName(Path,class,Nm,FullNm),
  unwrapType(Tp,_Q,_Cx,ITp),
  (deRef(ITp,consType(tplType([]),_)) ->
   declareEnum(Lc,Nm,FullNm,Tp,Env,Ev),
   Dfs=[cnsDef(Lc,Nm,enm(Lc,FullNm,Tp))|Defs];
   declareCns(Lc,Nm,FullNm,Tp,Env,Ev),
   Dfs=[cnsDef(Lc,Nm,cons(Lc,FullNm,Tp))|Defs]).

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
parseAnnotation(N,Lc,_,_,_,Env,F,[(N,Tp)|F]) :-
  getVar(Lc,N,Env,_,Tp),!.
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
  faceOfType(VrTp,Lc,E0,faceType(Flds,Tps)),
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

formDefn([Eqn|Eqns],Nm,LclNm,Env,Ev,Tp,Cx,[funDef(Lc,Nm,LclNm,hard,Tp,Cx,[Eqn|Eqns])|Dx],Dx) :-
  Eqn = rule(Lc,_,_,_),
  declareVr(Lc,Nm,Tp,none,Env,Ev).
formDefn([varDef(Lc,_,_,_,_,open(VLc,Value,Tp))],Nm,LclNm,Env,Ev,Tp,Cx,
	 [varDef(Lc,Nm,LclNm,Cx,Tp,open(VLc,Value,Tp))|Dx],Dx) :-
  freshen(Tp,Env,_,FTp),
  faceOfType(FTp,Lc,Env,FaceTp),
  declareVr(Lc,Nm,Tp,some(FaceTp),Env,Ev).
formDefn([varDef(Lc,_,_,_,_,Value)],Nm,LclNm,Env,Ev,Tp,Cx,
    [varDef(Lc,Nm,LclNm,Cx,Tp,Value)|Dx],Dx) :-
  declareVr(Lc,Nm,Tp,none,Env,Ev).

processStmts([],_,Defs,Defs,Dflts,Dflts,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Df,Dfx,E0,Path) :-
  processStmt(St,ProgramType,Defs,D0,Df,Df0,E0,Path),!,
  processStmts(More,ProgramType,D0,Dx,Df0,Dfx,E0,Path).

processStmt(St,ProgramType,Defs,Defx,Df,Dfx,E,Path) :-
  isEquation(St,Lc,L,Cond,R),!,
%  reportMsg("check rule %s",[St],Lc),
  checkEquation(Lc,L,Cond,R,ProgramType,Defs,Defx,Df,Dfx,E,Path).
processStmt(St,Tp,[Def|Defs],Defs,Df,Df,Env,Path) :-
  isDefn(St,Lc,L,R),
  checkDefn(Lc,L,R,Tp,Def,Env,Path),!.
processStmt(St,_,Defs,Defs,Df,Df,_,_) :-
  locOfAst(St,Lc),
  reportError("Invalid statement %s",[ast(St)],Lc).

pickupDefnType(N,_,faceType(F,_),_,Ev,Ev,Tp) :-
  is_member((N,Tp),F),!.
pickupDefnType(Nm,Lc,_,Stmts,E,Ev,Tp) :-
  guessStmtType(Stmts,Nm,Lc,Tp),
  declareVr(Lc,Nm,Tp,none,E,Ev).

guessStmtType([],N,Lc,VTp) :- !,
  reportError("%s not declared",[N],Lc),
  newTypeVar("_",VTp).
guessStmtType([St|_],N,Lc,Guess) :-
  guessType(St,N,Lc,Guess).

guessType(St,_,_,funType(tplType(AT),RTp)) :-
  isEquation(St,_,H,_,_),!,
  splitHead(H,_,tuple(_,_,Args),_),
  genTpVars(Args,AT),
  newTypeVar("_",RTp).
guessType(St,_,_,GTp) :-
  isDefn(St,_,_,_),!,
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
  Eqn = rule(Lc,Args,Guard,Exp),
%  reportMsg("rule %s",[Eqn],Lc),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[Eqn|Dfx]; Defs=[Eqn|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("rule not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,Tp,varDef(Lc,Nm,ExtNm,[],Tp,Value),Env,Path) :-
  isIden(L,_,Nm),
  pushScope(Env,E),
  typeOfExp(R,Tp,E,_E2,Value,Path),
  packageVarName(Path,Nm,ExtNm).

checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  genNewName(Path,"Γ",ThPath),
  thetaEnv(ThPath,Lc,Els,Face,BaseEnv,_,Defs,Public),
  faceOfType(ETp,Lc,Env,TpFace),
  getConstraints(TpFace,_,faceType(Fs,_)),
  completePublic(Public,Public,FullPublic,Path),
  computeThetaExport(Defs,Fs,FullPublic,Decls,Defns),!,
  formTheta(Lc,Lbl,Decls,Defns,Fs,Tp,Val).

formTheta(Lc,Lbl,Decls,Defs,Flds,Tp,letRec(Lc,Decls,Defs,Exp)) :-
  sort(Flds,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,SortedFlds,Args),
  project1(SortedFlds,ElTps),
  Exp = apply(Lc,cons(Lc,Lbl,consType(tplType(ElTps),Tp)),
	      tple(Lc,Args),Tp).

checkRecordBody(Tp,Lbl,Lc,Els,Env,letExp(Lc,Decls,Defs,Exp),Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,faceType(Fs,Ts)),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  genNewName(Path,"Γ",ThPath),
  recordEnv(ThPath,Lc,Els,faceType(Fs,Ts),BaseEnv,_,Defs,Public),
  completePublic(Public,Public,FullPublic,Path),
  computeThetaExport(Defs,Fs,FullPublic,Decls,Defs),!,
  sort(Fs,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,SortedFlds,Args),
  Exp = apply(Lc,Lbl,tple(Lc,Args),Tp).

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
  parseContractConstraint(Quants,Cons,Sq,Env,Nm,_ConNm,ConSpec),
%  dispType(ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
%  dispType(CnSpec),
  getConstraints(CnSpec,AC,contractExists(Spec,_)),
  declareTypeVars(IQ,Lc,Env,E1),
  declareConstraints(Lc,AC,E1,ThEnv),
  implementationName(Spec,ImplName),
  mangleName(Path,value,ImplName,ImplVrNm),
  contractType(Spec,CnType),
  dollarName(Nm,DlNm),
  labelImplExp(IBody,DlNm,ImpBody),
  typeOfExp(ImpBody,CnType,ThEnv,_ThEv,ImplTerm,ImplVrNm),
%  reportMsg("implementation %s:%s",[can(ImplTerm),tpe(CnType)]),
  putConstraints(AC,CnType,SS1),
  reQuantTps(SS1,IQ,ImpType),
  ImplVar = varDef(Lc,ImplVrNm,ImplVrNm,AC,ImpType,ImplTerm),
  Impl = implDef(INm,ImplName,ImplVrNm,ImpType),
  declareVr(Lc,ImplVrNm,ImpType,none,Env,Ev0),
  declareImplementation(ImplName,ImplVrNm,ImpType,Ev0,Evx),!.
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_) :-
  locOfAst(Stmt,Lc),
  reportError("could not check implementation statement %s",[ast(Stmt)],Lc).

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
  reportError("invalid form of implementation %s",[ast(Trm)],Lc).

pickBoundType((_,Tv),Tp,allType(Tv,Tp)).

sameLength(L1,L2,_) :- length(L1,L), length(L2,L),!.
sameLength(L1,_,Lc) :-
  length(L1,L),
  reportError("expecting %s elements",[L],Lc).

% Patterns are very similarly checked to expressions, except that fewer forms
typeOfArgPtn(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(T),tplType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgPtn(T,Tp,Env,Ev,Exp,Path) :-
  typeOfPtn(T,Tp,Env,Ev,Exp,Path).

typeOfPtn(V,Tp,Env,Env,anon(Lc,Tp),_Path) :-
  isAnon(V,Lc),!.
typeOfPtn(V,Tp,Env,Ev,Term,Path) :-
  isIden(V,Lc,N),
  getVar(Lc,N,Env,_,_),!,
  mkWhereEquality(Lc,V,TT),
  typeOfPtn(TT,Tp,Env,Ev,Term,Path).
typeOfPtn(V,Tp,Ev,Env,v(Lc,N,Tp),_Path) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,none,Ev,Env).
typeOfPtn(Trm,Tp,Env,Ev,Term,Path) :-
  isEnum(Trm,_,_),
  typeOfExp(Trm,Tp,Env,Ev,Term,Path).
typeOfPtn(Trm,Tp,Env,Env,intLit(Lc,Ix),_) :-
  isLiteralInteger(Trm,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  verifyType(Lc,ast(Trm),IntTp,Tp,Env).
typeOfPtn(T,Tp,Env,Env,bigLit(Lc,Bx),_Path) :-
  isLiteralBigInt(T,Lc,Bx),!,
  findType("bigint",Lc,Env,BigTp),
  verifyType(Lc,ast(T),BigTp,Tp,Env).
typeOfPtn(T,Tp,Env,Env,floatLit(Lc,Dx),_Path) :- 
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  verifyType(Lc,ast(T),FltTp,Tp,Env).
typeOfPtn(char(Lc,Cp),Tp,Env,Env,charLit(Lc,Cp),_Path) :- !,
  findType("char",Lc,Env,StrTp),
  verifyType(Lc,ast(char(Lc,Cp)),StrTp,Tp,Env).
typeOfPtn(string(Lc,Sx),Tp,Env,Env,stringLit(Lc,Sx),_Path) :- !,
  findType("string",Lc,Env,StrTp),
  verifyType(Lc,ss(Sx),StrTp,Tp,Env).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  verifyType(Lc,ast(Term),RT,Tp,Env),
  typeOfPtn(L,Tp,Env,Ev,Exp,Path).
typeOfPtn(P,Tp,Env,Ev,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),
  typeOfPtn(L,Tp,Env,E0,Ptn,Path),
  checkGuard(some(C),E0,Ev,some(Cond),Path).
typeOfPtn(Trm,Tp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfPtn(Inner,Tp,Env,Ev,Exp,Path).
typeOfPtn(Trm,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(Trm),tplType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,Env,Ev,Lc,Els,Path).
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
  reportError("illegal pattern: %s, expecting a %s",[ast(Term),tpe(Tp)],Lc).

typeOfRecordPtn(Lc,Tp,F,Args,Env,Ev,Exp,Path) :-
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  faceOfType(FnTp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(E0,E1),
  declareConstraints(Lc,Cx,E1,BaseEnv),
  typeOfElementPtns(Args,Face,BaseEnv,Ev,PtnDefs,[],Path),
  fillinElementPtns(PtnDefs,Lc,FaceTp,ArgPtns),
  Exp = apply(Lc,Fun,tple(Lc,ArgPtns),Tp).
%  reportMsg("record ptn = %s",[can(Exp)],Lc).

typeOfElementPtns([],_,_,Env,Env,Defs,Defs,_Path).
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
fillinElementPtn(Lc,(Nm,Tp),Els,[(Nm,anon(Lc,Tp))|Els]).
  
typeOfArgTerm(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(T),tplType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfArgTerm(T,Tp,Env,Ev,Exp,Path) :-
  typeOfExp(T,Tp,Env,Ev,Exp,Path).

typeOfExp(V,Tp,Env,Env,anon(Lc,Tp),_) :-
  isAnon(V,Lc),
  reportError("anonymous variable not permitted as expression",[],Lc).
typeOfExp(V,Tp,Env,Env,Term,_Path) :-
  isIden(V,Lc,N),!,
  (getVar(Lc,N,Env,Term,VTp) ->
   verifyType(Lc,ast(V),VTp,Tp,Env);
   reportError("variable '%s' not defined, expecting a %s",[V,Tp],Lc),
   Term=void).
typeOfExp(T,Tp,Env,Ev,Term,Path) :-
  isEnum(T,_,N),
  typeOfExp(N,consType(tplType([]),Tp),Env,Ev,Term,Path),!.
typeOfExp(T,Tp,Env,Env,intLit(Lc,Ix),_Path) :-
  isLiteralInteger(T,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  verifyType(Lc,ast(T),IntTp,Tp,Env).
typeOfExp(T,Tp,Env,Env,bigLit(Lc,Ix),_Path) :-
  isLiteralBigInt(T,Lc,Ix),!,
  findType("bigint",Lc,Env,BigTp),
  verifyType(Lc,ast(T),BigTp,Tp,Env).
typeOfExp(T,Tp,Env,Env,floatLit(Lc,Dx),_Path) :-
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  verifyType(Lc,ast(T),FltTp,Tp,Env).
typeOfExp(char(Lc,Cp),Tp,Env,Env,charLit(Lc,Cp),_Path) :- !,
  findType("char",Lc,Env,StrTp),
  verifyType(Lc,ast(char(Lc,Cp)),StrTp,Tp,Env).
typeOfExp(string(Lc,Sx),Tp,Env,Env,stringLit(Lc,Sx),_Path) :- !,
  findType("string",Lc,Env,StrTp),
  verifyType(Lc,ss(Sx),StrTp,Tp,Env).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,PTp),
  verifyType(Lc,ast(Term),PTp,Tp,Env),
  typeOfExp(L,PTp,Env,Ev,Exp,Path).
typeOfExp(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),!,
  reportError("Unexpected where `%s' in expression",[ast(P)],Lc),
  typeOfExp(L,Tp,Env,E0,Ptn,Path),
  checkGuard(some(C),E0,Ex,some(Cond),Path).
typeOfExp(Term,Tp,Env,Ev,dot(Lc,Rec,Fld,Tp),Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,Env,Ev,Rec,Path).
typeOfExp(Term,Tp,Env,Ev,U,Path) :-
  isRecordUpdate(Term,Lc,Rc,Fld,Vl),!,
  typeOfRecordUpdate(Lc,Rc,Fld,Vl,Tp,Env,Ev,U,Path).
typeOfExp(Term,Tp,Env,Ev,cond(Lc,Test,Then,Else,Tp),Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  checkGoal(Tst,Env,E0,Test,Path),
  typeOfExp(Th,Tp,E0,E1,Then,Path),
  typeOfExp(El,Tp,Env,E2,Else,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,Env,Ev,case(Lc,Bound,Eqns,Tp),Path) :-
  isCaseExp(Term,Lc,Bnd,Cases),
  checkCaseExp(Lc,Bnd,Cases,Tp,Env,Ev,checker:typeOfExp,Bound,Eqns,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isOpen(Term,Lc,I),!,
  typeOfOpen(Lc,I,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,cell(Lc,Exp),Path) :-
  isRef(Term,Lc,I),
  newTypeVar("r",RT),
  verifyType(Lc,ast(Term),refType(RT),Tp,Env),
  typeOfExp(I,RT,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,deref(Lc,Exp),Path) :-
  isCellRef(Term,Lc,I),
  typeOfExp(I,refType(Tp),Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isTaskTerm(Term,Lc,A),!,
  typeOfTask(Lc,A,Tp,Env,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTuple(Term,Lc,Els),
  \+isComprehension(Term,_,_,_),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTuple(Term,Lc,Els),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkRecordBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  checkRecordBody(FnTp,Fun,Lc,Els,E0,Val,Path).
%  reportMsg("labeled record %s:%s",[can(Val),tpe(Tp)],Lc).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  brceConLbl(Fun,Lbl),
  checkThetaBody(FnTp,Lbl,Lc,Els,E0,Val,Path).
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
  isTuple(Trm,Lc,A),!,
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(Trm),tplType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSlice(Term,Lc,S,F,T),!,
  ternary(Lc,"_slice",S,F,T,Actual),
  typeOfExp(Actual,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  typeOfRoundTerm(Lc,F,A,Tp,Env,Exp,Path).  
typeOfExp(Term,Tp,Env,Env,Lam,Path) :-
  isEquation(Term,_Lc,_H,_R),
  typeOfLambda(Term,Tp,Env,Lam,Path).
typeOfExp(Term,Tp,Env,Ev,sequence(Lc,Fst,Snd),Path) :-
  isSequence(Term,Lc,L,R),!,
  newTypeVar("L",LTp),
  typeOfExp(L,LTp,Env,E1,Fst,Path),
  typeOfExp(R,Tp,E1,Ev,Snd,Path).
typeOfExp(Term,Tp,Env,Ex,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,disj(Lc,Lhs,Rhs),Path) :-
  isDisjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,Env,E2,Rhs,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,Env,Env,implies(Lc,Lhs,Rhs),Path) :-
  isForall(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,_Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Env,neg(Lc,Rhs),Path) :-
  isNegation(Term,Lc,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(R,LogicalTp,Env,_Ex,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,Env,Ev,Lhs,Path),
  typeOfExp(E,TV,Env,_,Rhs,Path).
typeOfExp(Term,Tp,Env,Ev,valof(Lc,doExp(Lc,Act,ResTp),Tp),Path) :-
  isValof(Term,Lc,A),
  isBraceTuple(A,_,[Ac]),!,
  findType("result",Lc,Env,ResTp),
  newTypeVar("E",ER),
  applyTypeFun(ResTp,[ER,Tp],Lc,Env,ATp),
  checkAction(Ac,ATp,Env,Ev,Act,checker:noBindCall,checker:abortCall,Path).
typeOfExp(Term,Tp,Env,Ev,doExp(Lc,Act,Tp),Path) :-
  isDoTerm(Term,Lc,A),!,
  newTypeVar("ETp",ETp),
  newTypeVar("VTp",VTp),
  newTypeFun("R",2,RsltTp),
  applyTypeFun(RsltTp,[ETp,VTp],Lc,Env,RTp),
  verifyType(Lc,ast(Term),Tp,RTp,Env),
  checkAction(A,RTp,Env,Ev,Act,checker:makeBindCall,checker:makeBindCall,Path).
typeOfExp(Term,Tp,Env,Ev,valof(Lc,apply(Lc,PrfFn,tple(Lc,[As]),Tp),Tp),Path) :-
  isValof(Term,Lc,A),!,
  getVar(Lc,"_perform",Env,PrfFn,PerfTp),
  newTypeVar("ATp",ATp),
  verifyType(Lc,ast(A),funType(tplType([ATp]),Tp),PerfTp,Env),
  typeOfExp(A,ATp,Env,Ev,As,Path).
typeOfExp(Term,Tp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

verifyType(Lc,_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Lc,Env),!.
verifyType(Lc,M,S,T,_) :-
  reportError("%s:%s not consistent with expected type\n%s",[M,tpe(S),tpe(T)],Lc).

brceConLbl(over(_,T,_,_),L) :- brceConLbl(T,L).
brceConLbl(v(_,L,_),L).
brceConLbl(cons(_,Nm,_),Nm).
brceConLbl(enm(_,Nm,_),Nm).
brceConLbl(mtd(_,Nm,_),Nm).

typeOfRecordUpdate(Lc,Rc,Fld,Vl,Tp,Env,Ev,update(Lc,Rec,Fld,Val),Path) :-
  typeOfExp(Rc,Tp,Env,Ev0,Rec,Path),
  newTypeVar("_V",VT),
  typeOfExp(Vl,VT,Ev0,Ev,Val,Path).

typeOfOpen(Lc,I,Tp,Env,Ev,open(Lc,Exp,Tp),Path) :-
  typeOfExp(I,Tp,Env,Ev,Exp,Path).
%  reportMsg("opened value %s:%s",[can(Exp),tpe(Tp)],Lc).

typeOfRoundTerm(Lc,F,A,Tp,Env,apply(Lc,Fun,Args,Tp),Path) :-
  newTypeVar("F",FnTp),
  genTpVars(A,Vrs),
  At = tplType(Vrs),
  typeOfExp(F,FnTp,Env,E0,Fun,Path),
  (sameType(funType(At,Tp),FnTp,Lc,E0) ->
     typeOfArgTerm(tuple(Lc,"()",A),At,E0,_Ev,Args,Path);
   sameType(consType(At,Tp),FnTp,Lc,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),At,E0,_Ev,Args,Path);
   reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FnTp,At,Tp],Lc),
   Args = tple(Lc,[])).
					%   reportMsg("after type of %s:%s (%s)",[F,FnTp,Tp]).

typeOfLambda(Term,Tp,Env,lambda(Lc,Lbl,rule(Lc,Args,Guard,Exp),Tp),Path) :-
%  reportMsg("expected type of lambda %s = %s",[Term,Tp]),
  isEquation(Term,Lc,H,C,R),
  newTypeVar("_A",AT),
  typeOfArgPtn(H,AT,Env,E0,Args,Path),
  checkGuard(C,E0,E1,Guard,Path),
  newTypeVar("_E",RT),
  verifyType(Lc,ast(Term),funType(AT,RT),Tp,Env),
  lambdaLbl(Path,"λ",Lbl),
  typeOfExp(R,RT,E1,_,Exp,Path).

typeOfTask(Lc,A,Tp,Env,task(Lc,TskFun,Tp),Path) :-
  findType("task",Lc,Env,TskTp),
  newTypeVar("SComm",SV),
  newTypeVar("RComm",RV),
  applyTypeFun(TskTp,[SV,RV],Lc,Env,TTp),
  roundTuple(Lc,[name(Lc,"this")],Args),
  braceTuple(Lc,[A],AA),
  mkValof(Lc,AA,VlOf),
  mkEquation(Lc,Args,none,VlOf,Lam),
  verifyType(Lc,ast(A),TTp,Tp,Env),
  unitTp(UnitTp),
  LmTp = funType(tplType([Tp]),UnitTp),
  reportMsg("check task lambda %s:%s",[ast(Lam),tpe(LmTp)],Lc),
  typeOfLambda(Lam,LmTp,Env,TskFun,Path).

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

checkDo(Vls,Rsr,A,Tp,Env,Ev,As,Path) :-
  checkAction(A,Tp,Env,Ev,As,Vls,Rsr,Path).

checkAction(A,Tp,Env,Ev,As,Vls,Rsr,Path) :-
  isBraceTuple(A,_,[S]),!,
  checkAction(S,Tp,Env,Ev,As,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Ax,Vls,Rsr,Path) :-
  isDoTerm(A,_,AA),!,
  checkAction(AA,Tp,Env,Ev,Ax,Vls,Rsr,Path).
checkAction(A,Tp,Env,Env,doNop(Lc),_,_,_) :-
  isIden(A,Lc,"nothing"),!,
  unitTp(UnitTp),
  verifyType(Lc,A,UnitTp,Tp,Env).
checkAction(A,Tp,Env,Env,doNop(Lc),_,_,_) :-
  isBraceTuple(A,Lc,[]),!,
  unitTp(UnitTp),
  verifyType(Lc,A,UnitTp,Tp,Env).
checkAction(A,Tp,Env,Ev,doSeq(Lc,L,R),Vls,Rsr,Path) :-
  isActionSeq(A,Lc,A1,A2),!,
  checkAction(A1,Tp,Env,E0,L,Vls,Rsr,Path),
  checkAction(A2,Tp,E0,Ev,R,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Ax,Vls,Rsr,Path) :-
  isActionSeq(A,_,A1),!,
  checkAction(A1,Tp,Env,Ev,Ax,Vls,Rsr,Path).
checkAction(A,Tp,Env,Env,doValis(Lc,ValExp),Vls,_Rsr,Path) :-
  isValis(A,Lc,E),!,
  getVar(Lc,"_valis",Env,ValFn,ValTp),
  newTypeVar("ATp",ATp),
  verifyType(Lc,ast(A),funType(tplType([ATp]),Tp),ValTp,Env),
  call(Vls,Lc,Ex,Tp,ValFn,ValExp),
  typeOfExp(E,ATp,Env,_,Ex,Path).
checkAction(A,Tp,Env,Env,doRaise(Lc,RaiseExp),_Vls,Rsr,Path) :-
  isRaise(A,Lc,E),!,
  getVar(Lc,"_raise",Env,RseFn,RseTp),
  newTypeVar("ETp",ETp),
  verifyType(Lc,ast(A),funType(tplType([ETp]),Tp),RseTp,Env),
  typeOfExp(E,ETp,Env,_,Ex,Path),
  call(Rsr,Lc,Ex,Tp,RseFn,RaiseExp).
checkAction(A,Tp,Env,Env,Act,_Vls,Rsr,Path) :-
  isPerform(A,Lc,A0),!,
  checkPerform(Lc,A0,Tp,Env,Act,Rsr,Path).
checkAction(A,Tp,Env,Ev,Exp,_Vls,Rsr,Path) :-
  isBind(A,Lc,P,E),!,
  (\+ isName(E,_,_) ->
   reportError("expecting a name, not %s",[ast(E)],Lc);
   true),
  checkBind(Lc,P,E,Tp,Env,Ev,Exp,Rsr,Path).
checkAction(A,_Tp,Env,Ev,doMatch(Lc,Ptn,Exp),_Vls,_Rsr,Path) :-
  isMatch(A,Lc,P,E),!,
  newTypeVar("V",TV),
  typeOfPtn(P,TV,Env,Ev,Ptn,Path),
  typeOfExp(E,TV,Env,_,Exp,Path).
checkAction(A,_Tp,Env,Ev,doAssign(Lc,Ptn,Exp),_Vls,_Rsr,Path) :-
  isAssignment(A,Lc,P,E),!,
  newTypeVar("V",TV),
  typeOfExp(P,refType(TV),Env,Ev,Ptn,Path),
  typeOfExp(E,TV,Env,_,Exp,Path).
checkAction(A,Tp,Env,Ev,Act,Vls,Rsr,Path) :-
  isTryCatch(A,Lc,B,H),!,
  checkTryCatch(Lc,B,H,Tp,Env,Ev,Act,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,doIfThenElse(Lc,Tst,Thn,Els),Vls,Rsr,Path) :-
  isIfThenElse(A,Lc,G,T,E),!,
  checkGoal(G,Env,E0,Tst,Path),
  checkAction(T,Tp,E0,E1,Thn,Vls,Rsr,Path),
  checkAction(E,Tp,Env,E2,Els,Vls,Rsr,Path),
  mergeDict(E1,E2,Env,Ev).
checkAction(A,Tp,Env,Env,doIfThen(Lc,Tst,Thn),Vls,Rsr,Path) :-
  isIfThen(A,Lc,G,T),!,
  checkGoal(G,Env,E0,Tst,Path),
  checkAction(T,Tp,E0,_,Thn,Vls,Rsr,Path).
checkAction(A,Tp,Env,Env,doWhile(Lc,Tst,Bdy),Vls,Rsr,Path) :-
  isWhileDo(A,Lc,G,B),!,
  checkGoal(G,Env,E0,Tst,Path),
  checkAction(B,Tp,E0,_,Bdy,Vls,Rsr,Path).
checkAction(A,Tp,Env,Env,doUntil(Lc,Bdy,Tst),Vls,Rsr,Path) :-
  isUntilDo(A,Lc,B,G),!,
  checkGoal(G,Env,_,Tst,Path),
  checkAction(B,Tp,Env,_,Bdy,Vls,Rsr,Path).
checkAction(A,Tp,Env,Env,doFor(Lc,Ptn,Src,Bdy),Vls,Rsr,Path) :-
  isForDo(A,Lc,P,E,B),!,
  newTypeVar("V",TV),
  typeOfPtn(P,TV,Env,E0,Ptn,Path),
  typeOfExp(E,TV,Env,_,Src,Path),
  checkAction(B,Tp,E0,_,Bdy,Vls,Rsr,Path).
checkAction(A,Tp,Env,Env,doLet(Lc,Decls,XDefs,Ac),Vls,Rsr,Path) :-
  isLetDef(A,Lc,D,B),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(ThPath,Lc,D,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  computeLetExport(Defs,[],Decls,XDefs),
  checkAction(B,Tp,OEnv,_,Ac,Vls,Rsr,ThPath).
checkAction(A,Tp,Env,Env,doLetRec(Lc,Decls,XDefs,Ac),Vls,Rsr,Path) :-
  isLetRec(A,Lc,D,B),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(ThPath,Lc,D,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  computeLetExport(Defs,[],Decls,XDefs),
  checkAction(B,Tp,OEnv,_,Ac,Vls,Rsr,ThPath).
checkAction(A,Tp,Env,Env,doCase(Lc,Bound,Eqns,Tp),Vls,Rsr,Path) :-
  isCaseExp(A,Lc,Bnd,Cases),
  checkCases(Lc,Bnd,Cases,Tp,Env,_,checker:checkDo(Vls,Rsr),Bound,Eqns,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Susp,Vls,Rsr,Path) :-
  isSuspend(A,Lc,E,C),!,
  checkSuspend(Lc,name(Lc,"this"),E,Tp,C,Env,Ev,Susp,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Susp,Vls,Rsr,Path) :-
  isSuspend(A,Lc,T,E,C),!,
  checkSuspend(Lc,T,E,Tp,C,Env,Ev,Susp,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Susp,Vls,Rsr,Path) :-
  isResume(A,Lc,T,E,C),!,
  checkResume(Lc,T,E,Tp,C,Env,Ev,Susp,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Susp,Vls,Rsr,Path) :-
  isRetire(A,Lc,E),!,
  unitTp(UnitTp),
  verifyType(Lc,A,UnitTp,Tp,Env),
  checkRetire(Lc,name(Lc,"this"),E,Env,Ev,Susp,Vls,Rsr,Path).
checkAction(A,Tp,Env,Ev,Susp,Vls,Rsr,Path) :-
  isRetire(A,Lc,T,E),!,
  unitTp(UnitTp),
  verifyType(Lc,A,UnitTp,Tp,Env),
  checkRetire(Lc,T,E,Env,Ev,Susp,Vls,Rsr,Path).
% checkAction(A,Tp,Env,Env,doPerform(Lc,Call),_Vls,_Rsr,Path) :-
%   isRoundTerm(A,Lc,F,Args),!,
%   typeOfRoundTerm(Lc,F,Args,Tp,Env,Call,Path).
checkAction(A,Tp,Env,Env,doNop(Lc),_,_,_) :-
  locOfAst(A,Lc),
  reportError("%s:%s illegal form of action",[ast(A),tpe(Tp)],Lc).

makeBindCall(Lc,Ex,Tp,Fn,apply(Lc,Fn,tple(Lc,[Ex]),Tp)) :-!.
noBindCall(_,Ex,_,_,Ex).
abortCall(Lc,Ex,_,_,Ex) :-
  reportError("not permitted to raise %s here",[can(Ex)],Lc).

checkGuard(none,Env,Env,none,_) :-!.
checkGuard(some(G),Env,Ev,some(Goal),Path) :-
  checkGoal(G,Env,Ev,Goal,Path).

checkBind(Lc,P,E,Tp,Env,Ev,Action,Rsr,Path) :-
  newTypeVar("V",TV),
  typeOfPtn(P,TV,Env,Ev,Ptn,Path),

  newTypeVar("BTp",BTp),
  typeOfExp(E,BTp,Env,_,Exp,Path),

  getVar(Lc,"_perform",Env,ValFn,ValTp),
  verifyType(Lc,ast(E),funType(tplType([BTp]),TV),ValTp,Env),
  Vl = apply(Lc,ValFn,tple(Lc,[Exp]),TV),

  getVar(Lc,"_errval",Env,ErrVlFn,RseTp),
  newTypeVar("ETp",ETp),
  verifyType(Lc,ast(E),funType(tplType([BTp]),ETp),RseTp,Env),
  ErrVl = apply(Lc,ErrVlFn,tple(Lc,[Exp]),ETp),

  findType("boolean",Lc,Env,LogicalTp),
  getVar(Lc,"_isOk",Env,OkFn,OkTp),
  verifyType(Lc,ast(E),funType(tplType([BTp]),LogicalTp),OkTp,Env),

  Ok = apply(Lc,OkFn,tple(Lc,[Exp]),LogicalTp),

  getVar(Lc,"_raise",Env,RseFn,RaiseFnTp),
  verifyType(Lc,ast(E),funType(tplType([ETp]),Tp),RaiseFnTp,Env),
  call(Rsr,Lc,ErrVl,Tp,RseFn,Raise),

  Action = doIfThenElse(Lc,Ok,doMatch(Lc,Ptn,Vl),doRaise(Lc,Raise)).

checkPerform(Lc,Env,Tp,Action,Env,Rsr,Path) :-
  newTypeVar("BTp",BTp),
  typeOfExp(E,BTp,Env,_,Exp,Path),

  getVar(Lc,"_errval",Env,ErrVlFn,RseTp),
  newTypeVar("ETp",ETp),
  verifyType(Lc,ast(E),funType(tplType([BTp]),ETp),RseTp,Env),
  ErrVl = apply(Lc,ErrVlFn,tple(Lc,[Exp]),ETp),

  findType("boolean",Lc,Env,LogicalTp),
  getVar(Lc,"_isOk",Env,OkFn,OkTp),
  verifyType(Lc,ast(E),funType(tplType([BTp]),LogicalTp),OkTp,Env),

  Ok = apply(Lc,OkFn,tple(Lc,[Exp]),LogicalTp),

  getVar(Lc,"_raise",Env,RseFn,RaiseFnTp),
  verifyType(Lc,ast(E),funType(tplType([ETp]),Tp),RaiseFnTp,Env),
  call(Rsr,Lc,ErrVl,Tp,RseFn,Raise),

  Action = doIfThen(Lc,neg(Lc,Ok),doRaise(Lc,Raise)).

checkTryCatch(Lc,B,Hs,Tp,Env,Ev,doTryCatch(Lc,Body,Hndlr),Vls,Rsr,Path) :-
  newTypeVar("EE",ETp),
  newTypeVar("VTp",VTp),
  newTypeFun("R",2,RsltTp),
  applyTypeFun(RsltTp,[ETp,VTp],Lc,Env,RTp),
  verifyType(Lc,ast(B),Tp,RTp,Env),
  checkAction(B,Tp,Env,Ev,Body,Vls,checker:noBindCall,Path),
  checkCases(Hs,ETp,Tp,Env,Hndlr,Eqx,Eqx,[],checker:checkDo(Vls,Rsr),Path),!.

checkGoal(Term,Env,Ex,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  checkGoal(L,Env,E1,Lhs,Path),
  checkGoal(R,E1,Ex,Rhs,Path).
checkGoal(Term,Env,Ev,disj(Lc,Lhs,Rhs),Path) :-
  isDisjunct(Term,Lc,L,R),!,
  checkGoal(L,Env,E1,Lhs,Path),
  checkGoal(R,Env,E2,Rhs,Path),
  mergeDict(E1,E2,Env,Ev).
checkGoal(Term,Env,Env,implies(Lc,Lhs,Rhs),Path) :-
  isForall(Term,Lc,L,R),!,
  checkGoal(L,Env,E1,Lhs,Path),
  checkGoal(R,E1,_Ex,Rhs,Path).
checkGoal(Term,Env,Env,neg(Lc,Rhs),Path) :-
  isNegation(Term,Lc,R),!,
  checkGoal(R,Env,_Ex,Rhs,Path).
checkGoal(Term,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isMatch(Term,Lc,P,E),!,
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,Env,Ev,Lhs,Path),
  typeOfExp(E,TV,Env,_,Rhs,Path).
checkGoal(Trm,Env,Ev,Gl,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  checkGoal(Inner,Env,Ev,Gl,Path).
checkGoal(G,Env,Env,Goal,Path) :-
  locOfAst(G,Lc),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,Env,_Ev,Goal,Path).

checkCaseExp(_Lc,Bnd,Cases,Tp,Env,Env,Checker,Bound,Eqns,Path) :-
  newTypeVar("_L",LhsTp),
  typeOfExp(Bnd,LhsTp,Env,_,Bound,Path),
%  reportMsg("case governer: %s:%s",[Bound,LhsTp]),
  checkCases(Cases,LhsTp,Tp,Env,Eqns,Eqx,Eqx,[],Checker,Path),!.

checkCases([],_,_,_,Eqs,Eqs,Dfx,Dfx,_,_).
checkCases([C|Ss],LhsTp,Tp,Env,Eqns,Eqx,Df,Dfx,Checker,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkCase(Lc,L,G,R,LhsTp,Tp,Env,Eqns,Eqs,Df,Df0,Checker,Path),
  checkCases(Ss,LhsTp,Tp,Env,Eqs,Eqx,Df0,Dfx,Checker,Path).

checkCase(Lc,Lhs,G,R,LhsTp,Tp,Env,Eqns,Eqns,Df,Defx,Checker,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkCase(Lc,DLhs,G,R,LhsTp,Tp,Env,Df,Defx,_,_,Checker,Path).
checkCase(Lc,H,G,R,LhsTp,Tp,Env,
	  [rule(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Checker,Path) :-
  typeOfPtn(H,LhsTp,Env,E1,Arg,Path),
  checkGuard(G,E1,E2,Guard,Path),
  call(Checker,R,Tp,E2,_,Exp,Path).

checkSuspend(Lc,T,E,Tp,Cs,Env,Env,doSuspend(Lc,Tsk,Evt,Eqns),_Vls,_Rsr,Path) :-
  findType("task",Lc,Env,TskTp),
  newTypeVar("SComm",SV),
  newTypeVar("RComm",RV),
  applyTypeFun(TskTp,[SV,RV],Lc,Env,TTp),
  typeOfExp(T,TTp,Env,_,Tsk,Path),
  typeOfExp(E,SV,Env,_,Evt,Path),
  checkCases(Cs,RV,Tp,Env,Eqns,Eqx,Eqx,[],checker:checkAction,Path),!.

checkResume(Lc,T,E,Tp,Cs,Env,Env,doResume(Lc,Tsk,Evt,Eqns),Path) :-
  findType("task",Lc,Env,TskTp),
  newTypeVar("SComm",SV),
  newTypeVar("RComm",RV),
  applyTypeFun(TskTp,[SV,RV],Lc,Env,TTp),
  typeOfExp(T,TTp,Env,_,Tsk,Path),
  typeOfExp(E,RV,Env,_,Evt,Path),
  checkCases(Cs,SV,Tp,Env,Eqns,Eqx,Eqx,[],checker:checkAction,Path),!.

checkRetire(Lc,T,E,Env,Env,doRetire(Lc,Tsk,Evt),Path) :-
  findType("task",Lc,Env,TskTp),
  newTypeVar("SComm",SV),
  newTypeVar("RComm",RV),
  applyTypeFun(TskTp,[SV,RV],Lc,Env,TTp),
  typeOfExp(T,TTp,Env,_,Tsk,Path),
  typeOfExp(E,SV,Env,_,Evt,Path).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

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

packageExport(Defs,Public,ExportDecls,LDecls,XDefs) :-
  genDecls(Defs,checker:isPkgPublic(Public),ExportDecls,[],LDecls,[],XDefs,[]).

isPkgPublic(Public,V) :-
  is_member(V,Public),!.

genDecls([],_,Exx,Exx,LDx,LDx,Dfx,Dfx).
genDecls([Def|Defs],Public,Exports,Exx,LDecls,LDx,Dfs,Dfx) :-
  genDecl(Def,Def,Public,Exports,Ex0,LDecls,LD0,Dfs,Df0),
  genDecls(Defs,Public,Ex0,Exx,LD0,LDx,Df0,Dfx).

genDecl(funDef(_,Nm,FullNm,_,Tp,_,_),Def,Public,
	[funDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,[Def|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(funDef(_,Nm,FullNm,_,Tp,_,_),Def,_,Ex,Ex,
	[funDec(Nm,FullNm,Tp)|Lx],Lx,[Def|Dfx],Dfx).
genDecl(typeDef(_,Nm,Tp,TpRule),Def,Public,
	[typeDec(Nm,Tp,TpRule)|Ex],Ex,Lx,Lx,[Def|Dfx],Dfx) :-
  call(Public,tpe(Nm)),!.
genDecl(typeDef(_,Nm,Tp,TpRule),Def,_,Ex,Ex,
	[typeDec(Nm,Tp,TpRule)|Lx],Lx,[Def|Dfx],Dfx).
genDecl(varDef(Lc,Nm,FullNm,[],Tp,lambda(_,_,Eqn),_),_,Public,
	 [funDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,
	 [funDef(Lc,Nm,FullNm,hard,Tp,[],[Eqn])|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(varDef(Lc,Nm,FullNm,[],Tp,lambda(_,_,Eqn),_),_,Public,
	 Ex,Ex,[funDec(Nm,FullNm,Tp)|Lx],Lx,
	 [funDef(Lc,Nm,FullNm,hard,Tp,[],[Eqn])|Dfx],Dfx) :-
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
genDecl(conDef(Nm,CnNm,CnSpec),_,Public,
	[contractDec(Nm,CnNm,CnSpec)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  call(Public,con(Nm)).
genDecl(conDef(Nm,CnNm,CnSpec),_,_,Ex,Ex,
	[contractDec(Nm,CnNm,CnSpec)|Lx],Lx,Dfx,Dfx).
genDecl(implDef(TmpNm,ImplName,ImplVrNm,Spec),_,Public,
	[impDec(ImplName,ImplVrNm,Spec),
	 varDec(ImplVrNm,ImplVrNm,Spec)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  call(Public,imp(TmpNm)),!.
genDecl(implDef(_,ImplName,ImplVrNm,Spec),_,_,Ex,Ex,
	[impDec(ImplName,ImplVrNm,Spec)|Lx],Lx,Dfx,Dfx).
genDecl(accDec(Tp,Fld,AccFn,AccTp),_,Public,
	[accDec(Tp,Fld,AccFn,AccTp)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  exportAcc(Tp,Public).
genDecl(accDec(Tp,Fld,AccFn,AccTp),_,_,Ex,Ex,
	[accDec(Tp,Fld,AccFn,AccTp)|Lx],Lx,Dfx,Dfx).
genDecl(updDec(Tp,Fld,AccFn,AccTp),_,Public,
	[updDec(Tp,Fld,AccFn,AccTp)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  exportAcc(Tp,Public).
genDecl(updDec(Tp,Fld,AccFn,AccTp),_,_,Ex,Ex,
	[updDec(Tp,Fld,AccFn,AccTp)|Lx],Lx,Dfx,Dfx).

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
completePublic([con(Nm)|List],Pub,[tpe(ConNm),var(DtNm)|Px],Path) :-
  contractName(Path,Nm,ConNm),
  dollarName(Nm,DtNm),
  completePublic(List,Pub,Px,Path).
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

mkBoot(Env,Lc,Pkg,Dfs,[BootDef|Dfs],Decls,[funDec("_boot",BootNm,BootTp)|Decls]) :-
  findType("cons",Lc,Env,ConsTp),
  getVar(Lc,"_main",Env,MainTrm,MnTp),
  mangleName(Pkg,value,"_boot",BootNm),
  applyTypeFun(ConsTp,[type("star.core*string")],Lc,Env,LSTp),
  CmdVr = v(Lc,"CmdVr",LSTp),
  unitTp(UnitTp),
  MainTp = funType(tplType([LSTp]),UnitTp),
  sameType(MnTp,MainTp,Lc,Env),
  BootTp = funType(tplType([LSTp]),UnitTp),
  BootEqn = rule(Lc,tple(Lc,[CmdVr]),none,
		 apply(Lc,MainTrm,
		       tple(Lc,[CmdVr]),UnitTp)),
  BootDef = funDef(Lc,"_boot",BootNm,hard,BootTp,[],[BootEqn]).
mkBoot(_Env,_,_,Dfs,Dfs,Decls,Decls).