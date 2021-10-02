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
:- use_module(vartypes).

checkProgram(Prg,Pkg,Repo,_Opts,PkgDecls,Canon) :-
  stdDict(Base),
  isBraceTerm(Prg,Lc,_,Els),
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
  call(Export,var(Nm),P,Px),
  pullOthers(Els,Entries,_Asserts,_Defaults),
  braceTuple(Lc,Entries,Hd),
  reConstrain(XC,Hd,XHd),
  reXQuant(XQ,XHd,QHd),
  binary(Lc,"<=>",QHd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St).
collectConstructors(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,_Export) :-
  isPrivate(C,_,I),
  collectConstructors(I,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,checker:nop).

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
  importDecl(Lc,D,Env,E0),!,
  declareAllDecls(More,Lc,E0,Evx).

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
importDecl(Lc,funDec(Nm,_FullNm,Tp),Env,Evx) :-
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
  unitTp(ErTp),
  typeOfExp(Vr,VrTp,ErTp,Env,E0,Rc,Path),
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
  declareVr(Lc,Nm,Tp,E,Ev).

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
  unitTp(ErTp),
  typeOfArgPtn(A,AT,ErTp,Env,E0,Args,Path),
  checkGuard(C,ErTp,E0,E1,Guard,Path),
  typeOfExp(R,RT,ErTp,E1,_E2,Exp,Path),
  Eqn = rule(Lc,Args,Guard,Exp),
%  reportMsg("rule %s",[Eqn],Lc),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[Eqn|Dfx]; Defs=[Eqn|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("rule not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,Tp,varDef(Lc,Nm,ExtNm,[],Tp,Value),Env,Path) :-
  isIden(L,_,Nm),
  pushScope(Env,E),
  unitTp(ErTp),
  typeOfExp(R,Tp,ErTp,E,_E2,Value,Path),
  packageVarName(Path,Nm,ExtNm).

checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  thetaEnv(Path,Lc,Els,Face,BaseEnv,_,Defs,Public),
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

checkRecordBody(Tp,Lbl,Lc,Els,Env,Val,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
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
  findExportedDefs(Lc,SortedFlds,Args),
  project1(SortedFlds,ElTps),
  Exp = apply(Lc,cons(Lc,Lbl,consType(tplType(ElTps),Tp)),
	      tple(Lc,Args),Tp).

checkLetRec(Tp,ErTp,Lc,Els,Ex,Env,letRec(Lc,Decls,XDefs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  computeLetExport(Defs,[],Decls,XDefs),
  typeOfExp(Ex,Tp,ErTp,OEnv,_,Bound,Path).

checkLetExp(Tp,ErTp,Lc,Els,Ex,Env,letExp(Lc,Decls,XDefs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  computeLetExport(Defs,[],Decls,XDefs),
  typeOfExp(Ex,Tp,ErTp,OEnv,_,Bound,Path).

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
%  dispType(ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
%  dispType(CnSpec),
  getConstraints(CnSpec,AC,contractExists(Spec,_)),
  declareTypeVars(IQ,Lc,Env,E1),
  declareConstraints(Lc,AC,E1,ThEnv),
  implementationName(Spec,ImplName),
  localName(Path,value,ImplName,ImplVrNm),
  contractType(Spec,CnType),
%  dispType(CnType),
  labelImplExp(IBody,ConNm,ImpBody),
%  reportMsg("implementation %s, expected type %s",[ast(ImpBody),tpe(CnType)]),
  typeOfExp(ImpBody,CnType,tplType([]),ThEnv,_ThEv,ImplTerm,ImplVrNm),
  putConstraints(AC,CnType,SS1),
  reQuantTps(SS1,IQ,ImpType),
  ImplVar = varDef(Lc,ImplVrNm,ImplVrNm,AC,ImpType,ImplTerm),
  Impl = implDef(INm,ImplName,ImplVrNm,ImpType),
  declareVr(Lc,ImplVrNm,ImpType,Env,Ev0),
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
typeOfArgPtn(T,Tp,ErTp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,tplType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,ErTp,Env,Ev,Lc,Els,Path).
typeOfArgPtn(T,Tp,ErTp,Env,Ev,Exp,Path) :-
  typeOfPtn(T,Tp,ErTp,Env,Ev,Exp,Path).

typeOfPtn(V,Tp,_ErTp,Env,Env,anon(Lc,Tp),_Path) :-
  isAnon(V,Lc),!.
typeOfPtn(V,Tp,ErTp,Env,Ev,Term,Path) :-
  isIden(V,Lc,N),
  isVar(N,Env,_),!,
  mkWhereEquality(Lc,V,TT),
  typeOfPtn(TT,Tp,ErTp,Env,Ev,Term,Path).
typeOfPtn(V,Tp,_ErTp,Ev,Env,v(Lc,N,Tp),_Path) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,Ev,Env).
typeOfPtn(Trm,Tp,ErTp,Env,Ev,Term,Path) :-
  isEnum(Trm,_,_),
  typeOfExp(Trm,Tp,ErTp,Env,Ev,Term,Path).
typeOfPtn(Trm,Tp,_ErTp,Env,Env,intLit(Lc,Ix),_) :-
  isLiteralInteger(Trm,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  verifyType(Lc,IntTp,Tp,Env).
typeOfPtn(T,Tp,_ErTp,Env,Env,floatLit(Lc,Dx),_Path) :- 
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  verifyType(Lc,FltTp,Tp,Env).
typeOfPtn(chars(Lc,Sx),Tp,_ErTp,Env,Env,charsLit(Lc,Sx),_Path) :- !,
  verifyType(Lc,type("chars"),Tp,Env).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  verifyType(Lc,RT,Tp,Env),
  typeOfPtn(L,Tp,ErTp,Env,Ev,Exp,Path).
typeOfPtn(P,Tp,ErTp,Env,Ev,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),
  typeOfPtn(L,Tp,ErTp,Env,E0,Ptn,Path),
  checkGuard(some(C),ErTp,E0,Ev,some(Cond),Path).
typeOfPtn(Trm,Tp,ErTp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfPtn(Inner,Tp,ErTp,Env,Ev,Exp,Path).
typeOfPtn(Trm,Tp,ErTp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,tplType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,ErTp,Env,Ev,Lc,Els,Path).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Ptn,Path) :-
  isOptionPtn(Term,Lc,Pt,Ex),
  mkWherePtn(Lc,Pt,Ex,Trm),
  typeOfPtn(Trm,Tp,ErTp,Env,Ev,Ptn,Path).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  newTypeVar("A",At),
  typeOfExp(F,consType(At,Tp),ErTp,Env,E0,Fun,Path),
  evidence(At,E0,_,AT),
  typeOfArgPtn(tuple(Lc,"()",A),AT,ErTp,E0,Ev,Args,Path),
  Exp = apply(Lc,Fun,Args,Tp).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  (isBraceTerm(Term,Lc,F,Args);isQBraceTerm(Term,Lc,F,Args)),
  typeOfRecordPtn(Lc,Tp,ErTp,F,Args,Env,Ev,Exp,Path).
typeOfPtn(Term,Tp,_,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal pattern: %s, expecting a %s",[ast(Term),tpe(Tp)],Lc).

typeOfRecordPtn(Lc,Tp,ErTp,F,Args,Env,Ev,Exp,Path) :-
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),ErTp,Env,E0,Fun,Path),
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(E0,E1),
  declareTypeVars(Q,Lc,E1,E2),
  declareConstraints(Lc,Cx,E2,BaseEnv),
  typeOfElementPtns(Args,Face,ErTp,BaseEnv,Ev,PtnDefs,[],Path),
  fillinElementPtns(PtnDefs,Lc,FaceTp,ArgPtns),
  Exp = apply(Lc,Fun,tple(Lc,ArgPtns),Tp).

typeOfElementPtns([],_,_,Env,Env,Defs,Defs,_Path).
typeOfElementPtns([E|Els],Face,ErTp,Env,Ev,Defs,Dfx,Path) :-
  elementPtn(E,ErTp,Face,Env,E0,Defs,Df0,Path),
  typeOfElementPtns(Els,Face,ErTp,E0,Ev,Df0,Dfx,Path).

elementPtn(E,ErTp,Face,Env,Ev,[(Nm,Ptn)|Defs],Defs,Path) :-
  isDefn(E,Lc,Lhs,Rhs),
  isIden(Lhs,_,Nm),
  fieldInFace(Face,Nm,Lc,Tp),
  typeOfPtn(Rhs,Tp,ErTp,Env,Ev,Ptn,Path).

fillinElementPtns(Els,Lc,faceType(Flds,_),Args) :-
  rfold(Flds,checker:fillinElementPtn(Lc),Els,NEls),
  sort(NEls,checker:cmpPair,Elements),
  project1(Elements,Args).

cmpPair((N1,_),(N2,_)) :-
  str_lt(N1,N2).

fillinElementPtn(_,(Nm,_),Els,Els) :-
  is_member((Nm,_),Els) ,!.
fillinElementPtn(Lc,(Nm,Tp),Els,[(Nm,anon(Lc,Tp))|Els]).
  
typeOfArgTerm(T,Tp,ErTp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,tplType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,ErTp,Env,Ev,Lc,Els,Path).
typeOfArgTerm(T,Tp,ErTp,Env,Ev,Exp,Path) :-
  typeOfExp(T,Tp,ErTp,Env,Ev,Exp,Path).

typeOfExp(V,Tp,_ErTp,Env,Env,anon(Lc,Tp),_) :-
  isAnon(V,Lc),
  reportError("anonymous variable not permitted as expression",[],Lc).
typeOfExp(V,Tp,_ErTp,Env,Ev,Term,_Path) :-
  isIden(V,Lc,N),!,
  (getVar(Lc,N,Env,Ev,Term) ->
   typeOfCanon(Term,VTp),
   verifyType(Lc,Tp,VTp,Ev);
   reportError("variable '%s' not defined, expecting a %s",[V,Tp],Lc),
   Term=void,
   Env=Ev).
typeOfExp(T,Tp,ErTp,Env,Ev,Term,Path) :-
  isEnum(T,_,N),
  typeOfExp(N,Tp,ErTp,Env,Ev,Term,Path),!.
typeOfExp(T,Tp,_ErTp,Env,Env,intLit(Lc,Ix),_Path) :-
  isLiteralInteger(T,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  verifyType(Lc,IntTp,Tp,Env).
typeOfExp(T,Tp,_ErTp,Env,Env,floatLit(Lc,Dx),_Path) :-
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  verifyType(Lc,FltTp,Tp,Env).
typeOfExp(chars(Lc,Sx),Tp,_ErTp,Env,Env,charsLit(Lc,Sx),_Path) :- !,
  findType("chars",Lc,Env,StrTp),
  verifyType(Lc,StrTp,Tp,Env).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  verifyType(Lc,RT,Tp,Env),
  typeOfExp(L,Tp,ErTp,Env,Ev,Exp,Path).
typeOfExp(P,Tp,ErTp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),!,
  reportError("Unexpected where `%s' in expression",[ast(P)],Lc),
  typeOfExp(L,Tp,ErTp,Env,E0,Ptn,Path),
  checkGuard(some(C),ErTp,E0,Ex,some(Cond),Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  recordAccessExp(Lc,Rc,Fld,Tp,ErTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,replace(Lc,LExp,RExp),Path) :-
  isRecordUpdate(Term,Lc,Lft,Rgt),!,
  typeOfExp(Lft,Tp,ErTp,Env,Ev,LExp,Path),
  newTypeVar("_r",R),
  typeOfExp(Rgt,R,ErTp,Env,_,RExp,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,cond(Lc,Test,Then,Else,Tp),Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  checkGoal(Tst,ErTp,Env,E0,Test,Path),
  typeOfExp(Th,Tp,ErTp,E0,E1,Then,Path),
  typeOfExp(El,Tp,ErTp,Env,E2,Else,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  isCaseExp(Term,Lc,Bnd,Cases),
  checkCaseExp(Lc,Bnd,Cases,Tp,ErTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,throw(Lc,Exp,Tp),Path) :-
  isThrow(Term,Lc,E),!,
  typeOfExp(E,ErTp,ErTp,Env,_,Exp,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,cell(Lc,Exp),Path) :-
  isRef(Term,Lc,I),
  newTypeVar("r",RT),
  verifyType(Lc,refType(RT),Tp,"expecting a ref type"),
  typeOfExp(I,RT,ErTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,deref(Lc,Exp),Path) :-
  isCellRef(Term,Lc,I),
  typeOfExp(I,refType(Tp),ErTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Action,Path) :-
  isDoTerm(Term,Lc,Stmts),!,
  newTypeVar("E",ErTp),
%  dispType(Tp),
  typeOfDoExp(Lc,Stmts,Tp,ErTp,Env,Action,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Action,Path) :-
  isActionTerm(Term,Lc,Stmts),!,
  typeOfActionExp(Lc,Stmts,Tp,Env,Action,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Action,Path) :-
  isTaskTerm(Term,Lc,Stmts),!,
  typeOfTaskExp(Lc,Stmts,Tp,Env,Action,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Val,Path) :-
  isBraceTuple(Term,Lc,Els),
  \+isComprehension(Term,_,_,_),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Val,Path) :-
  isQBraceTuple(Term,Lc,Els),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkRecordBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,Val,Path) :-
  isBraceTerm(Term,Lc,F,Els),
  typeOfBraceTerm(Lc,F,Els,Tp,ErTp,Env,Env,Val,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,Val,Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  typeOfQBraceTerm(Lc,F,Els,Tp,ErTp,Env,Env,Val,Path).
typeOfExp(Term,Tp,ErTp,Ev,Ev,LetExp,Path) :-
  isLetDef(Term,Lc,Els,Ex),
  checkLetExp(Tp,ErTp,Lc,Els,Ex,Ev,LetExp,Path).
typeOfExp(Term,Tp,ErTp,Ev,Ev,LetExp,Path) :-
  isLetRec(Term,Lc,Els,Ex),
  checkLetRec(Tp,ErTp,Lc,Els,Ex,Ev,LetExp,Path).
typeOfExp(Trm,Tp,ErTp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfExp(Inner,Tp,ErTp,Env,Ev,Exp,Path).
typeOfExp(Trm,Tp,ErTp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),!,
  genTpVars(A,ArgTps),
  verifyType(Lc,tplType(ArgTps),Tp,Env),
  typeOfTerms(A,ArgTps,ErTp,Env,Ev,Lc,Els,Path).
typeOfExp(Trm,Tp,_ErTp,Env,Env,Exp,_Path) :-
  isTag(Trm,Lc),!,
  typeOfTag(Lc,Tp,Env,Exp).
typeOfExp(Trm,Tp,ErTp,Env,Env,Exp,Path) :-
  isPrompt(Trm,Lc,L,P),!,
  typeOfPrompt(Lc,L,P,Tp,ErTp,Env,Exp,Path).
typeOfExp(Trm,Tp,ErTp,Env,Env,Shift,Path) :-
  isCut(Trm,Lc,L,Lhs,Rhs),!,
  typeOfCut(Lc,L,Lhs,Rhs,Tp,ErTp,Env,Shift,Path).
typeOfExp(Trm,_Tp,ErTp,Env,Env,resume(Lc,Kont,Arg,RTp),Path) :-
  isResume(Trm,Lc,F,A),!,
  newTypeVar("R",RTp),
  newTypeVar("A",At),
  KTp = contType(tplType([At]),RTp),
  typeOfExp(F,KTp,ErTp,Env,_,Kont,Path),
  typeOfExp(A,At,ErTp,Env,_,Arg,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path) :-
  isSlice(Term,Lc,S,F,T),!,
  ternary(Lc,"_slice",S,F,T,Actual),
  typeOfExp(Actual,Tp,ErTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  typeOfRoundTerm(Lc,F,A,Tp,ErTp,Env,Exp,Path).  
typeOfExp(Term,Tp,_,Env,Env,Lam,Path) :-
  isEquation(Term,_Lc,_H,_R),
  typeOfLambda(Term,Tp,Env,Lam,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,sequence(Lc,Fst,Snd),Path) :-
  isSequence(Term,Lc,L,R),!,
  newTypeVar("L",LTp),
  typeOfExp(L,LTp,ErTp,Env,E1,Fst,Path),
  typeOfExp(R,Tp,ErTp,E1,Ev,Snd,Path).
typeOfExp(Term,Tp,ErTp,Env,Ex,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,ErTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,ErTp,E1,Ex,Rhs,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,disj(Lc,Lhs,Rhs),Path) :-
  isDisjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,ErTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,ErTp,Env,E2,Rhs,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,ErTp,Env,Env,implies(Lc,Lhs,Rhs),Path) :-
  isForall(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,ErTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,ErTp,E1,_Ex,Rhs,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,neg(Lc,Rhs),Path) :-
  isNegation(Term,Lc,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,LogicalTp,Tp,Env),
  typeOfExp(R,LogicalTp,ErTp,Env,_Ex,Rhs,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,ErTp,Env,Ev,Lhs,Path),
  typeOfExp(E,TV,ErTp,Env,_,Rhs,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,void,_) :-
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

funLbl(over(_,T,_,_),L) :- funLbl(T,L).
funLbl(v(_,L,_),L).
funLbl(cons(_,Nm,_),Nm).
funLbl(enm(_,Nm,_),Nm).
funLbl(mtd(_,Nm,_),Nm).

typeOfRoundTerm(Lc,F,A,Tp,ErTp,Env,apply(Lc,Fun,Args,Tp),Path) :-
  newTypeVar("F",FnTp),
  genTpVars(A,Vrs),
  At = tplType(Vrs),
  typeOfExp(F,FnTp,ErTp,Env,E0,Fun,Path),
  (sameType(funType(At,Tp),FnTp,Lc,E0) ->
     typeOfArgTerm(tuple(Lc,"()",A),At,ErTp,E0,_Ev,Args,Path);
   sameType(consType(At,Tp),FnTp,Lc,E0) ->
    typeOfArgTerm(tuple(Lc,"()",A),At,ErTp,E0,_Ev,Args,Path);
   reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FnTp,At,Tp],Lc),
   Args = tple(Lc,[])).
					%   reportMsg("after type of %s:%s (%s)",[F,FnTp,Tp]).

typeOfBraceTerm(Lc,F,Els,Tp,ErTp,Env,Env,Val,Path) :-
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),ErTp,Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
%  dispType(consType(FnTp,Tp)),
  checkThetaBody(FnTp,Lbl,Lc,Els,E0,Val,Path).

typeOfQBraceTerm(Lc,F,Els,Tp,ErTp,Env,Env,Val,Path) :-
  newTypeVar("R",FnTp),
  typeOfExp(F,consType(FnTp,Tp),ErTp,Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  checkRecordBody(FnTp,Lbl,Lc,Els,E0,Val,Path).

typeOfLambda(Term,Tp,Env,lambda(Lc,Lbl,rule(Lc,Args,Guard,Exp),Tp),Path) :-
%  reportMsg("expected type of lambda %s = %s",[Term,Tp]),
  isEquation(Term,Lc,H,C,R),
  newTypeVar("_A",AT),
  unitTp(ErTp),
  typeOfArgPtn(H,AT,ErTp,Env,E0,Args,Path),
  checkGuard(C,ErTp,E0,E1,Guard,Path),
  newTypeVar("_E",RT),
  verifyType(Lc,funType(AT,RT),Tp,Env),
  lambdaLbl(Path,"λ",Lbl),
  typeOfExp(R,RT,ErTp,E1,_,Exp,Path).

typeOfTag(Lc,Tp,Env,tag(Lc,Tp)) :-
  newTypeVar("_",Rt),
  newTypeVar("_",Ct),
  mkTypeExp(tpFun("tag",2),[Rt,Ct],TTp),
  verifyType(Lc,TTp,Tp,Env).

typeOfPrompt(Lc,L,P,Tp,ErTp,Env,prompt(Lc,Lb,Lam,Tp),Path) :-
  newTypeVar("_",Rt),
  mkTypeExp(tpFun("tag",2),[Rt,Tp],TTp),
  typeOfExp(L,TTp,ErTp,Env,_,Lb,Path),
%  dispType(TTp),
  typeOfExp(P,Tp,ErTp,Env,_,Exp,Path),
  lambdaLbl(Path,"ç",Lbl),
  Lam = lambda(Lc,Lbl,rule(Lc,tple(Lc,[]),none,Exp),
	       funType(tplType([]),Tp)).

typeOfCut(Lc,L,Lhs,Rhs,Tp,ErTp,Env,shift(Lc,Lb,Lam),Path) :-
%  dispType(Tp),
  newTypeVar("_",Rt),
  newTypeVar("_",Ct),
  KType = contType(tplType([Tp]),Rt),
  mkTypeExp(tpFun("tag",2),[Tp,Ct],TTp),
%  dispType(TTp),
  typeOfExp(L,TTp,ErTp,Env,_,Lb,Path),
%  dispType(TTp),
  typeOfPtn(Lhs,KType,ErTp,Env,E0,V,Path),
%  dispType(KType),
%  dispType(Ct),
  typeOfExp(Rhs,Ct,ErTp,E0,_,Exp,Path),
  lambdaLbl(Path,"ç",Lbl),
  Lam = lambda(Lc,Lbl,rule(Lc,tple(Lc,[V]),none,Exp),
	       funType(tplType([KType]),Rt)).
%  dispCanon(Lam).

typeOfDoExp(Lc,Stmts,Tp,ErTp,Env,doTerm(Lc,Act,Tp),Path) :-
  findType("result",Lc,Env,ResltTp),
  getVar(Lc,"_valis",Env,_,OkFn),
  typeOfCanon(OkFn,OkFnTp),
  getVar(Lc,"_raise",Env,_,EvtFn),
  typeOfCanon(EvtFn,EvtFnTp),
  newTypeVar("X",ElTp),
  applyTypeFun(ResltTp,[ErTp,ElTp],Lc,Env,RTp),
  verifyType(Lc,RTp,Tp,Env),
  verifyType(Lc,funType(tplType([ElTp]),RTp),OkFnTp,Env),
  verifyType(Lc,funType(tplType([ErTp]),RTp),EvtFnTp,Env),
  checkAction(Stmts,Env,_Ev,ResltTp,ElTp,ErTp,OkFn,EvtFn,Act,Path).

typeOfActionExp(Lc,Stmts,Tp,Env,Action,Path) :-
  findType("action",Lc,Env,ActionTp),
  findType("result",Lc,Env,ResltTp),
  getVar(Lc,"action",Env,_,ActFn),
  newTypeVar("X",ElTp),
  newTypeVar("E",ErTp),
  applyTypeFun(ActionTp,[ErTp,ElTp],Lc,Env,MTp),
  applyTypeFun(ResltTp,[ErTp,ElTp],Lc,Env,RTp),
  verifyType(Lc,MTp,Tp,Env),
  typeOfCanon(ActFn,ActConsTp),
  LamTp = funType(tplType([]),RTp),
  verifyType(Lc,consType(tplType([LamTp]),MTp),ActConsTp,Env),
  lambdaLbl(Path,"λ",LamLbl),
  typeOfDoExp(Lc,Stmts,RTp,ErTp,Env,Act,Path),
  Action = apply(Lc,ActFn,
		 tple(Lc,
		      [lambda(Lc,LamLbl,
			      rule(Lc,tple(Lc,[]),none,Act),
			      LamTp)]),Tp).

typeOfTaskExp(Lc,Stmts,Tp,Env,taskTerm(Lc,TaskLbl,Action,Tp),Path) :-
  findType("task",Lc,Env,TaskTp), % action type is just the core
  findType("result",Lc,Env,ResltTp),
  getVar(Lc,"task",Env,_,ActFn),
  newTypeVar("E",ErTp),
  newTypeVar("X",ElTp),
  applyTypeFun(TaskTp,[ErTp,ElTp],Lc,Env,TTp),
  applyTypeFun(ResltTp,[ErTp,ElTp],Lc,Env,RTp),
  verifyType(Lc,TTp,Tp,Env),
  LamTp = funType(tplType([]),RTp),
  typeOfCanon(ActFn,ActConsTp),
  verifyType(Lc,consType(tplType([LamTp]),Tp),ActConsTp,Env),
%  dispType(ActConsTp),
  lambdaLbl(Path,"∂",ActLbl),
  typeOfDoExp(Lc,Stmts,RTp,ErTp,Env,Act,Path),
  lambdaLbl(Path,"𝜏",TaskLbl),
  Action = apply(Lc,ActFn,
		 tple(Lc,
		      [lambda(Lc,ActLbl,
			      rule(Lc,tple(Lc,[]),none,Act),
			      LamTp)]),Tp).

typeOfIndex(Lc,Mp,Arg,Tp,ErTp,Env,Ev,Exp,Path) :-
  isBinary(Arg,_,"->",Ky,Vl),!,
  ternary(Lc,"_put",Mp,Ky,Vl,Term),
  typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,ErTp,Env,Ev,Exp,Path) :-
  isNegation(Arg,_,Ky),!,
  binary(Lc,"_remove",Mp,Ky,Term),
  typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path).
typeOfIndex(Lc,Mp,Arg,Tp,ErTp,Env,Ev,Exp,Path) :-
  binary(Lc,"_index",Mp,Arg,Term),
  typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Path).

pickupContract(Lc,Env,Nm,StTp,DpTps,Op) :-
  (getContract(Nm,Env,conDef(_,_,_,Con)) ->
   freshen(Con,Env,_,contractExists(conTract(Op,[StTp],DpTps),_));
   reportError("%s contract not defined",[Nm],Lc),
   newTypeVar("_St",StTp),
   DpTps=[]).

checkGuard(none,_,Env,Env,none,_) :-!.
checkGuard(some(G),ErTp,Env,Ev,some(Goal),Path) :-
  checkGoal(G,ErTp,Env,Ev,Goal,Path).

checkGoal(Term,ErTp,Env,Ex,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  checkGoal(L,ErTp,Env,E1,Lhs,Path),
  checkGoal(R,ErTp,E1,Ex,Rhs,Path).
checkGoal(Term,ErTp,Env,Ev,disj(Lc,Lhs,Rhs),Path) :-
  isDisjunct(Term,Lc,L,R),!,
  checkGoal(L,ErTp,Env,E1,Lhs,Path),
  checkGoal(R,ErTp,Env,E2,Rhs,Path),
  mergeDict(E1,E2,Env,Ev).
checkGoal(Term,ErTp,Env,Env,implies(Lc,Lhs,Rhs),Path) :-
  isForall(Term,Lc,L,R),!,
  checkGoal(L,ErTp,Env,E1,Lhs,Path),
  checkGoal(R,ErTp,E1,_Ex,Rhs,Path).
checkGoal(Term,ErTp,Env,Env,neg(Lc,Rhs),Path) :-
  isNegation(Term,Lc,R),!,
  checkGoal(R,ErTp,Env,_Ex,Rhs,Path).
checkGoal(Term,ErTp,Env,Ev,match(Lc,Lhs,Rhs),Path) :-
  isMatch(Term,Lc,P,E),!,
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,ErTp,Env,Ev,Lhs,Path),
  typeOfExp(E,TV,ErTp,Env,_,Rhs,Path).
checkGoal(Trm,ErTp,Env,Ev,Gl,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  checkGoal(Inner,ErTp,Env,Ev,Gl,Path).
checkGoal(G,ErTp,Env,Env,Goal,Path) :-
  locOfAst(G,Lc),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,ErTp,Env,_Ev,Goal,Path).

checkCaseExp(Lc,Bnd,Cases,Tp,ErTp,Env,Env,case(Lc,Bound,Eqns,Tp),Path) :-
  newTypeVar("_L",LhsTp),
  typeOfExp(Bnd,LhsTp,ErTp,Env,_,Bound,Path),
%  reportMsg("case governer: %s:%s",[Bound,LhsTp]),
  checkCases(Cases,LhsTp,Tp,ErTp,Env,Eqns,Eqx,Eqx,[],Path),!.

checkCases([],_,_,_,_,Eqs,Eqs,Dfx,Dfx,_).
checkCases([C|Ss],LhsTp,Tp,ErTp,Env,Eqns,Eqx,Df,Dfx,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkCase(Lc,L,G,R,LhsTp,Tp,ErTp,Env,Eqns,Eqs,Df,Df0,Path),
  checkCases(Ss,LhsTp,Tp,ErTp,Env,Eqs,Eqx,Df0,Dfx,Path).

checkCase(Lc,Lhs,G,R,LhsTp,Tp,ErTp,Env,Eqns,Eqns,Df,Defx,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkCase(Lc,DLhs,G,R,LhsTp,Tp,ErTp,Env,Df,Defx,_,_,Path).
checkCase(Lc,H,G,R,LhsTp,Tp,ErTp,Env,
	  [rule(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Path) :-
  typeOfPtn(H,LhsTp,ErTp,Env,E1,Arg,Path),
  checkGuard(G,ErTp,E1,E2,Guard,Path),
  typeOfExp(R,Tp,ErTp,E2,_,Exp,Path).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

checkAction(Term,Env,Env,_,VlTp,_,_,_,noDo(Lc),_) :-
  isIden(Term,Lc,"nothing"),!,
  verifyType(Lc,tplType([]),VlTp,Env).
checkAction(Term,Env,Env,_,VlTp,_ErTp,_,_,noDo(Lc),_Path) :-
  isBraceTuple(Term,Lc,[]),!,
  verifyType(Lc,tplType([]),VlTp,Env).
checkAction(Term,Env,Ev,Tp,VlTp,ErTp,OkFn,EvtFn,seqDo(Lc,A1,A2),Path) :-
  isActionSeq(Term,Lc,S1,S2),!,
  checkAction(S1,Env,E1,Tp,tplType([]),ErTp,OkFn,EvtFn,A1,Path),
  checkAction(S2,E1,Ev,Tp,VlTp,ErTp,OkFn,EvtFn,A2,Path).
checkAction(Term,Env,Ev,_Tp,VlTp,ErTp,_,_,varDo(Lc,Ptn,Exp),Path) :-
  isMatch(Term,Lc,P,Ex),!,
  verifyType(Lc,tplType([]),VlTp,Env),
  newTypeVar("_P",PT),
  typeOfPtn(P,PT,ErTp,Env,Ev,Ptn,Path),
  typeOfExp(Ex,PT,ErTp,Env,_,Exp,Path).
checkAction(Term,Env,Ev,_Tp,VlTp,ErTp,_OkFn,_EvtFn,Act,Path) :-
  isAssignment(Term,Lc,L,R),!,
  checkAssignment(Lc,L,R,Env,Ev,VlTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,XTp,VlTp,ErTp,_OkFn,_EvtFn,Act,Path) :-
  isBind(Term,Lc,L,R),!,
  checkBind(Lc,L,R,Env,Ev,XTp,VlTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,Tp,VlTp,ErTp,OkFn,EvtFn,ifThenDo(Lc,Ts,Th,El),Path) :-
  isIfThenElse(Term,Lc,T,H,E),!,
  checkGoal(T,ErTp,Env,Et,Ts,Path),
  checkAction(H,Et,E1,Tp,VlTp,ErTp,OkFn,EvtFn,Th,Path),
  checkAction(E,Env,E2,Tp,VlTp,ErTp,OkFn,EvtFn,El,Path),
  mergeDict(E1,E2,Env,Ev).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,ifThenDo(Lc,Ts,Th,noDo(Lc)),Path) :-
  isIfThen(Term,Lc,T,H),!,
  checkGoal(T,ErTp,Env,Et,Ts,Path),
  verifyType(Lc,tplType([]),VlTp,Env),
  checkAction(H,Et,_E1,Tp,VlTp,ErTp,OkFn,EvtFn,Th,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,whileDo(Lc,Ts,Bdy),Path) :-
  isWhileDo(Term,Lc,T,B),!,
  unitTp(UnitTp),
  verifyType(Lc,UnitTp,VlTp,Env),
  checkGoal(T,ErTp,Env,Ev,Ts,Path),
  checkAction(B,Ev,_,Tp,UnitTp,ErTp,OkFn,EvtFn,Bdy,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,untilDo(Lc,Ts,Bdy),Path) :-
  isUntilDo(Term,Lc,B,T),!,
  verifyType(Lc,tplType([]),VlTp,Env),
  checkAction(B,Env,Ev,Tp,tplType([]),ErTp,OkFn,EvtFn,Bdy,Path),
  checkGoal(T,ErTp,Ev,_,Ts,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,tryCatchDo(Lc,Bdy,Hndlr),Path) :-
  isTryCatch(Term,Lc,B,H),!,
  anonVar(Lc,Anon,BdErTp),
  checkAction(B,Env,_,Tp,VlTp,BdErTp,OkFn,EvtFn,Bdy,Path),
  checkCatch(H,Env,Tp,VlTp,ErTp,BdErTp,Anon,OkFn,EvtFn,Hndlr,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,tryHandleDo(Lc,Tag,Bdy,Hndlr),Path) :-
  isTryHandle(Term,Lc,B,H),!,
  anonVar(Lc,Anon,BdErTp),
  newTypeVar("Tg",TgTp),
  typeOfTag(Lc,TgTp,Tag),
  declareVr(Lc,"$tag",TgTp,Env,Ev0),
  checkAction(B,Ev0,_,Tp,VlTp,BdErTp,OkFn,EvtFn,Bdy,Path),
  checkHandle(H,Ev0,TgTp,Tp,VlTp,ErTp,BdErTp,Anon,OkFn,EvtFn,Hndlr,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,_OkFn,EvtFn,throwDo(Lc,Evt),Path) :-
  isThrow(Term,Lc,E),!,
  unitTp(Unit),
  typeOfExp(E,ErTp,Unit,Env,_,Exp,Path),
  applyTypeFun(Tp,[ErTp,VlTp],Lc,Env,RTp),
  Evt=apply(Lc,EvtFn,tple(Lc,[Exp]),RTp).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,_EvtFn,valisDo(Lc,Reslt),Path) :-
  isValis(Term,Lc,Ex),!,
  typeOfExp(Ex,VlTp,ErTp,Env,_,Exp,Path),
  applyTypeFun(Tp,[ErTp,VlTp],Lc,Env,RTp),
  Reslt=apply(Lc,OkFn,tple(Lc,[Exp]),RTp).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,caseDo(Lc,Gov,Cases),Path) :-
  isCaseExp(Term,Lc,Gv,Cs),!,
  newTypeVar("_G",GTp),
  typeOfExp(Gv,GTp,ErTp,Env,_,Gov,Path),
  checkActionCases(Cs,GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Cases,Dflt,Dflt,[],Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,_OkFn,_EvtFn,performDo(Lc,Act),Path) :-
  isPerform(Term,Lc,Arg),!,
  verifyType(Lc,tplType([]),VlTp,Env),
  newTypeVar("T",ATp),
  applyTypeFun(Tp,[ErTp,ATp],Lc,Env,RTp),
  typeOfExp(Arg,RTp,ErTp,Env,_Ev,Act,Path).
checkAction(Term,Env,Env,_Tp,VlTp,ErTp,_,_,varDo(Lc,Anon,Exp),Path) :-
  isIgnore(Term,Lc,Ex),!,
  verifyType(Lc,tplType([]),VlTp,Env),
  anonVar(Lc,Anon,PT),
  typeOfExp(Ex,PT,ErTp,Env,_,Exp,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Action,Path) :-
  isPrompt(Term,Lc,L,P),!,
  checkPromptAction(Lc,L,P,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Action,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Action,Path) :-
  isCut(Term,Lc,L,Lhs,Rhs),
  checkCutAction(Lc,L,Lhs,Rhs,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Action,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Action,Path) :-
  isResume(Term,Lc,F,A),!,
  checkResumeAction(Lc,F,A,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Action,Path).
checkAction(Term,Env,Ev,Tp,VlTp,ErTp,OkFn,EvtFn,Exp,Path) :-
  isBraceTuple(Term,_,[Stmt]),!,
  checkAction(Stmt,Env,Ev,Tp,VlTp,ErTp,OkFn,EvtFn,Exp,Path).
checkAction(Term,Env,Env,Tp,VlTp,ErTp,_,_,simpleDo(Lc,Exp),Path) :-
  isRoundTerm(Term,Lc,F,A),!,
  applyTypeFun(Tp,[ErTp,VlTp],Lc,Env,RTp),
  typeOfRoundTerm(Lc,F,A,RTp,ErTp,Env,Exp,Path).
checkAction(Term,Env,Env,_,_,_,_,_,noDo(Lc),_) :-
  locOfAst(Term,Lc),
  reportError("invalid action: %s",[ast(Term)],Lc).

checkPromptAction(Lc,L,P,Env,Tp,VlTp,ErTp,OkFn,EvtFn,promptDo(Lc,Lb,Lam,Tp),Path) :-
  verifyType(Lc,tplType([]),VlTp,Env),
  newTypeVar("_",Rt),
  applyTypeFun(Tp,[ErTp,VlTp],Lc,Env,RTp),
  applyTypeFun(tpFun("tag",2),[Rt,RTp],Lc,Env,TTp),
  typeOfExp(L,TTp,ErTp,Env,_,Lb,Path),
  dispType(TTp),
  checkAction(P,Env,_,Tp,VlTp,ErTp,OkFn,EvtFn,Act,Path),
  lambdaLbl(Path,"ç",Lbl),
  Lam = lambda(Lc,Lbl,rule(Lc,tple(Lc,[]),none,doTerm(Lc,Act,Tp)),
	       funType(tplType([]),Tp)).

checkCutAction(Lc,L,Lhs,Rhs,Env,Tp,VlTp,ErTp,OkFn,EvtFn,cutDo(Lc,Lb,Lam),Path) :-
  newTypeVar("_",Rt),
  newTypeVar("_",Ct),
  KType = contType(tplType([Tp]),Rt),
  mkTypeExp(tpFun("tag",2),[Tp,Ct],TTp),
  dispType(TTp),
  typeOfExp(L,TTp,ErTp,Env,_,Lb,Path),
  dispType(TTp),
  typeOfPtn(Lhs,KType,ErTp,Env,E0,V,Path),
  dispType(TTp),
  checkAction(Rhs,E0,_,Tp,VlTp,ErTp,OkFn,EvtFn,Act,Path),
  lambdaLbl(Path,"ç",Lbl),
  Lam = lambda(Lc,Lbl,rule(Lc,tple(Lc,[V]),none,doTerm(Lc,Act,Tp)),
	       funType(tplType([KType]),Rt)),
  dispCanon(Lam).

checkResumeAction(Lc,F,A,Env,Tp,VlTp,ErTp,_OkFn,_EvtFn,resumeDo(Lc,Kont,Arg,RTp),Path) :-
  verifyType(Lc,tplType([]),VlTp,Env),
  applyTypeFun(Tp,[ErTp,VlTp],Lc,Env,RTp),
  newTypeVar("A",At),
  KTp = contType(At,RTp),
  typeOfExp(F,KTp,ErTp,Env,_,Kont,Path),
  typeOfArgTerm(A,At,ErTp,Env,_,Arg,Path),
  dispType(KTp).

checkAssignment(Lc,L,R,Env,Ev,VlTp,ErTp,assignDo(Lc,Lhs,Rhs),Path) :-
  verifyType(Lc,tplType([]),VlTp,Env),
  newTypeVar("A",ATp),
  typeOfExp(L,refType(ATp),ErTp,Env,Ev,Lhs,Path),
  typeOfExp(R,ATp,ErTp,Env,_,Rhs,Path).

checkBind(Lc,L,R,Env,E1,XTp,VlTp,ErTp,bindDo(Lc,Lhs,Rhs),Path) :-
  verifyType(Lc,tplType([]),VlTp,Env),
  newTypeVar("A",ATp),
  typeOfPtn(L,ATp,ErTp,Env,E1,Lhs,Path),
  applyTypeFun(XTp,[ErTp,ATp],Lc,Env,RTp),
  typeOfExp(R,RTp,ErTp,Env,_,Rhs,Path).

checkActionCases([],_,_,_,_,_,_,_,Cases,Cases,Dfx,Dfx,_).
checkActionCases([C|Ss],GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Cases,Cx,Df,Dfx,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkActionCase(Lc,L,G,R,GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Cases,C0,Df,Df0,Path),
  checkActionCases(Ss,GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,C0,Cx,Df0,Dfx,Path).

checkActionCase(Lc,Lhs,G,R,GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Cases,Cases,Df,Dfx,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkActionCase(Lc,DLhs,G,R,GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,Df,Dfx,_,_,Path).
checkActionCase(Lc,H,G,R,GTp,Env,Tp,VlTp,ErTp,OkFn,EvtFn,
		[rule(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Path) :-
  typeOfPtn(H,GTp,ErTp,Env,E1,Arg,Path),
  checkGuard(G,ErTp,E1,E2,Guard,Path),
  checkAction(R,E2,_,Tp,VlTp,ErTp,OkFn,EvtFn,Exp,Path).

checkCatch(Term,Env,MdTp,VlTp,ErTp,BdErTp,_,_,_,Hndlr,Path) :-
  locOfAst(Term,Lc),
  applyTypeFun(MdTp,[ErTp,VlTp],Lc,Env,RTp),
  typeOfExp(Term,funType(tplType([BdErTp]),RTp),ErTp,Env,_,Hndlr,Path).

% checkHandle(Term,Env,TgTp,MdTp,VlTp,ErTp,BdErTp,_,_,_,Hndlr,Path) :-
%   locOfAst(Term,Lc),
%   applyTypeFun(MdTp,[ErTp,VlTp],Lc,Env,RTp),
%   newTypeVar("_",Ct),
%   mkTypeExp(tpFun("tag",2),[RTp,Ct],TTp),
  


%     newTypeVar("_",Rt),
%   KType = contType(tplType([Tp]),Rt),

  
%   typeOfExp(Term,funType(tplType([BdErTp,KType]),RTp),ErTp,Env,_,Hndlr,Path).

recordAccessExp(Lc,Rc,Fld,Tp,ErTp,Env,Ev,dot(Lc,Rec,Fld,Tp),Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,ErTp,Env,Ev,Rec,Path).
recordAccessExp(Lc,Rc,Fld,Tp,ErTp,Env,Ev,dot(Lc,Rec,Fld,Tp),Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,ErTp,Env,Ev,Rec,Path),
  faceOfType(AT,Lc,Env,Fc),
  freshen(Fc,Env,_,Fce),
  deRef(Fce,Face),
  fieldInFace(Face,Fld,Lc,FTp),!,
  freshen(FTp,Env,_,FldTp),
  verifyType(Lc,FldTp,Tp,Env).

fieldInFace(faceType(Fields,_),Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfTerms([],[],_,Env,Env,_,[],_).
typeOfTerms([],[T|_],_,Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfTerms([A|_],[],_,Env,Env,_,[],_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfTerms([A|As],[ETp|ElTypes],ErTp,Env,Ev,_,[Term|Els],Path) :-
  deRef(ETp,ElTp),
  typeOfExp(A,ElTp,ErTp,Env,E0,Term,Path),
  % reportMsg("type of argument %s |= %s",[A,ETp]),
  % dispEnv(Env),
  locOfAst(A,Lc),
  typeOfTerms(As,ElTypes,ErTp,E0,Ev,Lc,Els,Path).

typeOfPtns([],[],_,Env,Env,_,[],_).
typeOfPtns([],[T|_],_,Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfPtns([A|_],[],_,Env,Env,_,[],_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfPtns([A|As],[ETp|ElTypes],ErTp,Env,Ev,_,[Term|Els],Path) :-
  deRef(ETp,ElTp),
  typeOfPtn(A,ElTp,ErTp,Env,E0,Term,Path),
  locOfAst(A,Lc),
  typeOfPtns(As,ElTypes,ErTp,E0,Ev,Lc,Els,Path).

% Analyse a square tuple term to try to disambiguate maps from others.

isMapSequence([E|_]) :-
  isBinary(E,_,"->",_,_).

isMapType(Tp,Env) :-
  isType("map",Env,tpDef(_,MpTp,_)),!,
  deRef(Tp,tpExp(TF1,_)),
  deRef(TF1,tpExp(MpTp,_)).

isListSequence([E|_]) :-
  \+isBinary(E,_,"->",_,_).

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
genDecl(conDef(Nm,CnNm,CnTp,CnSpec),_,Public,
	[contractDec(Nm,CnNm,CnTp,CnSpec)|Ex],Ex,Lx,Lx,Dfx,Dfx) :-
  call(Public,con(Nm)).
genDecl(conDef(Nm,CnNm,CnTp,CnSpec),_,_,Ex,Ex,
	[contractDec(Nm,CnNm,CnTp,CnSpec)|Lx],Lx,Dfx,Dfx).
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

mkBoot(Env,Lc,Pkg,Dfs,[BootDef|Dfs],Decls,[funDec("_boot",BootNm,BootTp)|Decls]) :-
  findType("cons",Lc,Env,ConsTp),
  isVar("_main",Env,Spec),
  localName(Pkg,value,"_boot",BootNm),
  applyTypeFun(ConsTp,[type("chars")],Lc,Env,LSTp),
  CmdVr = v(Lc,"CmdVr",LSTp),
  unitTp(UnitTp),
  MnTp = funType(tplType([LSTp]),UnitTp),
  typeOfVar(Lc,MnTp,Spec,Env,_Ev,MainTrm),
  BootTp = funType(tplType([LSTp]),UnitTp),
  BootEqn = rule(Lc,tple(Lc,[CmdVr]),none,
		 apply(Lc,MainTrm,
		       tple(Lc,[CmdVr]),UnitTp)),
  BootDef = funDef(Lc,"_boot",BootNm,hard,BootTp,[],[BootEqn]).
mkBoot(_Env,_,_,Dfs,Dfs,Decls,Decls).