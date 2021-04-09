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
  declarePkg(Pkg,Base,B0),
  isBraceTerm(Prog,Lc,_,El0),
  build_main(El0,Els),
  pushScope(B0,Env),
  Pkg = pkg(Pk,_),
  collectImports(Els,Imps,Stmts),
  importAll(Imps,Repo,AllImports),
  importAllDefs(AllImports,ImportSpecs,Repo,_IVars,[],Env,Env0),
  thetaEnv(Pk,Lc,Stmts,faceType([],[]),Env0,OEnv,Defs,Public),
  overload(Lc,Defs,OEnv,_Dict,ODefs),
  completePublic(Public,Public,FllPb,Pk),
  packageExport(ODefs,faceType([],[]),FllPb,XDefs,PDefs,
		Exports,Types,Contracts,Impls),!,
%  displayln(sq([ss("external defs"),canon:ssDefs(XDefs,0)])),
  filter(Exports,parsetype:nonConstructorTp,PkgFlds),
  PkgTp=faceType(PkgFlds,[]),
  genBraceType(Lc,PkgTp,ODfx,XDefs,OEnv,PEnv),
  tpName(PkgTp,Lbl),
  formTheta(Lc,Lbl,PDefs,PkgFlds,PkgTp,PkgVal),
  packageVarName(Pk,"",PkVr),
  mkBoot(PEnv,Lc,Pk,PkVr,Lbl,ODfx,PkgDefs,BDefs),
  BDefs=[varDef(Lc,PkVr,PkVr,[],PkgTp,PkgVal)|ODfx],
  Canon=prog(Pkg,Lc,ImportSpecs,PkgDefs,Exports,Types,Contracts,Impls),
  (is_member(showTCCode,Opts) -> displayln(canon:ssCanonProg(Canon));true).


mkPkgTheta(Lc,Pkg,Defs,Exports,Tp,PkgVal) :-
  formTheta(Lc,Pkg,Defs,Exports,Tp,PkgVal).

formTheta(Lc,Lbl,Defs,Flds,Tp,letRec(Lc,Defs,Exp)) :-
  sort(Flds,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,Flds,Args),
  project1(SortedFlds,ElTps),
  Exp = apply(Lc,cons(Lc,Lbl,consType(tupleType(ElTps),Tp)),
	      tple(Lc,Args),Tp).

formRecord(Lc,Lbl,Defs,Flds,Tp,letExp(Lc,Defs,Exp)) :-
  sort(Flds,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,Flds,Args),
  project1(SortedFlds,ElTps),
  Exp = apply(Lc,cons(Lc,Lbl,consType(tupleType(ElTps),Tp)),
	      tple(Lc,Args),Tp).

findExportedDefs(Lc,Flds,Els) :-
  map(Flds,checker:mkFieldArg(Lc),Els).

mkFieldArg(Lc,(Nm,Tp),v(Lc,Nm,Tp)).

thetaEnv(Pkg,Lc,Stmts,Fields,Base,TheEnv,Defs,Public) :-
  errorCount(ErrCount),
  collectDefinitions(Stmts,Dfs,Public,Annots),
  (noNewErrors(ErrCount) ->
   dependencies(Dfs,Groups,Annots),
   pushFace(Fields,Lc,Base,Env),
   checkGroups(Groups,Fields,Annots,Defs,[],Env,TheEnv,Pkg);
   Defs=[],TheEnv=Base).

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

findAllImports([],[]).
findAllImports([St|More],[Spec|Imports]) :-
  findImport(St,private,Spec),
  findAllImports(More,Imports).

importDefs(spec(Pkg,faceType(Exported,Types),Enums,Cons,Impls,_),Lc,PkgVr,E,Evx) :-
  importTypes(Types,Lc,E,E1),
  declareImportedFields(Exported,Pkg,Enums,Lc,PkgVr,E1,E2),
  importContracts(Cons,Lc,E2,E3),
  importImplementations(Lc,Impls,E3,Evx).

declareImportedFields([],_,_,_,_,Env,Env).
declareImportedFields([(Nm,Tp)|More],Pkg,Enums,Lc,PkgVr,Env,Ex) :-
  declareImportedVar(Lc,Nm,Pkg,PkgVr,Enums,Tp,Env,E0),
  declareImportedFields(More,Pkg,Enums,Lc,PkgVr,E0,Ex).

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

importAllDefs([],[],_,Vrs,Vrs,Env,Env).
importAllDefs([import(Lc,Viz,Pkg)|More],
	      [import(Viz,Pkg,PkgVr,Exported,Cnx,Cons,Impls)|Specs],Repo,
	      [PkgVr|IVrs],IVrx,
	      Env,Ex) :-
  importPkg(Pkg,Lc,Repo,Spec),
  packageVar(Pkg,Lc,Spec,PkgVr),
  Spec = spec(_,Exported,Cnx,Cons,Impls,_),
  importDefs(Spec,Lc,PkgVr,Env,Ev0),
  importAllDefs(More,Specs,Repo,IVrs,IVrx,Ev0,Ex).

packageVar(pkg(Pkg,_),Lc,spec(_,Face,_,_,_,_),v(Lc,PkgVar,Face)) :-
  packageVarName(Pkg,"",PkgVar).

importContracts([],_,Env,Env).
importContracts([C|L],Lc,E,Env) :-
  C = conDef(_,Nm,_,_,_),
  defineContract(Nm,Lc,C,E,E0),
  importContracts(L,Lc,E0,Env).

notAlreadyImported(import(_,_,Pkg),SoFar) :-
  \+ is_member(import(_,_,Pkg),SoFar),!.

importImplementations(Lc,Impls,Ev,Evx) :-
  rfold(Impls,checker:declImplementation(Lc),Ev,Evx).

declImplementation(Lc,imp(_ImplNm,FullNm,ImplTp),Ev,Evx) :-
  declareImplementation(Lc,FullNm,ImplTp,Ev,Evx).
declImplementation(_,acc(Tp,Fld,FnNm,AccTp),Ev,Evx) :-
  declareFieldAccess(Tp,Fld,FnNm,AccTp,Ev,Evx).

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
checkGroup([],Defs,Defs,Env,Env,_,_).

defineContract(N,Lc,Contract,E0,Ex) :-
  declareContract(N,Contract,E0,E1),
  declareMethods(Contract,Lc,E1,Ex).

declareMethods(conDef(_,_,_,_,ConEx),Lc,Env,Ev) :-
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

checkThetaBody(Tp,Lc,Els,Env,OEnv,Defns,Flds,Types,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  thetaEnv(Path,Lc,Els,Face,BaseEnv,OEnv,Defs,Public),
  faceOfType(ETp,Env,FaceTp2),
  getConstraints(FaceTp2,_,faceType(Fs,_)),
  completePublic(Public,Public,FullPublic,Path),
  computeExport(Defs,Fs,FullPublic,Defns,Flds,Types,[],[]),!.

checkRecordBody(Tp,Lc,Els,Env,OEnv,Defns,Flds,Types,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Env,FaceTp),
  getConstraints(FaceTp,Cx,faceType(Fs,Ts)),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  recordEnv(Path,Lc,Els,faceType(Fs,Ts),BaseEnv,OEnv,Defs,Public),
  completePublic(Public,Public,FullPublic,Path),
  computeExport(Defs,Fs,FullPublic,Defns,Flds,Types,[],[]),!.

checkLetRec(Tp,Lc,Els,Ex,Env,letRec(Lc,Defs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path).

checkLetExp(Tp,Lc,Els,Ex,Env,letExp(Lc,Defs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,_Public),
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

checkImplementation(Stmt,INm,[Impl,ImplDef|Dfs],Dfs,Env,Ex,_,_Path) :-
  isImplementationStmt(Stmt,Lc,Quants,Cons,Sq,IBody),
  parseContractConstraint(Quants,Cons,Sq,Env,ConNm,ConSpec),
  evidence(ConSpec,Env,IQ,CnSpec),
  getConstraints(CnSpec,AC,contractExists(Spec,_)),
  declareTypeVars(IQ,Lc,Env,E1),
  declareConstraints(Lc,AC,E1,ThEnv),
  implementationName(Spec,ImplName),
  contractType(Spec,CnType),
  labelImplExp(IBody,ConNm,ImpBody),
%  reportMsg("implementation %s, expected type %s",[ast(ImpBody),tpe(CnType)]),
  typeOfExp(ImpBody,CnType,ThEnv,_ThEv,ImplTerm,ImplName),
  putConstraints(AC,CnType,SS1),
  reQuantTps(SS1,IQ,ImpType),
  ImplDef = varDef(Lc,ImplName,ImplName,AC,ImpType,ImplTerm),
  Impl = implDef(INm,ConNm,ImplName,ImpType),
  declareImplementation(Lc,ImplName,ImpType,Env,Ex),!.
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
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTuple(Term,Lc,Els),
  \+isAbstraction(Term,_,_,_),
  checkThetaBody(Tp,Lc,Els,Env,_OEnv,Defs,Flds,Types,Path),
  tpName(Tp,Nm),
  formTheta(Lc,Nm,Defs,Flds,faceType(Flds,Types),Val).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTuple(Term,Lc,Els),
  checkRecordBody(Tp,Lc,Els,Env,_OEnv,Defs,Flds,Types,Path),
  tpName(faceType(Flds,Types),Nm),
  formRecord(Lc,Nm,Defs,Flds,Tp,Val).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  checkThetaBody(FnTp,Lc,Els,E0,_OEnv,Defs,Flds,_Types,Path),
  formTheta(Lc,Lbl,Defs,Flds,Tp,Val).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("R",FnTp),
  typeOfExp(F,consType(FnTp,Tp),Env,E0,Fun,Path),
  funLbl(Fun,Lbl),
  checkRecordBody(FnTp,Lc,Els,E0,_OEnv,Defs,Flds,_Types,Path),
  formRecord(Lc,Lbl,Defs,Flds,Tp,Val).
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
  (getContract(Nm,Env,conDef(_,_,_,_,Con)) ->
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

checkDo(Lc,B,Env,Ev,Tp,EE,Path) :-
  newTypeVar("_x",ValTp),
  newTypeVar("_er",ErTp),
  (getContract("execution",Env,conDef(_,_,_,_,Con)) ->
   freshen(Con,Env,_,contractExists(conTract(Contract,[ExTp],[]),_)),
   mkTypeExp(ExTp,[ErTp,ValTp],MTp),
   checkType(B,Tp,MTp,Env);
   reportError("execution contract not defined",[],Lc),
   newTypeVar("_t",ExTp)),
  checkAction(B,Env,Ev,Contract,ExTp,ValTp,ErTp,Body,Path),!,
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
checkAction(Term,Env,Ev,_Contract,ExTp,_VlTp,ErTp,bindDo(Lc,Ptn,Gen,ExTp),Path) :-
  isBind(Term,Lc,P,Ex),!,
  newTypeVar("_P",PT),
  mkTypeExp(ExTp,[ErTp,PT],HType),  % in a bind, the type of the value must be in the same monad
  typeOfExp(Ex,HType,Env,E1,Gen,Path),
  typeOfPtn(P,PT,E1,Ev,Ptn,Path).
checkAction(Term,Env,Ev,_,ExTp,VlTp,ErTp,varDo(Lc,Ptn,Exp,ExTp,VlTp,ErTp),Path) :-
  isMatch(Term,Lc,P,Ex),!,
  newTypeVar("_P",PT),
  typeOfPtn(P,PT,Env,Ev,Ptn,Path),
  typeOfExp(Ex,PT,Env,_,Exp,Path).
checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,Act,Path) :-
  isOptionMatch(Term,Lc,P,Exp),!,
  unary(Lc,"some",P,OP),
  binary(Lc,".=",OP,Exp,Term2),
  checkAction(Term2,Env,Ev,Contract,ExTp,ValTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,Act,Path) :-
  isIntegrity(Term,Lc,Cond),!,
  createIntegrityAction(Lc,Cond,IAc),
%  astDisp(IAc),
  checkAction(IAc,Env,Ev,Contract,ExTp,ValTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,Act,Path) :-
  isShow(Term,Lc,Exp),!,
  createShowAction(Lc,Exp,IAc),
%  astDisp(IAc),
  checkAction(IAc,Env,Ev,Contract,ExTp,ValTp,ErTp,Act,Path).
checkAction(Term,Env,Ev,_,ExTp,ValTp,ErTp,Act,Path) :-
  isSplice(Term,Lc,S,F,T,R),!,
  unary(Lc,"!",S,Src),
  nary(Lc,"_splice",[S,F,T,R],Rep),
  checkAssignment(Lc,Src,Rep,Env,Ev,ExTp,ValTp,ErTp,Act,Path).
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
	    ifThenDo(Lc,Ts,Th,returnDo(Lc,tple(Lc,[]),ExTp,tupleType([]),ErTp),ExTp,ValTp,ErTp),Path) :-
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
checkAction(Term,Env,Env,Contract,ExTp,_,ErTp,untilDo(Lc,Ts,Bdy,ExTp,ErTp),Path) :-
  isUntilDo(Term,Lc,B,T),!,
  findType("boolean",Lc,Env,LogicalTp),
  pushScope(Env,WEnv),
  typeOfExp(T,LogicalTp,WEnv,Et,Ts,Path),
  checkAction(B,Et,_,Contract,ExTp,tupleType([]),ErTp,Bdy,Path).
checkAction(Term,Env,Env,Contract,ExTp,_,ErTp,forDo(Lc,Ts,Bdy,ExTp,ErTp),Path) :-
  isForDo(Term,Lc,T,B),!,
  findType("boolean",Lc,Env,LogicalTp),
  pushScope(Env,FEnv),
  typeOfExp(T,LogicalTp,FEnv,Et,Ts,Path),
  UnitTp = tupleType([]),
  checkAction(B,Et,_,Contract,ExTp,UnitTp,ErTp,Bdy,Path).
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
checkAction(Term,Env,Env,Contract,ExTp,ValTp,ErTp,caseDo(Lc,Gov,Cases,ExTp,ErTp),Path) :-
  isCaseExp(Term,Lc,Gv,Cs),!,
  newTypeVar("_G",GTp),
  typeOfExp(Gv,GTp,Env,_,Gov,Path),
  checkActionCases(Cs,GTp,Env,Contract,ExTp,ValTp,ErTp,Cases,Dflt,Dflt,[],Path).
%  reportMsg("gen case %s",[caseDo(Lc,Gov,Cases,ExTp,ErTp)],Lc).

checkAction(Term,Env,Ev,Contract,ExTp,ValTp,ErTp,Exp,Path) :-
  isBraceTuple(Term,_,[Stmt]),!,
  checkAction(Stmt,Env,Ev,Contract,ExTp,ValTp,ErTp,Exp,Path).
checkAction(Term,Env,Env,_Contract,_ExTp,_ValTp,_ErTp,noDo(Lc),_Path) :-
  isBraceTuple(Term,Lc,[]),!.
checkAction(Term,Env,Ev,_,ExTp,ValTp,ErTp,simpleDo(Lc,Exp,ExTp,ErTp),Path) :-
  isRoundTerm(Term,Lc,_,_),!,
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  typeOfExp(Term,MTp,Env,Ev,Exp,Path).
checkAction(Term,Env,Env,_,_,_,_,noDo(Lc),_) :-
  locOfAst(Term,Lc),
  reportError("invalid action: %s",[Term],Lc).

checkAssignment(Lc,L,R,Env,Ev,ExTp,ValTp,ErTp,simpleDo(Lc,Exp,ExTp,ErTp),
		Path) :-
  (isIndexTerm(L,LLc,C,I) ->
     unary(LLc,"!",C,CC),
    ternary(LLc,"_put",CC,I,R,Repl),
    binary(Lc,":=",C,Repl,Term);
   binary(Lc,":=",L,R,Term)),
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  typeOfExp(Term,MTp,Env,Ev,Exp,Path).

checkActionCases([],_,_,_,_,_,_,Cases,Cases,Dfx,Dfx,_).
checkActionCases([C|Ss],GTp,Env,Contract,ExTp,ValTp,ErTp,Cases,Cx,Df,Dfx,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkActionCase(Lc,L,G,R,GTp,Env,Contract,ExTp,ValTp,ErTp,Cases,C0,Df,Df0,Path),
  checkActionCases(Ss,GTp,Env,Contract,ExTp,ValTp,ErTp,C0,Cx,Df0,Dfx,Path).

checkActionCase(Lc,Lhs,G,R,GTp,Env,Contract,ExTp,ValTp,ErTp,Cases,Cases,Df,Dfx,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkActionCase(Lc,DLhs,G,R,GTp,Env,Contract,ExTp,ValTp,ErTp,Df,Dfx,_,_,Path).
checkActionCase(Lc,H,G,R,GTp,Env,Contract,ExTp,ValTp,ErTp,
		[equation(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Path) :-
  typeOfPtn(H,GTp,Env,E1,Arg,Path),
  checkGuard(G,E1,E2,Guard,Path),
  checkAction(R,E2,_,Contract,ExTp,ValTp,ErTp,Exp,Path).

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

checkCatch(Term,Env,Contract,ExTp,ValTp,ErTp,BdErTp,Anon,Hndlr,Path) :-
  isBraceTuple(Term,Lc,[St]),!,
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
  Htype = funType(tupleType([BdErTp]),MTp),
  checkAction(St,Env,_,Contract,ExTp,ValTp,ErTp,H,Path),
  genAction(H,Contract,noDo(Lc),HH,Path),
  Hndlr = lambda(Lc,LamLbl,equation(Lc,tple(Lc,[Anon]),none,HH),Htype),
  lambdaLbl(Path,"catch",LamLbl).
checkCatch(Term,Env,_Contract,ExTp,ValTp,ErTp,BdErTp,_,Hndlr,Path) :-
  mkTypeExp(ExTp,[ErTp,ValTp],MTp),
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
  % dispEnv(Env,1),
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

packageExport(Defs,Fields,Public,Defns,LDefs,Exports,Types,Contracts,Impls) :-
  compExport(Defs,Fields,Public,LDefs,[],Defns,[],
	     Exports,[],Types,[],Contracts,[],Impls,[]).

computeExport(Defs,Fields,Public,Defns,Exports,Types,Contracts,Impls) :-
  compExport(Defs,Fields,Public,Defns,LDefns,LDefns,[],
	     Exports,[],Types,[],Contracts,[],Impls,[]).

compExport([],_,_,Lcls,Lcls,Defx,Defx,Exx,Exx,Tpx,Tpx,Cxx,Cxx,Ixx,Ixx).
compExport([Def|Defs],Fields,Public,Lcls,Lclx,
	   Defns,Dxx,Exports,Exx,Types,Txx,Contracts,Cxx,Impls,Ixx) :-
  exportDef(Def,Def,Fields,Public,Lcls,Lcl0,Defns,Df0,
	    Exports,Ex0,Types,Tx0,Contracts,Cx0,Impls,Ix0),!,
  compExport(Defs,Fields,Public,Lcl0,Lclx,Df0,Dxx,Ex0,Exx,Tx0,Txx,Cx0,Cxx,Ix0,Ixx).

exportDef(funDef(_,Nm,_,Tp,_,_),Def,Fields,Public,[Def|Lcls],Lcls,Dx,Dx,
	  [(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(typeDef(_,Nm,_,_,TpRl),Def,_,Public,Lcls,Lcx,Dfs,Dfx,Ex,Ex,Tps,Tx,Cx,Cx,Ix,Ix) :-
  exportType(Nm,TpRl,Public,Def,Tps,Tx,Lcls,Lcx,Dfs,Dfx).
exportDef(varDef(_,Nm,_,_,Tp,_),Def,Fields,Public,[Def|Lcx],Lcx,Dfx,Dfx,
	  [(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public).
exportDef(cnsDef(_,Nm,Con),Def,Fields,Public,Lcx,Lcx,[Def|Dfx],Dfx,
	  [(Nm,Tp)|Ex],Ex,Tx,Tx,Cx,Cx,Impl,Impl) :-
  isPublicVar(Nm,Fields,Public),
  typeOfCanon(Con,Tp).
exportDef(conDef(_,Nm,_,_CnTp,_),Con,_,Public,Lcx,Lcx,Dfx,Dfx,Ex,Ex,Tx,Tx,
	  [Con|Cx],Cx,Impl,Impl) :-
  isPublicContract(Nm,Public).
exportDef(implDef(TmpNm,ConNm,ImplName,Spec),_,_,Public,Lcx,Lcx,Dfx,Dfx,
	  Ex,Ex,Tps,Tps,Cx,Cx,
	  [imp(ConNm,ImplName,Spec)|Ix],Ix) :-
  isPublicImpl(TmpNm,Public),!.
exportDef(accDef(Tp,Fld,AccFn,AccTp),Def,_,Public,Lcx,Lcx,[Def|Dfx],Dfx,
	  [(AccFn,AccTp)|Ex],Ex,Tx,Tx,Cx,Cx,
	  [acc(Tp,Fld,AccFn,AccTp)|Ix],Ix) :-
  exportAcc(Tp,Public).
exportDef(_,Df,_,_,[Df|Lcx],Lcx,Dfx,Dfx,Ex,Ex,Tps,Tps,Cx,Cx,Impls,Impls).

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

exportType(Nm,TpRule,Public,Def,[(Nm,TpRule)|Tx],Tx,Lcx,Lcx,[Def|Dfs],Dfs) :-
  isPublicType(Nm,Public),!.
exportType(_,_,_,Def,Tx,Tx,[Def|Lcx],Lcx,Dfs,Dfs).

isPublicType(Nm,Public) :-
  is_member(tpe(Nm),Public),!.
  
isPublicContract(Nm,Public) :-
  is_member(con(Nm),Public),!.

isPublicImpl(Nm,Public) :-
  is_member(imp(Nm),Public),!.

exportAcc(Tp,Public) :-
  tpName(Tp,TpNm),
  (marker(type,TpMrkr),
   splitLocalName(TpNm,TpMrkr,_,Nm),
   isPublicType(Nm,Public);
   marker(conTract,CnMrkr),
   splitLocalName(TpNm,CnMrkr,_,Nm),
   isPublicContract(Nm,Public)),!.

declarePkg(P,E,Ev) :-
  declareVar("__pkg__",vrEntry(std,checker:mkPkg(P)),E,Ev).

mkPkg(pkg(P,_),_,Tp,stringLit(P,Tp)).


mkVers(Lc,defltVersion,v(Lc,"defltVersion",Tp)) :-
  stdType("version",Tp,_).
mkVers(Lc,ver(V),apply(Lc,v(Lc,"ver",consType(tupleType([StrTp]),VerTp)),
		       tple(Lc,[stringLit(V,StrTp)]),VerTp)) :-
  stdType("version",VerTp,_),
  stdType("string",StrTp,_).

mkBoot(Env,Lc,Pkg,PkgVr,PkgTpLbl,PkgDefs,[BootDef|Dfs],Dfs) :-
  isVar("_main",Env,vrEntry(_,_,_)),
  localName(Pkg,value,"_boot",BootNm),
  is_member(accDef(PkgTp,"_main",AccFn,AccTp),PkgDefs),tpName(PkgTp,PkgTpLbl),!,
  mkTypeExp(tpFun("star.core*consType",1),[type("star.core*string")],LSTp),
  CmdVr = v(Lc,"CmdVr",LSTp),
  UnitTp = tupleType([]),
  MnTp = funType(tupleType([LSTp]),UnitTp),
  BootEqn = equation(Lc,tple(Lc,[CmdVr]),none,
		     apply(Lc,
			   apply(Lc,v(Lc,AccFn,AccTp),tple(Lc,[v(Lc,PkgVr,PkgTp)]),
				 funType(tupleType([PkgTp],MnTp))),
			   tple(Lc,[CmdVr]),UnitTp)),
  BootDef = funDef(Lc,"_boot",BootNm,funType(tupleType([LSTp]),UnitTp),[],[BootEqn]).
mkBoot(_Env,_,_,_,_,_,Dfs,Dfs).