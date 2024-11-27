:- module(checker,[checkProgram/6]).

:- use_module(abstract).
:- use_module(astdisp).
:- use_module(display).
:- use_module(wff).
:- use_module(dependencies).
:- use_module(freshen).
:- use_module(unify).
:- use_module(types).
:- use_module(parsetype).
:- use_module(dict).
:- use_module(declmgt).
:- use_module(misc).
:- use_module(canon).
:- use_module(errors).
:- use_module(operators).
:- use_module(import).
:- use_module(resolve).

checkProgram(Prg,Pkg,Repo,Opts,PkgDecls,Canon) :-
  stdDict(Base),
  isBraceTerm(Prg,Lc,_,Els),
  Pkg = pkg(Pk,_),
  collectImports(Els,Imports,Stmts),
  importAll(Imports,Repo,AllImports),
  collectImportDecls(AllImports,Repo,[],IDecls),
  declareAllDecls(IDecls,Lc,Base,Env0),
%  dispEnv(Env0),
  thetaEnv(checker:pkgExport,Pk,Lc,Stmts,faceType([],[]),Opts,Env0,_OEnv,Defs,decls(ThEx,ThL)),
  declareAllDecls(ThEx,Lc,Env0,Env1),
  declareAllDecls(ThL,Lc,Env1,Env2),
  overload(Defs,Env2,ODefs),
  Canon=prog(Pkg,Imports,ThEx,ThL,ODefs),
%  reportMsg("export declarations",[]),
%  dispDecls(ThEx),
%  reportMsg("private declarations",[]),
%  dispDecls(ThL),
  concat(ThEx,ThL,D0),
  concat(D0,IDecls,PkgDecls).

collectDefinitions([St|Stmts],Defs,V,Vx,A) :-
  collectDefinition(St,Stmts,S0,Defs,D0,V,V0,A,A0,checker:defltVz),
  collectDefinitions(S0,D0,V0,Vx,A0).
collectDefinitions([],[],Vz,Vz,[]).

collectDefinition(St,Stmts,Stmts,[(cns(V),Lc,[T])|Defs],Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  isConstructorType(T,_,_,_,_,_),!,
  (isIden(L,V) -> call(Export,cns(V),P,Px);
   isPrivate(L,_,V1),isIden(V1,V),prvteViz(P,cns(V),Px)).
collectDefinition(St,Stmts,Stmts,Defs,Defs,P,Px,[(V,T)|A],A,Export) :-
  isTypeAnnotation(St,Lc,L,T),
  (isIden(L,V) ->
   call(Export,var(V),P,Px) ;
   reportError("cannot understand type annotation %s",[ast(St)],Lc),
   P=Px).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,_) :-
  isPrivate(St,_,Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,checker:prvteViz).
collectDefinition(St,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,_) :-
  isPublic(St,_,Inner),
  collectDefinition(Inner,Stmts,Stx,Defs,Dfx,P,Px,A,Ax,checker:pubViz).
collectDefinition(St,Stmts,Stmts,[(con(Nm),Lc,[St])|Defs],Defs,P,Px,A,Ax,Export) :-
  isContractStmt(St,Lc,Quants,Constraints,Con,Els),
  generateAnnotations(Els,Quants,[Con|Constraints],A,Ax),
  typeName(Con,Nm),
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
  isTypeExistsStmt(St,Lc,_,_,L,B),
  typeName(L,Nm),
  call(Export,tpe(Nm),P,P0),
  isBraceTuple(B,_,Els),
  rfold(Els,checker:tpeFldViz(Nm,Export),P0,Px).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Defs,P,Px,A,A,Export) :-
  isTypeFunStmt(St,Lc,_,_,L,_),
  typeName(L,Nm),
  call(Export,tpe(Nm),P,Px).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St])|Defs],Dfx,P,Px,A,Ax,Export) :-
  isAlgebraicTypeStmt(St,Lc,Quants,Constraints,Head,Body),
  typeName(Head,Nm),
  call(Export,tpe(Nm),P,P0),
  algebraicFace(Body,[],_,Face),
  isBraceTuple(Face,_,Els),
  rfold(Els,checker:tpeFldViz(Nm,Export),P0,P1),
  collectConstructors(Body,Quants,Constraints,Head,Defs,Dfx,P1,Px,A,Ax,Export).
collectDefinition(St,Stmts,Stmts,[(tpe(Nm),Lc,[St]),(cns(CnNm),Lc,[CnSt])|Defs],Defs,P,Px,Ax,Ax,Export) :-
  isStructTypeStmt(St,Lc,Q,XQ,Cx,Tp,CnNm,Els),
  typeName(Tp,Nm),
  call(Export,tpe(Nm),P,P0),
  rfold(Els,checker:tpeFldViz(Nm,Export),P0,P1),
  call(Export,cns(CnNm),P1,Px),
  braceTuple(Lc,Els,Hd),
  reConstrain(Cx,Hd,XHd),
  reXQuant(XQ,XHd,QHd),
  binary(Lc,"<=>",QHd,Tp,Rl),
  reConstrain(Cx,Rl,CRl),
  reUQuant(Q,CRl,CnSt).
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

tpeFldViz(TpNm,Export,Entry,Vz,Vx) :-
  isTypeAnnotation(Entry,_,N,_), isIden(N,Fld),
  call(Export,fld(TpNm,Fld),Vz,Vx).
tpeFldViz(_TpNm,_Export,_Entry,Vz,Vz).

pubViz(V,viz(Pu,D,Pr),viz(Px,Dx,Prx)) :- add_mem(V,Pu,Px),del_mem(V,D,Dx),del_mem(V,Pr,Prx).
prvteViz(V,viz(Pu,D,Pr),viz(Pu,Dx,Prx)) :- del_mem(V,D,Dx), add_mem(V,Pr,Prx).
defltVz(V,viz(Pu,D,Pr),viz(Pu,Dx,Pr)) :-
  \+is_member(V,Pu),
  \+is_member(V,Pr),!,
  add_mem(V,D,Dx).
defltVz(_,Viz,Viz).

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

collectConstructors(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,Export) :-
  isBinary(C,_,"|",L,R),!,
  collectConstructors(L,Quants,Constraints,Tp,Defs,Df0,P,P0,A,A0,Export),
  collectConstructors(R,Quants,Constraints,Tp,Df0,Dfx,P0,Px,A0,Ax,Export).
collectConstructors(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,Export) :-
  isUnary(C,_,"|",R),!,
  collectConstructors(R,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,Export).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Enm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isIden(C,Lc,Enm),!,
  roundTuple(Lc,[],Hd),
  binary(Lc,"<=>",Hd,Tp,CnTp),
  reConstrain(Constraints,CnTp,Rl),
  reUQuant(Quants,Rl,St),
  call(Export,cns(Enm),P,Px).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Enm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isEnum(C,Lc,E),
  isIden(E,_,Enm),!,
  roundTuple(Lc,[],Hd),
  binary(Lc,"<=>",Hd,Tp,CnTp),
  reConstrain(Constraints,CnTp,Rl),
  reUQuant(Quants,Rl,St),
  call(Export,cns(Enm),P,Px).
collectConstructors(C,Quants,Constraints,Tp,[(cns(Nm),Lc,[St])|Defs],Defs,
		    P,Px,Ax,Ax,Export) :-
  isRoundCon(C,_,_,Lc,Nm,Args),!,
  roundTuple(Lc,Args,Hd),
  binary(Lc,"<=>",Hd,Tp,Rl),
  reConstrain(Constraints,Rl,CRl),
  reUQuant(Quants,CRl,St),
  call(Export,cns(Nm),P,Px).
collectConstructors(C,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,_Export) :-
  isPrivate(C,_,I),
  collectConstructors(I,Quants,Constraints,Tp,Defs,Dfx,P,Px,A,Ax,checker:prvteViz).

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

notAlreadyImported(import(_,_,Pkg),SoFar) :-
  \+ is_member(import(_,_,Pkg),SoFar),!.

thetaEnv(Publish,Pkg,Lc,Stmts,Face,Opts,Base,TheEnv,Defs,Decls) :-
  collectDefinitions(Stmts,Dfs,viz([],[],[]),Viz,Annots),!,
  dependencies(Dfs,Opts,Groups,Annots),
  pushFace(Face,Lc,Base,Env),
  checkGroups(Groups,Face,Annots,Defs,[],Env,TheEnv,Publish,Viz,decls([],[]),Decls,Pkg).

recordEnv(Publish,Path,_Lc,Stmts,Fields,Base,TheEnv,Defs,Decls) :-
  collectDefinitions(Stmts,Dfs,viz([],[],[]),Viz,Annots),!,
  parseAnnotations(Dfs,Fields,Annots,Base,Path,Face),
  checkGroup(Dfs,Defs,[],Base,TheEnv,Face,Publish,Viz,decls([],[]),Decls,Path).

checkGroups([],_,_,Defs,Defs,E,E,_,_,Dc,Dc,_).
checkGroups([Gp|More],Fields,Annots,Defs,Dfx,E,Ev,Publish,Viz,Dc,Dcx,Path) :-
  parseAnnotations(Gp,Fields,Annots,E,Path,Face),!,
  groupLc(Gp,Lc),
  pushFace(Face,Lc,E,E0),
  checkGroup(Gp,Defs,Df2,E0,E3,Face,Publish,Viz,Dc,Dc0,Path),!,
  checkGroups(More,Fields,Annots,Df2,Dfx,E3,Ev,Publish,Viz,Dc0,Dcx,Path).

groupLc([(_,Lc,_)|_],Lc).

checkGroup([(con(N),Lc,[ConStmt])|More],[Contract|Defs],Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Path) :-
  parseContract(ConStmt,Env,E0,Path,[Contract|Defs],Df0,Publish,Viz,Dc,Dc0),
  defineContract(N,Lc,Contract,E0,E1),
  checkGroup(More,Df0,Dx,E1,Ex,Face,Publish,Viz,Dc0,Dcx,Path).
checkGroup([(cns(Nm),Lc,[St])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Path) :-
  parseConstructor(Nm,Lc,St,Env,E0,Defs,D0,Publish,Viz,Dc,Dc0,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Path).
checkGroup([(var(N),Lc,Stmts)|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Path) :-
  checkVarRules(N,Lc,Stmts,Env,E0,Defs,D0,Face,Publish,Viz,Dc,Dc0,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Path).
checkGroup([(imp(Nm),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Path) :-
  checkImplementation(Stmt,Nm,Defs,D0,Env,E0,Face,Publish,Viz,Dc,Dc0,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Path).
checkGroup([(tpe(_),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Path) :-
  parseTypeDef(Stmt,Defs,D0,Env,E0,Publish,Viz,Dc,Dc0,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Path).
checkGroup([(open(_),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Path) :-
  checkOpenStmt(Stmt,Env,Ev0,Publish,Viz,Dc,Dc0,Path),
  checkGroup(More,Defs,Dx,Ev0,Ex,Face,Publish,Viz,Dc0,Dcx,Path).
checkGroup([],Defs,Defs,Env,Env,_,_,_,Dcx,Dcx,_).

parseConstructor(Nm,Lc,T,Env,Ev,[Defn|Defs],Defs,Publish,Viz,Dc,Dcx,Path) :-
%  reportMsg("parse constructor type %s:%s",[id(Nm),ast(T)],Lc),
  parseType(T,Env,Tp),
%  reportMsg("constructor %s:%s",[id(Nm),tpe(Tp)],Lc),
  mangleName(Path,class,Nm,FullNm),
  unwrapType(Tp,_Q,_Cx,ITp),
  Defn = cnsDef(Lc,Nm,enm(Lc,FullNm,Tp)),
  Decl = cnsDec(Nm,FullNm,Tp),
  (deRef(ITp,consType(tplType([]),_)) ->
   declareEnum(Lc,Nm,FullNm,Tp,Env,Ev);
   declareCns(Lc,Nm,FullNm,Tp,Env,Ev)),
  call(Publish,Viz,cns(Nm),Decl,Dc,Dcx).

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
  typeOfExp(Vr,VrTp,none,Env,E0,Rc,Path),
  faceOfType(VrTp,Lc,E0,faceType(Flds,Tps)),
  declareExported(Flds,Rc,Lc,Env,E1),
  declareExportedTypes(Tps,Rc,Lc,E1,Ev).

declareExported([],_,_,Env,Env).
declareExported([(Nm,Tp)|More],Rc,Lc,Env,Ev) :-
  declareField(Lc,Rc,Nm,Tp,Env,Ev0),
  declareExported(More,Rc,Lc,Ev0,Ev).

declareExportedTypes(_,_,_,Env,Env).

checkVarRules(N,Lc,Stmts,E,Ev,Defs,Dx,Face,Publish,Viz,Dc,Dcx,Path) :-
  pickupDefnType(N,Lc,Face,Stmts,E,E0,Tp),
  evidence(Tp,E,Q,ETp),
  getConstraints(ETp,Cx,ProgramType),
  declareTypeVars(Q,Lc,E0,E1),
  declareConstraints(Lc,Cx,E1,E2),
  processStmts(Stmts,ProgramType,Rules,Deflts,Deflts,[],E2,Path),
  qualifiedName(Path,N,LclName),
  formDefn(Rules,N,LclName,E,Ev,Tp,Cx,Defs,Dx,Publish,Viz,Dc,Dcx).
%  reportMsg("type of %s:%s",[N,ProgramType]).

formDefn([Eqn|Eqns],Nm,LclNm,Env,Ev,Tp,Cx,[Defn|Dx],Dx,Publish,Viz,Dc,Dcx) :-
  Eqn = rule(Lc,_,_,_),
  Defn = funDef(Lc,Nm,LclNm,hard,Tp,Cx,[Eqn|Eqns]),
  Decl = funDec(Nm,LclNm,Tp),
  declareVr(Lc,Nm,Tp,none,Env,Ev),
  call(Publish,Viz,var(Nm),Decl,Dc,Dcx).
formDefn([varDef(Lc,_,_,_,_,open(VLc,Value,Tp))],Nm,LclNm,Env,Ev,Tp,Cx,[Defn|Dx],Dx,
	 Publish,Viz,Dc,Dcx) :-
  freshen(Tp,Env,_,FTp),
  faceOfType(FTp,Lc,Env,FaceTp),
  Defn = varDef(Lc,Nm,LclNm,Cx,Tp,open(VLc,Value,Tp)),
  Decl = varDec(Nm,LclNm,Tp),
  declareVr(Lc,Nm,Tp,some(FaceTp),Env,Ev),
  call(Publish,Viz,var(Nm),Decl,Dc,Dcx).
formDefn([varDef(Lc,_,_,_,_,Value)],Nm,LclNm,Env,Ev,Tp,Cx,[Defn|Dx],Dx,
	 Publish,Viz,Dc,Dcx) :-
  Defn = varDef(Lc,Nm,LclNm,Cx,Tp,Value),
  Decl = varDec(Nm,LclNm,Tp),
  declareVr(Lc,Nm,Tp,none,Env,Ev),
  call(Publish,Viz,var(Nm),Decl,Dc,Dcx).

processStmts([],_,Defs,Defs,Dflts,Dflts,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Df,Dfx,E0,Path) :-
  processStmt(St,ProgramType,Defs,D0,Df,Df0,E0,Path),!,
  processStmts(More,ProgramType,D0,Dx,Df0,Dfx,E0,Path).

processStmt(St,ProgramType,Defs,Defx,Df,Dfx,E,Path) :-
  isEquation(St,Lc,L,Cond,R),!,
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
%  reportMsg("rule %s",[rle(Eqn)],Lc),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[Eqn|Dfx]; Defs=[Eqn|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("rule not consistent with expected type: %s",[ProgramType],Lc).

checkDefn(Lc,L,R,VlTp,varDef(Lc,Nm,ExtNm,[],VlTp,Value),Env,Path) :-
  isIden(L,_,Nm),
  pushScope(Env,E),
  typeOfExp(R,VlTp,E,_E2,Value,Path),
  qualifiedName(Path,Nm,ExtNm).

checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  genNewName(Path,"Γ",ThPath),
  faceOfType(ETp,Lc,Env,TpFace),
  getConstraints(TpFace,_,faceType(Fs,_)),
  thetaEnv(checker:letExport,ThPath,Lc,Els,Face,[],BaseEnv,_,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  formTheta(Lc,Lbl,Decls,Defs,Fs,Tp,Val).

faceDecls(Fs,Ts,decls(Pub,[],[],[])) :-
  rfold(Fs,checker:mkVarMd,[],P),
  rfold(Ts,checker:mkTpMd,P,Pub).

publicDecls(Pub,decls(P,Pr,X,L),decls(Pb,Pr,X,L)) :-
  merge(Pub,P,Pb).

mkVarMd((Nm,Tp),P,[cns(Nm)|P]) :-
  isCnsType(Tp,_,_),!.
mkVarMd((Nm,_),P,[var(Nm)|P]).

mkTpMd((Nm,_),P,[tpe(Nm)|P]).

formTheta(Lc,Lbl,Decls,Defs,Flds,Tp,letRec(Lc,Decls,Defs,Exp)) :-
  sort(Flds,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,SortedFlds,Args),
  project1(SortedFlds,ElTps),
  Exp = capply(Lc,enm(Lc,Lbl,consType(tplType(ElTps),Tp)),tple(Lc,Args),Tp).

findExportedDefs(Lc,Flds,Els) :-
  map(Flds,checker:mkFieldArg(Lc),Els).

mkFieldArg(Lc,(Nm,Tp),v(Lc,Nm,Tp)).

checkRecordBody(Tp,Lbl,Lc,Els,Env,letExp(Lc,LDecls,Defs,Exp),Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,faceType(Fs,Ts)),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  genNewName(Path,"Γ",ThPath),
  recordEnv(checker:letExport,ThPath,Lc,Els,faceType(Fs,Ts),BaseEnv,_,Defs,RDecls),
%  completePublic(Public,Public,FullPublic,Path),
%  computeThetaExport(Defs,Fs,FullPublic,_Decls,Defs),!,
%  filterTheta(Decls,Fs,Public,XDecls,LDecls),
  sort(Fs,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,SortedFlds,Args),
  Exp = capply(Lc,Lbl,tple(Lc,Args),Tp),
  mergeDecls(RDecls,LDecls).

checkLetRec(Tp,Lc,Els,Ex,Env,letRec(Lc,Decls,Defs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(checker:letExport,ThPath,Lc,Els,faceType([],[]),[],ThEnv,OEnv,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path).

checkLetExp(Tp,Lc,Els,Ex,Env,letExp(Lc,LDecls,Defs,Bound),Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(checker:letExport,ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,Decls),
%  computeLetExport(Defs,[],_Decls,XDefs),
  typeOfExp(Ex,Tp,OEnv,_,Bound,Path),
  mergeDecls(Decls,LDecls).

mergeDecls(decls(Pu,Pr),Dc) :-
  concat(Pu,Pr,Dc).

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

checkImplementation(Stmt,INm,[ImplVar|Dfs],Dfs,Env,Evx,_,
		    Publish,Viz,Dc,Dcx,Path) :-
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
%  Impl = implDef(INm,ImplName,ImplVrNm,ImpType),
  Decl = impDec(ImplName,ImplVrNm,ImpType),
  VDcl = varDec(ImplVrNm,ImplVrNm,ImpType),
  declareVr(Lc,ImplVrNm,ImpType,none,Env,Ev0),
  declareImplementation(ImplName,ImplVrNm,ImpType,Ev0,Evx),
  call(Publish,Viz,imp(INm),Decl,Dc,Dc0),
  call(Publish,Viz,imp(INm),VDcl,Dc0,Dcx),!.
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_,_,Dcx,Dcx,_) :-
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

% Patterns are very similarly checked to expressions, except that fewer forms
typeOfArgPtn(T,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(T,Lc,A),!,
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
  isEnum(Trm,_,N),isIden(N,_,_),!,
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
  isConApply(Term,Lc,F,A),!,
  newTypeVar("A",At),
  typeOfExp(F,consType(At,Tp),Env,E0,Fun,Path),
  typeOfArgPtn(tuple(Lc,"()",A),At,E0,Ev,Args,Path),
  Exp = capply(Lc,Fun,Args,Tp).
typeOfPtn(Term,Tp,Env,Ev,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  mkConApply(Lc,F,A,TT),
  reportWarning("this form of pattern: %s is deprecated, use %s",
		[ast(Term),ast(TT)],Lc),
  newTypeVar("A",At),
  typeOfExp(F,consType(At,Tp),Env,E0,Fun,Path),
%  reportMsg("con type = %s",[tpe(consType(At,Tp))],Lc),
  typeOfArgPtn(tuple(Lc,"()",A),At,E0,Ev,Args,Path),
  Exp = capply(Lc,Fun,Args,Tp).
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
  Exp = capply(Lc,Fun,tple(Lc,ArgPtns),Tp).
%  reportMsg("record ptn = %s",[can(Exp)],Lc).

typeOfElementPtns([],_Face,Env,Env,Defs,Defs,_Path).
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
  typeOfExps(A,ArgTps,Env,Ev,Lc,Els,Path).
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
  isEnum(T,_,N),isIden(N,_,_),!,
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
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isSuppress(Term,Lc,V),
  (isIden(V,VLc,N) ->
   Env=Ev,
   (getNRVar(Lc,N,Env,Exp,VTp) ->
    verifyType(Lc,ast(V),VTp,Tp,Env);
    reportError("variable '%s' not defined, expecting a %s",[V,Tp],VLc),
    Term=void);
   reportError("expecting an identifier, not '%s'",[V],Lc),
   typeOfExp(V,Tp,Env,Ev,Exp,Path)).
typeOfExp(P,Tp,Env,Ex,where(Lc,Ptn,Cond),Path) :-
  isWhere(P,Lc,L,C),!,
  typeOfExp(L,Tp,Env,E0,Ptn,Path),
  checkGuard(some(C),E0,Ex,some(Cond),Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  typeOfFieldAcc(Lc,Rc,Fld,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,U,Path) :-
  isRecordUpdate(Term,Lc,Rc,Fld,Vl),!,
  typeOfRecordUpdate(Lc,Rc,Fld,Vl,Tp,Env,Ev,U,Path).
typeOfExp(Term,Tp,Env,Ev,Exp,Path) :-
  isTupleAcc(Term,Lc,Rc,Fld),!,
  typeOfTupleAcc(Lc,Rc,Fld,Tp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,cond(Lc,Test,Then,Else,Tp),Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  checkGoal(Tst,Env,E0,Test,Path),
  typeOfExp(Th,Tp,E0,E1,Then,Path),
  typeOfExp(El,Tp,Env,E2,Else,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,Env,Ev,conj(Lc,Lhs,Rhs),Path) :-
  isConjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,Env,E1,Lhs,Path),
  typeOfExp(R,LogicalTp,E1,Ev,Rhs,Path).
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
  typeOfExp(R,LogicalTp,E1,_Ev,Rhs,Path).
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
typeOfExp(Term,Tp,Env,Ev,case(Lc,Bound,Eqns,Tp),Path) :-
  isCaseExp(Term,Lc,Bnd,Cases),
  checkCaseExp(Lc,Bnd,Cases,Tp,Env,Ev,checker:typeOfExp,Bound,Eqns,Path).
typeOfExp(Term,Tp,Env,Ev,cell(Lc,Exp),Path) :-
  isRef(Term,Lc,I),
  newTypeVar("r",RT),
  mkRefTp(RT,RTp),
  verifyType(Lc,ast(Term),RTp,Tp,Env),
  typeOfExp(I,RT,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Ev,deref(Lc,Exp),Path) :-
  isCellRef(Term,Lc,I),
  mkRefTp(Tp,RTp),
  typeOfExp(I,RTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Thnk,Path) :-
  isThunk(Term,Lc,Th),!,
  typeOfThunk(Lc,Th,Tp,Env,Thnk,Path).
typeOfExp(Term,Tp,Env,Ev,thnkRef(Lc,Exp,Tp),Path) :-
  isThunkRef(Term,Lc,Rf),!,
  thunkType(Tp,ThTp),
  typeOfExp(Rf,ThTp,Env,Ev,Exp,Path).
typeOfExp(Term,Tp,Env,Env,reset(Lc,Lam,Tp),Path) :-
  isReset(Term,Lc,Tg,Bnd),!,
  tagType(Tp,TTp),
  typeOfPtn(Tg,TTp,Env,Ev0,Ptn,Path),
  typeOfExp(Bnd,Tp,Ev0,_Ev,Exp,Path),
  lambdaLbl(Path,"ρ",Lbl),
  Lam = lambda(Lc,Lbl,[],rule(Lc,tple(Lc,[Ptn]),none,Exp),funType(tplType([TTp]),Tp)).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isShift(Term,Lc,Tg,K,Bnd),!,
  typeOfShift(Lc,Tg,K,Bnd,Tp,Env,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isInvoke(Term,Lc,O,A),!,
  typeOfInvoke(Lc,O,A,Tp,Env,Exp,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isQBraceTuple(Term,Lc,Els),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Path).
typeOfExp(Term,Tp,Env,Env,Val,Path) :-
  isBraceTuple(Term,Lc,Els),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkRecordBody(Tp,enm(Lc,Lbl,consType(Tp,Tp)),Lc,Els,Env,Val,Path).
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
  isLetDef(Term,Lc,Els,Ex),!,
  checkLetExp(Tp,Lc,Els,Ex,Ev,LetExp,Path).
typeOfExp(Term,Tp,Ev,Ev,LetExp,Path) :-
  isLetRec(Term,Lc,Els,Ex),!,
  checkLetRec(Tp,Lc,Els,Ex,Ev,LetExp,Path).
typeOfExp(Trm,Tp,Env,Ev,Exp,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfExp(Inner,Tp,Env,Ev,Exp,Path).
typeOfExp(Trm,Tp,Env,Ev,tple(Lc,Els),Path) :-
  isTuple(Trm,Lc,A),!,
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(Trm),tplType(ArgTps),Tp,Env),
  typeOfExps(A,ArgTps,Env,Ev,Lc,Els,Path).
typeOfExp(Term,Tp,Env,Env,capply(Lc,Fun,Args,Tp),Path) :-
  isEnum(Term,Lc,I),
  isRoundTerm(I,_,F,A),!,
  genTpVars(A,Vrs),
  At = tplType(Vrs),
  typeOfExp(F,consType(At,Tp),Env,E0,Fun,Path),
  typeOfArgTerm(tuple(Lc,"()",A),At,E0,_Ev,Args,Path).
typeOfExp(Term,Tp,Env,Env,Lam,Path) :-
  isEquation(Term,_Lc,_H,_R),
  typeOfLambda(Term,Tp,Env,Lam,Path).
typeOfExp(Term,Tp,Env,Ev,valof(Lc,Act,Tp),Path) :-
  isValof(Term,Lc,A),
  isBraceTuple(A,_,[Ac]),!,
  checkAction(Ac,Tp,hasVal,Env,Ev,Act,Path).
typeOfExp(A,Tp,Env,Env,tryCatch(Lc,Body,Trw,Hndlr),Path) :-
  isTryCatch(A,Lc,B,E,H),!,
  checkTryCatch(Lc,B,E,H,Tp,Env,checker:typeOfExp,Body,Trw,Hndlr,Path).
typeOfExp(A,Tp,Env,Env,over(Lc,raise(Lc,void,ErExp,Tp),[raises(ErTp)]),Path) :-
  isRaise(A,Lc,E),!,
  newTypeVar("E",ErTp),
  typeOfExp(E,ErTp,Env,_,ErExp,Path).
typeOfExp(Term,Tp,Env,Env,Exp,Path) :-
  isRoundTerm(Term,Lc,F,A),
  typeOfRoundTerm(Lc,F,A,Tp,Env,Exp,Path).
typeOfExp(Term,Tp,Env,Env,void,_) :-!,
  locOfAst(Term,Lc),
  reportError("illegal expression: %s, expecting a %s",[Term,Tp],Lc).

verifyType(Lc,_,Actual,Expected,Env) :-
  sameType(Actual,Expected,Lc,Env),!.
verifyType(Lc,M,S,T,_) :-
  reportError("%s:%s not consistent with expected type\n%s",[M,tpe(S),tpe(T)],Lc).

brceConLbl(over(_,T,_),L) :- brceConLbl(T,L).
brceConLbl(v(_,L,_),L).
brceConLbl(enm(_,Nm,_),Nm).
brceConLbl(mtd(_,Nm,_),Nm).

% We only do partial type checking at this stage
typeOfFieldAcc(Lc,Rc,Fld,Tp,Env,Ev,dot(Lc,Rec,Fld,Tp),Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,Env,Ev,Rec,Path).

% We only do partial type checking at this stage
typeOfRecordUpdate(Lc,Rc,Fld,Vl,Tp,Env,Ev,update(Lc,Rec,Fld,Val),Path) :-
  typeOfExp(Rc,Tp,Env,Ev0,Rec,Path),
  newTypeVar("_V",VT),
  typeOfExp(Vl,VT,Ev0,Ev,Val,Path).

% same: we only partially check tuple index
typeOfTupleAcc(Lc,Rc,Fld,Tp,Env,Ev,tdot(Lc,Rec,Fld,Tp),Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,Env,Ev,Rec,Path).

typeOfRoundTerm(Lc,F,A,Tp,Env,Call,Path) :-
  newTypeVar("F",FnTp),
  genTpVars(A,Vrs),
  At = tplType(Vrs),
  typeOfExp(F,FnTp,Env,E0,Fun,Path),
  (sameType(funType(At,Tp),FnTp,Lc,E0) ->
   typeOfArgTerm(tuple(Lc,"()",A),At,E0,_Ev,Args,Path),
   Call=apply(Lc,Fun,Args,Tp);
   sameType(consType(At,Tp),FnTp,Lc,E0) ->
   typeOfArgTerm(tuple(Lc,"()",A),At,E0,_Ev,Args,Path),
   Call=capply(Lc,Fun,Args,Tp);
   reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FnTp,At,Tp],Lc),
   Call=void).

typeOfLambda(Term,Tp,Env,lambda(Lc,Lbl,Cx,rule(Lc,Args,Guard,Exp),Tp),Path) :-
%  reportMsg("expected type of lambda %s = %s",[Term,Tp]),
  getConstraints(Tp,Cx,LambdaTp),
  declareConstraints(Lc,Cx,Env,EvL),
  isEquation(Term,Lc,H,C,R),
  newTypeVar("_A",AT),
  typeOfArgPtn(H,AT,EvL,E0,Args,Path),
  checkGuard(C,E0,E1,Guard,Path),
  newTypeVar("_E",RT),
  verifyType(Lc,ast(Term),funType(AT,RT),LambdaTp,Env),
  lambdaLbl(Path,"λ",Lbl),
  typeOfExp(R,RT,E1,_,Exp,Path).

typeOfThunk(Lc,Term,Tp,Env,
	    thunk(Lc,
		  lambda(Lc,Lbl,[],rule(Lc,tple(Lc,[ThnkVar]),none,thnkSet(Lc,ThnkVar,Exp)),funType(tplType([Tp]),ThT)),
		  Tp),Path) :-
  newTypeVar("t",ThT),
  genNewName(Path,"Γ",ThNme),
  ThnkVar = v(Lc,ThNme,Tp),
  lambdaLbl(Path,"λ",Lbl),
  thunkType(ThT,ThTp),
  verifyType(Lc,ast(Term),ThTp,Tp,Env),
  typeOfExp(Term,ThT,Env,_,Exp,Path).

typeOfShift(Lc,Tg,K,Bnd,Tp,Env,shift(Lc,Tag,Lam,Tp),Path):-
  newTypeVar("A",AnsTp),
  tagType(AnsTp,TgTp),
  typeOfExp(Tg,TgTp,Env,Ev0,Tag,Path),
  contType(Tp,AnsTp,KTp),
  typeOfPtn(K,KTp,Ev0,Ev1,KV,Path),
  typeOfExp(Bnd,AnsTp,Ev1,_,Shft,Path),
  lambdaLbl(Path,"σ",Lbl),
  Lam = lambda(Lc,Lbl,[],rule(Lc,tple(Lc,[KV]),none,Shft),funType(tplType([KTp]),AnsTp)).

typeOfInvoke(Lc,O,A,Tp,Env,invoke(Lc,K,Ar,Tp),Path) :-
  newTypeVar("A",Atp),
  contType(Atp,Tp,KTp),
  typeOfExp(O,KTp,Env,_E,K,Path),
  typeOfExp(A,Atp,Env,_,Ar,Path).

checkAction(A,Tp,HasVal,Env,Ev,As,Path) :-
  isBraceTuple(A,_,[S]),!,
  checkAction(S,Tp,HasVal,Env,Ev,As,Path).
checkAction(A,_Tp,HasVal,Env,Env,doNop(Lc),_) :-
  isBraceTuple(A,Lc,[]),!,
  validLastAct(A,Lc,HasVal).
checkAction(A,Tp,HasVal,Env,Ev,doSeq(Lc,L,R),Path) :-
  isActionSeq(A,Lc,A1,A2),!,
  checkAction(A1,Tp,noVal,Env,E0,L,Path),
  checkAction(A2,Tp,HasVal,E0,Ev,R,Path).
checkAction(A,Tp,HasVal,Env,Ev,Ax,Path) :-
  isActionSeq(A,_,A1),!,
  checkAction(A1,Tp,HasVal,Env,Ev,Ax,Path).
checkAction(A,Tp,HasVal,Env,Env,doLbld(Lc,Lb,Ax),Path) :-
  isLbldAction(A,Lc,L,AA),!,isIden(L,_,Lb),
  checkAction(AA,Tp,HasVal,Env,_,Ax,Path).
checkAction(A,_,_,Env,Env,doBrk(Lc,Lb),_Path) :-
  isBreak(A,Lc,L),!,isIden(L,_,Lb).
checkAction(A,Tp,_HasVal,Env,Env,doValis(Lc,ValExp),Path) :-
  isValis(A,Lc,E),!,
  typeOfExp(E,Tp,Env,_,ValExp,Path).
checkAction(A,_Tp,_HasVal,Env,Ev,doCall(Lc,Thrw),Path) :-
  isRaise(A,Lc,_E),!,
  newTypeVar("E",ErTp),
  typeOfExp(A,ErTp,Env,Ev,Thrw,Path).
checkAction(A,_Tp,HasVal,Env,Ev,doDefn(Lc,v(NLc,Nm,TV),Exp),Path) :-
  isDefn(A,Lc,L,R),
  isIden(L,NLc,Nm),!,
  newTypeVar("V",TV),
  typeOfExp(R,TV,Env,_,Exp,Path),
  declareVr(NLc,Nm,TV,none,Env,Ev),
  validLastAct(A,Lc,HasVal).
checkAction(A,_Tp,HasVal,Env,Ev,doMatch(Lc,Ptn,Exp),Path) :-
  isDefn(A,Lc,P,E),
  isTuple(P,_,_),!,
  newTypeVar("V",TV),
  typeOfPtn(P,TV,Env,Ev,Ptn,Path),
  typeOfExp(E,TV,Env,_,Exp,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,_Tp,HasVal,Env,Ev,doMatch(Lc,Ptn,Exp),Path) :-
  isMatch(A,Lc,P,E),!,
  reportWarning("Use of .= in actions is deprecated",[],Lc),
  newTypeVar("V",TV),
  typeOfPtn(P,TV,Env,Ev,Ptn,Path),
  typeOfExp(E,TV,Env,_,Exp,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,_Tp,HasVal,Env,Ev,Act,Path) :-
  isAssignment(A,Lc,P,E),!,
  checkAssignment(Lc,P,E,Env,Ev,Act,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,Tp,HasVal,Env,Env,doTryCatch(Lc,Body,Trw,Hndlr),Path) :-
  isTryCatch(A,Lc,B,E,H),!,
  checkTryCatch(Lc,B,E,H,Tp,Env,checker:tryAction(HasVal),Body,Trw,Hndlr,Path).
checkAction(A,Tp,HasVal,Env,Ev,doIfThenElse(Lc,Tst,Thn,Els),Path) :-
  isIfThenElse(A,Lc,G,T,E),!,
  checkGoal(G,Env,E0,Tst,Path),
  checkAction(T,Tp,HasVal,E0,E1,Thn,Path),
  checkAction(E,Tp,HasVal,Env,E2,Els,Path),
  mergeDict(E1,E2,Env,Ev).
checkAction(A,Tp,_HasVal,Env,Env,doIfThenElse(Lc,Tst,Thn,doNop(Lc)),Path) :-
  isIfThen(A,Lc,G,T),!,
  checkGoal(G,Env,E0,Tst,Path),
  checkAction(T,Tp,noVal,E0,_,Thn,Path).
checkAction(A,Tp,HasVal,Env,Env,doWhile(Lc,Tst,Bdy),Path) :-
  isWhileDo(A,Lc,G,B),!,
  checkGoal(G,Env,E0,Tst,Path),
  checkAction(B,Tp,noVal,E0,_,Bdy,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,Tp,HasVal,Env,Env,doLet(Lc,Decls,Defs,Ac),Path) :-
  isLetDef(A,Lc,D,B),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(checker:letExport,ThPath,Lc,D,faceType([],[]),ThEnv,OEnv,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  checkAction(B,Tp,HasVal,OEnv,_,Ac,ThPath).
checkAction(A,Tp,HasVal,Env,Env,doLetRec(Lc,Decls,Defs,Ac),Path) :-
  isLetRec(A,Lc,D,B),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(checker:letExport,ThPath,Lc,D,faceType([],[]),[],ThEnv,OEnv,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  checkAction(B,Tp,HasVal,OEnv,_,Ac,ThPath).
checkAction(A,Tp,HasVal,Env,Env,doCase(Lc,Bound,Eqns,Tp),Path) :-
  isCaseExp(A,Lc,Bnd,Cases),
  newTypeVar("B",BVr),
  typeOfExp(Bnd,BVr,Env,_,Bound,Path),
  checkCases(Cases,BVr,Tp,Env,Eqns,Eqx,Eqx,[],checker:tryAction(HasVal),Path),!.
checkAction(A,Tp,HasVal,Env,Env,doCall(Lc,Exp),Path) :-
  isRoundTerm(A,Lc,F,Args),!,
  newTypeVar("_",RTp),
  typeOfRoundTerm(Lc,F,Args,RTp,Env,Exp,Path),
  checkLastAction(A,Lc,HasVal,Tp,RTp,Env).

checkAction(A,Tp,_HasVal,Env,Env,doNop(Lc),_) :-
  locOfAst(A,Lc),
  reportError("%s:%s illegal form of action",[ast(A),tpe(Tp)],Lc).

tryAction(HasVal,A,Tp,Env,Ev,Act,Path) :-
  checkAction(A,Tp,HasVal,Env,Ev,Act,Path).

checkAssignment(Lc,P,E,Env,Ev,doDefn(Lc,Ptn,cell(Lc,Exp)),Path) :-
  isIden(P,Nm),
  \+  getVar(Lc,Nm,Env,_,_),!,
  newTypeVar("V",TV),
  mkRefTp(TV,RTp),
  typeOfPtn(P,RTp,Env,Ev,Ptn,Path),
  typeOfExp(E,TV,Env,_,Exp,Path).
checkAssignment(Lc,P,E,Env,Ev,doAssign(Lc,Ptn,Exp),Path) :-
  newTypeVar("V",TV),
  mkRefTp(TV,RTp),
  typeOfExp(P,RTp,Env,Ev,Ptn,Path),
  typeOfExp(E,TV,Env,_,Exp,Path).

validLastAct(A,Lc,hasVal(_)) :-!,
  reportError("%s not permitted to be last action",[ast(A)],Lc).
validLastAct(_,_,_).

checkLastAction(_,_,noVal,_,_,_) :-!.
checkLastAction(A,Lc,hasVal,ETp,ATp,Env) :-
  verifyType(Lc,ast(A),ATp,ETp,Env).

checkGuard(none,Env,Env,none,_) :-!.
checkGuard(some(G),Env,Ev,some(Goal),Path) :-
  checkGoal(G,Env,Ev,Goal,Path).

checkTryCatch(Lc,B,E,Hs,Tp,Env,Check,Body,v(Lc,ErNm,ErTp),Hndlr,Path) :-
  parseType(E,Env,ErTp),
  tryBlockName(Path,ErTp,ErNm),
  declareTryScope(Lc,ErTp,ErNm,Env,Ev2),
  call(Check,B,Tp,Ev2,_,Body,Path),
  checkCases(Hs,ErTp,Tp,Env,Hndlr,Eqx,Eqx,[],Check,Path),!.

tryBlockName(Path,Tp,TrBlkNm) :-
  tpName(Tp,TpNm),
  mangleName(Path,conTract,TpNm,TrBlkNm).

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

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

fieldInFace(faceType(Fields,_),Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfExps([],[],Env,Env,_,[],_).
typeOfExps([],[T|_],Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfExps([A|_],[],Env,Env,_,[],_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfExps([A|As],[ETp|ElTypes],Env,Ev,_,[Term|Els],Path) :-
  evidence(ETp,Env,_Q,ElTp),
  typeOfExp(A,ElTp,Env,E0,Term,Path),
  % reportMsg("type of argument %s |= %s",[A,ETp]),
  % dispEnv(Env),
  locOfAst(A,Lc),
  typeOfExps(As,ElTypes,E0,Ev,Lc,Els,Path).

typeOfPtns([],[],Env,Env,_,[],_) :-!.
typeOfPtns([],[T|_],Env,Env,Lc,[],_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfPtns([A|_],[],Env,Env,_,[],_) :-!,
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfPtns([A|As],[ETp|ElTypes],Env,Ev,_,[Term|Els],Path) :-
  deRef(ETp,ElTp),
  typeOfPtn(A,ElTp,Env,E0,Term,Path),
  locOfAst(A,Lc),
  typeOfPtns(As,ElTypes,E0,Ev,Lc,Els,Path).

packageExport(Defs,Public,ExportDecls,LDecls,XDefs) :-
  genDecls(Defs,checker:isPkgPublic(Public),ExportDecls,[],LDecls,[],XDefs,[]).

filterExport(Decls,Public,ExportDecls,LclDecls) :-
  filterDecls(Decls,checker:isPkgPublic(Public),ExportDecls,[],LclDecls,[]).

isPkgPublic(Public,V) :-
  is_member(V,Public),!.

isAny(_).

pkgExport(viz(Pu,_,_),V,Decl,decls(Ex,Lcl),decls([Decl|Ex],Lcl)) :-
  is_member(V,Pu),!.
pkgExport(_,_,Decl,decls(Pu,Lcl),decls(Pu,[Decl|Lcl])).

letExport(viz(Pu,Df,_),V,Decl,decls(Ex,Lcl),decls([Decl|Ex],Lcl)) :-
  (is_member(V,Pu);is_member(V,Df)),!.
letExport(_,_,Decl,decls(Ex,Lcl),decls(Ex,[Decl|Lcl])).

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
genDecl(varDef(Lc,Nm,FullNm,[],Tp,lambda(_,_,Cx,Eqn,_)),_,Public,
	 [funDec(Nm,FullNm,Tp)|Ex],Ex,Lx,Lx,
	 [funDef(Lc,Nm,FullNm,hard,Tp,Cx,[Eqn])|Dfx],Dfx) :-
  call(Public,var(Nm)),!.
genDecl(varDef(Lc,Nm,FullNm,[],Tp,lambda(_,_,Cx,Eqn,OTp)),_,Public,
	Ex,Ex,[funDec(Nm,FullNm,Tp)|Lx],Lx,
	[funDef(Lc,Nm,FullNm,hard,OTp,Cx,[Eqn])|Dfx],Dfx) :-
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

filterDecls([],_,Exx,Exx,LDx,LDx).
filterDecls([Decl|Decls],Public,Exports,Exx,LDecls,LDx) :-
  filterDecl(Decl,Decl,Public,Exports,Ex0,LDecls,LD0),
  filterDecls(Decls,Public,Ex0,Exx,LD0,LDx).

filterDecl(funDec(Nm,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  call(Public,var(Nm)),!.
filterDecl(typeDec(Nm,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  call(Public,tpe(Nm)),!.
filterDecl(funDec(Nm,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  call(Public,var(Nm)),!.
filterDecl(varDec(Nm,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  call(Public,var(Nm)),!.
filterDecl(cnsDec(Nm,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  call(Public,var(Nm)),!.
filterDecl(contractDec(Nm,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  call(Public,con(Nm)),!.
filterDecl(impDec(_,_,Spec),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  exportImp(Spec,Public),!.
filterDecl(accDec(Tp,_,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  exportAcc(Tp,Public),!.
filterDecl(updDec(Tp,_,_,_),Decl,Public,[Decl|Ex],Ex,Lx,Lx) :-
  exportAcc(Tp,Public),!.
filterDecl(_,Decl,_,Ex,Ex,[Decl|Lx],Lx).

computeThetaExport(Defs,Fields,Public,Decls,XDefs) :-
  genDecls(Defs,checker:isThetaPublic(Fields,Public),Decls,LDecls,LDecls,[],XDefs,[]).

filterTheta(Decls,Fields,Public,XDecls,LDecls) :-
  filterDecls(Decls,checker:isThetaPublic(Fields,Public),XDecls,[],LDecls,[]).

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
completePublic([con(Nm)|List],Pub,[tpe(ConNm),var(DlNm)|Px],Path) :-
  contractName(Path,Nm,ConNm),
  dollarName(Nm,DlNm),
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

exportImp(Spec,Export) :-
%  reportMsg("check for export %s",[tpe(Spec)]),
  tpName(Spec,SpNm),
%  localName(SpNm,type,Cn),
  tpArgs(Spec,Tps),
%  localNames(Tps,Ns),
  lclImplName(SpNm,Tps,Imn),
  call(Export,imp(Imn)).

localNames([],[]).
localNames([T|Ts],[N|Ns]) :-
  localName(T,type,N),
  localNames(Ts,Ns).

mkBoot(Env,Lc,Pkg,Dfs,[BootDef|Dfs],Decls,[funDec("_boot",BootNm,BootTp)|Decls]) :-
  findType("cons",Lc,Env,ConsTp),
  getVar(Lc,"_main",Env,MainTrm,MnTp),
  mangleName(Pkg,value,"_boot",BootNm),
  applyTypeFun(ConsTp,[type("string")],Lc,Env,LSTp),
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
