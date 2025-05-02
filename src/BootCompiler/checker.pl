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
:- use_module(meta).
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
  thetaEnv(checker:pkgExport,Opts,Pk,Lc,Stmts,faceType([],[]),Env0,_OEnv,Defs,decls(ThEx,ThL)),
  (is_member(traceCheck,Opts) ->
   reportMsg("type check before resolving overloads %s",[defs(Defs)],Lc);
   true),
  declareAllDecls(ThEx,Lc,Env0,Env1),
  declareAllDecls(ThL,Lc,Env1,Env2),
  overload(Defs,Env2,Opts,ODefs),
  Canon=prog(Pkg,Imports,ThEx,ThL,ODefs),
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

collectImportDecls([],_,Decls,Decls) :-!.
collectImportDecls([importPk(_Lc,_Viz,Pkg)|More],Repo,Decls,Dcx) :-
  importPkg(Pkg,Repo,spec(_,_,PDecls)),
  concat(PDecls,Decls,Dc0),
  collectImportDecls(More,Repo,Dc0,Dcx).

thetaEnv(Publish,Opts,Pkg,Lc,Stmts,Face,Base,TheEnv,Defs,Decls) :-
  collectDefinitions(Stmts,Dfs,viz([],[],[]),Viz,Annots),!,
  dependencies(Dfs,Opts,Groups,Annots),
  pushFace(Face,Lc,Base,Env),
  checkGroups(Groups,Face,Annots,Defs,[],Env,TheEnv,Publish,Viz,decls([],[]),Decls,Opts,Pkg).

recordEnv(Publish,Opts,Path,_Lc,Stmts,Fields,Base,TheEnv,Defs,Decls) :-
  collectDefinitions(Stmts,Dfs,viz([],[],[]),Viz,Annots),!,
  parseAnnotations(Dfs,Fields,Annots,Base,Opts,Path,Face),
  checkGroup(Dfs,Defs,[],Base,TheEnv,Face,Publish,Viz,decls([],[]),Decls,Opts,Path).

checkGroups([],_,_,Defs,Defs,E,E,_,_,Dc,Dc,_,_).
checkGroups([Gp|More],Fields,Annots,Defs,Dfx,E,Ev,Publish,Viz,Dc,Dcx,Opts,Path) :-
  parseAnnotations(Gp,Fields,Annots,E,Opts,Path,Face),!,
  groupLc(Gp,Lc),
  pushFace(Face,Lc,E,E0),
  checkGroup(Gp,Defs,Df2,E0,E3,Face,Publish,Viz,Dc,Dc0,Opts,Path),!,
  checkGroups(More,Fields,Annots,Df2,Dfx,E3,Ev,Publish,Viz,Dc0,Dcx,Opts,Path).

groupLc([(_,Lc,_)|_],Lc).

checkGroup([(con(N),Lc,[ConStmt])|More],[Contract|Defs],Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  parseContract(ConStmt,Env,E0,Opts,Path,[Contract|Defs],Df0,Publish,Viz,Dc,Dc0),
  defineContract(N,Lc,Contract,E0,E1),
  checkGroup(More,Df0,Dx,E1,Ex,Face,Publish,Viz,Dc0,Dcx,Opts,Path).
checkGroup([(cns(Nm),Lc,[St])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  parseConstructor(Nm,Lc,St,Opts,Env,E0,Defs,D0,Publish,Viz,Dc,Dc0,Opts,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Opts,Path).
checkGroup([(var(N),Lc,Stmts)|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  checkVarRules(N,Lc,Stmts,Env,E0,Defs,D0,Face,Publish,Viz,Dc,Dc0,Opts,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Opts,Path).
checkGroup([(imp(Nm),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  checkImplementation(Stmt,Nm,Defs,D0,Env,E0,Face,Publish,Viz,Dc,Dc0,Opts,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Opts,Path).
checkGroup([(tpe(_),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  parseTypeDef(Stmt,Defs,D0,Env,E0,Publish,Viz,Dc,Dc0,Opts,Path),
  checkGroup(More,D0,Dx,E0,Ex,Face,Publish,Viz,Dc0,Dcx,Opts,Path).
checkGroup([(open(_),_,[Stmt])|More],Defs,Dx,Env,Ex,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  checkOpenStmt(Stmt,Env,Ev0,Publish,Viz,Dc,Dc0,Opts,Path),
  checkGroup(More,Defs,Dx,Ev0,Ex,Face,Publish,Viz,Dc0,Dcx,Opts,Path).
checkGroup([],Defs,Defs,Env,Env,_,_,_,Dcx,Dcx,_Opts,_).

parseConstructor(Nm,Lc,T,Opts,Env,Ev,[Defn|Defs],Defs,Publish,Viz,Dc,Dcx,Opts,Path) :-
  (is_member(traceCheck,Opts) -> 
     reportMsg("parse constructor type %s:%s",[id(Nm),ast(T)],Lc);
   true),
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

parseAnnotations(Defs,Fields,Annots,Env,Opts,Path,faceType(F,T)) :-
  parseAnnots(Defs,Fields,Annots,Env,[],F,[],T,Opts,Path).

parseAnnots([],_,_,_,Face,Face,Tps,Tps,_,_) :-!.
parseAnnots([(var(Nm),Lc,Stmts)|More],Fields,Annots,Env,F0,Face,T,Tps,Opts,Path) :-
  parseAnnotation(Nm,Lc,Stmts,Fields,Annots,Env,F0,F1),
  parseAnnots(More,Fields,Annots,Env,F1,Face,T,Tps,Opts,Path).
parseAnnots([(tpe(N),Lc,[Stmt])|More],Fields,Annots,Env,F,Face,T,Tps,Opts,Path) :-
  defineType(N,Lc,Stmt,Env,T,T1,Opts,Path),
  parseAnnots(More,Fields,Annots,Env,F,Face,T1,Tps,Opts,Path).
parseAnnots([_|More],Fields,Annots,Env,F,Face,T,Tps,Opts,Path) :-
  parseAnnots(More,Fields,Annots,Env,F,Face,T,Tps,Opts,Path).

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

defineType(N,_,_,Env,T,[(N,Tp)|T],_,_) :-
  isType(N,Env,tpDef(_,Tp,_)),!.
defineType(N,_,St,Env,T,[(N,Type)|T],_Opts,Path) :-
  parseTypeCore(St,Type,Env,Path).
defineType(_,Lc,St,_,T,T,_,_) :-
  reportError("cannot parse type statement %s",[St],Lc).

checkOpenStmt(Stmt,Env,Ev,Opts,Path) :-
  isOpen(Stmt,Lc,Vr),!,
  newTypeVar("_",VrTp),
  typeOfExp(Vr,VrTp,voidType,Env,E0,Rc,Opts,Path),
  faceOfType(VrTp,Lc,E0,faceType(Flds,Tps)),
  declareExported(Flds,Rc,Lc,Env,E1),
  declareExportedTypes(Tps,Rc,Lc,E1,Ev).

declareExported([],_,_,Env,Env).
declareExported([(Nm,Tp)|More],Rc,Lc,Env,Ev) :-
  declareField(Lc,Rc,Nm,Tp,Env,Ev0),
  declareExported(More,Rc,Lc,Ev0,Ev).

declareExportedTypes(_,_,_,Env,Env).

checkVarRules(N,Lc,Stmts,E,Ev,Defs,Dx,Face,Publish,Viz,Dc,Dcx,Opts,Path) :-
  pickupDefnType(N,Lc,Face,Stmts,E,E0,Tp),
  evidence(Tp,E,Q,ETp),
  getConstraints(ETp,Cx,ProgramType),
  declareTypeVars(Q,Lc,E0,E1),
  declareConstraints(Lc,Cx,E1,E2),
  processStmts(Stmts,ProgramType,Rules,Deflts,Deflts,[],E2,Opts,Path),
  qualifiedName(Path,N,LclName),
  formDefn(Rules,N,LclName,E,Ev,Tp,Cx,Defs,Dx,Publish,Viz,Dc,Dcx),
  checkOpt(Opts,traceCheck,meta:showMsg(Lc,"type of %s:%s",[id(N),tpe(ProgramType)])).

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

processStmts([],_,Defs,Defs,Dflts,Dflts,_,_,_).
processStmts([St|More],ProgramType,Defs,Dx,Df,Dfx,E0,Opts,Path) :-
  processStmt(St,ProgramType,Defs,D0,Df,Df0,E0,Opts,Path),!,
  processStmts(More,ProgramType,D0,Dx,Df0,Dfx,E0,Opts,Path).

processStmt(St,ProgramType,Defs,Defx,Df,Dfx,E,Opts,Path) :-
  isEquation(St,Lc,L,Cond,R),!,
  checkOpt(Opts,traceCheck,meta:showMsg(Lc,"check equation %s\nagainst %s",[ast(St),tpe(ProgramType)])),
  checkEquation(Lc,L,Cond,R,ProgramType,Defs,Defx,Df,Dfx,E,Opts,Path).
processStmt(St,Tp,[Def|Defs],Defs,Df,Df,Env,Opts,Path) :-
  isDefn(St,Lc,L,R),
  checkDefn(Lc,L,R,Tp,Def,Env,Opts,Path),!.
processStmt(St,_,Defs,Defs,Df,Df,_,_,_) :-
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

checkEquation(Lc,H,C,R,ProgramType,Defs,Defsx,Df,Dfx,E,Opts,Path) :-
  splitUpProgramType(ProgramType,AT,RT,ErTp),
  splitHead(H,_,A,IsDeflt),
  pushScope(E,Env),
  typeOfArgPtn(A,AT,ErTp,Env,E0,Args,Opts,Path),
  checkGuard(C,ErTp,E0,E1,Guard,Opts,Path),
  typeOfExp(R,RT,ErTp,E1,_E2,Exp,Opts,Path),
  Eqn = rule(Lc,Args,Guard,Exp),
  checkOpt(Opts,traceCheck,meta:showMsg(Lc,"rule: %s",[rle(Eqn)])),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[Eqn|Dfx]; Defs=[Eqn|Defsx],Df=Dfx).
checkEquation(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_,_) :-
  reportError("rule not consistent with expected type: %s",[ProgramType],Lc).

splitUpProgramType(funType(AT,RT),AT,RT,voidType) :-!.
splitUpProgramType(funType(AT,RT,ET),AT,RT,ET).

checkDefn(Lc,L,R,VlTp,varDef(Lc,Nm,ExtNm,[],VlTp,Value),Env,Opts,Path) :-
  isIden(L,_,Nm),
  pushScope(Env,E),
  typeOfExp(R,VlTp,voidType,E,_E2,Value,Opts,Path),
  qualifiedName(Path,Nm,ExtNm).

checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Opts,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  genNewName(Path,"Γ",ThPath),
  faceOfType(ETp,Lc,Env,TpFace),
  getConstraints(TpFace,_,faceType(Fs,_)),
  thetaEnv(checker:letExport,Opts,ThPath,Lc,Els,Face,BaseEnv,_,Defs,ThDecls),
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

checkRecordBody(Tp,Lbl,Lc,Els,Env,letExp(Lc,LDecls,Defs,Exp),Opts,Path) :-
  evidence(Tp,Env,Q,ETp),
  faceOfType(ETp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,faceType(Fs,Ts)),
  pushScope(Env,Base),
  declareTypeVars(Q,Lc,Base,E0),
  declareConstraints(Lc,Cx,E0,BaseEnv),
  genNewName(Path,"Γ",ThPath),
  recordEnv(checker:letExport,Opts,ThPath,Lc,Els,faceType(Fs,Ts),BaseEnv,_,Defs,RDecls),
  sort(Fs,checker:cmpPair,SortedFlds),
  findExportedDefs(Lc,SortedFlds,Args),
  Exp = capply(Lc,Lbl,tple(Lc,Args),Tp),
  mergeDecls(RDecls,LDecls).

checkLetRec(Tp,ErTp,Lc,Els,Ex,Env,letRec(Lc,Decls,Defs,Bound),Opts,Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(checker:letExport,Opts,ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  typeOfExp(Ex,Tp,ErTp,OEnv,_,Bound,Opts,Path).

checkLetExp(Tp,ErTp,Lc,Els,Ex,Env,letExp(Lc,LDecls,Defs,Bound),Opts,Path):-
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(checker:letExport,Opts,ThPath,Lc,Els,faceType([],[]),ThEnv,OEnv,Defs,Decls),
  typeOfExp(Ex,Tp,ErTp,OEnv,_,Bound,Opts,Path),
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
		    Publish,Viz,Dc,Dcx,Opts,Path) :-
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
  typeOfExp(ImpBody,CnType,voidType,ThEnv,_ThEv,ImplTerm,Opts,ImplVrNm),
  (is_member(traceCheck,Opts) -> 
     reportMsg("implementation %s:%s",[can(ImplTerm),tpe(CnType)]);
   true),
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
checkImplementation(Stmt,_,Defs,Defs,Env,Env,_,_,_,Dcx,Dcx,_,_) :-
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
typeOfArgPtn(T,Tp,ErTp,Env,Ev,tple(Lc,Els),Opts,Path) :-
  isTuple(T,Lc,A),!,
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(T),tplType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,ErTp,Env,Ev,Lc,Els,Opts,Path).
typeOfArgPtn(T,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  typeOfPtn(T,Tp,ErTp,Env,Ev,Exp,Opts,Path).

typeOfPtn(V,Tp,_ErTp,Env,Env,anon(Lc,Tp),_Opts,_Path) :-
  isAnon(V,Lc),!.
typeOfPtn(V,Tp,ErTp,Env,Ev,Term,Opts,Path) :-
  isIden(V,Lc,N),
  getVar(Lc,N,Env,_,_),!,
  mkWhereEquality(Lc,V,TT),
  typeOfPtn(TT,Tp,ErTp,Env,Ev,Term,Opts,Path).
typeOfPtn(V,Tp,_ErTp,Ev,Env,v(Lc,N,Tp),_Opts,_Path) :-
  isIden(V,Lc,N),
  declareVr(Lc,N,Tp,none,Ev,Env).
typeOfPtn(Trm,Tp,ErTp,Env,Ev,Term,Opts,Path) :-
  isEnum(Trm,_,N),isIden(N,_,_),!,
  typeOfExp(Trm,Tp,ErTp,Env,Ev,Term,Opts,Path).
typeOfPtn(Trm,Tp,_ErTp,Env,Env,intLit(Lc,Ix),_,_) :-
  isLiteralInteger(Trm,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  verifyType(Lc,ast(Trm),IntTp,Tp,Env).
typeOfPtn(T,Tp,_ErTp,Env,Env,bigLit(Lc,Bx),_Opts,_Path) :-
  isLiteralBigInt(T,Lc,Bx),!,
  findType("bigint",Lc,Env,BigTp),
  verifyType(Lc,ast(T),BigTp,Tp,Env).
typeOfPtn(T,Tp,_ErTp,Env,Env,floatLit(Lc,Dx),_Opts,_Path) :-
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  verifyType(Lc,ast(T),FltTp,Tp,Env).
typeOfPtn(char(Lc,Cp),Tp,_ErTp,Env,Env,charLit(Lc,Cp),_Opts,_Path) :- !,
  findType("char",Lc,Env,StrTp),
  verifyType(Lc,ast(char(Lc,Cp)),StrTp,Tp,Env).
typeOfPtn(string(Lc,Sx),Tp,_ErTp,Env,Env,stringLit(Lc,Sx),_Opts,_Path) :- !,
  findType("string",Lc,Env,StrTp),
  verifyType(Lc,ss(Sx),StrTp,Tp,Env).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,RT),
  verifyType(Lc,ast(Term),RT,Tp,Env),
  typeOfPtn(L,Tp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfPtn(P,Tp,ErTp,Env,Ev,where(Lc,Ptn,Cond),Opts,Path) :-
  isWhere(P,Lc,L,C),
  typeOfPtn(L,Tp,ErTp,Env,E0,Ptn,Opts,Path),
  checkGuard(some(C),ErTp,E0,Ev,some(Cond),Opts,Path).
typeOfPtn(Trm,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfPtn(Inner,Tp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfPtn(Trm,Tp,ErTp,Env,Ev,tple(Lc,Els),Opts,Path) :-
  isTuple(Trm,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(Trm),tplType(ArgTps),Tp,Env),
  typeOfPtns(A,ArgTps,ErTp,Env,Ev,Lc,Els,Opts,Path).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isConApply(Term,Lc,F,A),!,
  newTypeVar("A",At),
  typeOfExp(F,consType(At,Tp),ErTp,Env,E0,Fun,Opts,Path),
  typeOfArgPtn(tuple(Lc,"()",A),At,ErTp,E0,Ev,Args,Opts,Path),
  Exp = capply(Lc,Fun,Args,Tp).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isRoundTerm(Term,Lc,F,A),
  mkConApply(Lc,F,A,TT),
  reportWarning("this form of pattern: %s is deprecated, use %s",
		[ast(Term),ast(TT)],Lc),
  newTypeVar("A",At),
  typeOfExp(F,consType(At,Tp),ErTp,Env,E0,Fun,Opts,Path),
%  reportMsg("con type = %s",[tpe(consType(At,Tp))],Lc),
  typeOfArgPtn(tuple(Lc,"()",A),At,ErTp,E0,Ev,Args,Opts,Path),
  Exp = capply(Lc,Fun,Args,Tp).
typeOfPtn(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  (isBraceTerm(Term,Lc,F,Args);isQBraceTerm(Term,Lc,F,Args)),
  typeOfRecordPtn(Lc,Tp,ErTp,F,Args,Env,Ev,Exp,Opts,Path).
typeOfPtn(Term,Tp,_ErTp,Env,Env,void,_,_) :-
  locOfAst(Term,Lc),
  reportError("illegal pattern: %s, expecting a %s",[ast(Term),tpe(Tp)],Lc).

typeOfRecordPtn(Lc,Tp,ErTp,F,Args,Env,Ev,Exp,Opts,Path) :-
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),ErTp,Env,E0,Fun,Opts,Path),
  faceOfType(FnTp,Lc,Env,FaceTp),
  getConstraints(FaceTp,Cx,Face),
  pushScope(E0,E1),
  declareConstraints(Lc,Cx,E1,BaseEnv),
  typeOfElementPtns(Args,Face,ErTp,BaseEnv,Ev,PtnDefs,[],Opts,Path),
  fillinElementPtns(PtnDefs,Lc,FaceTp,ArgPtns),
  Exp = capply(Lc,Fun,tple(Lc,ArgPtns),Tp),
  (is_member(traceCheck,Opts) -> 
     reportMsg("record ptn = %s",[can(Exp)],Lc);
   true).

typeOfElementPtns([],_Face,_ErTp,Env,Env,Defs,Defs,_Opts,_Path).
typeOfElementPtns([E|Els],Face,ErTp,Env,Ev,Defs,Dfx,Opts,Path) :-
  elementPtn(E,Face,ErTp,Env,E0,Defs,Df0,Opts,Path),
  typeOfElementPtns(Els,Face,ErTp,E0,Ev,Df0,Dfx,Opts,Path).

elementPtn(E,Face,ErTp,Env,Ev,[(Nm,Ptn)|Defs],Defs,Opts,Path) :-
  isDefn(E,Lc,Lhs,Rhs),
  isIden(Lhs,_,Nm),
  fieldInFace(Face,Nm,Lc,Tp),
  typeOfPtn(Rhs,Tp,ErTp,Env,Ev,Ptn,Opts,Path).

fillinElementPtns(Els,Lc,faceType(Flds,_),Args) :-
  rfold(Flds,checker:fillinElementPtn(Lc),Els,NEls),
  sort(NEls,checker:cmpPair,Elements),
  project1(Elements,Args).

cmpPair((N1,_),(N2,_)) :-
  str_lt(N1,N2).

fillinElementPtn(_,(Nm,_),Els,Els) :-
  is_member((Nm,_),Els) ,!.
fillinElementPtn(Lc,(Nm,Tp),Els,[(Nm,anon(Lc,Tp))|Els]).
  
typeOfArgTerm(T,Tp,ErTp,Env,Ev,tple(Lc,Els),Opts,Path) :-
  isTuple(T,Lc,A),
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(T),tplType(ArgTps),Tp,Env),
  typeOfExps(A,ArgTps,ErTp,Env,Ev,Lc,Els,Opts,Path).
typeOfArgTerm(T,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  typeOfExp(T,Tp,ErTp,Env,Ev,Exp,Opts,Path).

typeOfExp(V,Tp,_ErTp,Env,Env,anon(Lc,Tp),_,_) :-
  isAnon(V,Lc),
  reportError("anonymous variable not permitted as expression",[],Lc).
typeOfExp(V,Tp,_ErTp,Env,Env,Term,_Opts,_Path) :-
  isIden(V,Lc,N),!,
  (getVar(Lc,N,Env,Term,VTp) ->
   verifyType(Lc,ast(V),VTp,Tp,Env);
   reportError("variable '%s' not defined, expecting a %s",[V,Tp],Lc),
   Term=void).
typeOfExp(T,Tp,ErTp,Env,Ev,Term,Opts,Path) :-
  isEnum(T,_,N),isIden(N,_,_),!,
  typeOfExp(N,consType(tplType([]),Tp),ErTp,Env,Ev,Term,Opts,Path),!.
typeOfExp(T,Tp,_ErTp,Env,Env,intLit(Lc,Ix),_Opts,_Path) :-
  isLiteralInteger(T,Lc,Ix),!,
  findType("integer",Lc,Env,IntTp),
  verifyType(Lc,ast(T),IntTp,Tp,Env).
typeOfExp(T,Tp,_ErTp,Env,Env,bigLit(Lc,Ix),_Opts,_Path) :-
  isLiteralBigInt(T,Lc,Ix),!,
  findType("bigint",Lc,Env,BigTp),
  verifyType(Lc,ast(T),BigTp,Tp,Env).
typeOfExp(T,Tp,_ErTp,Env,Env,floatLit(Lc,Dx),_Opts,_Path) :-
  isLiteralFloat(T,Lc,Dx),!,
  findType("float",Lc,Env,FltTp),
  verifyType(Lc,ast(T),FltTp,Tp,Env).
typeOfExp(char(Lc,Cp),Tp,_ErTp,Env,Env,charLit(Lc,Cp),_Opts,_Path) :- !,
  findType("char",Lc,Env,StrTp),
  verifyType(Lc,ast(char(Lc,Cp)),StrTp,Tp,Env).
typeOfExp(string(Lc,Sx),Tp,_ErTp,Env,Env,stringLit(Lc,Sx),_Opts,_Path) :- !,
  findType("string",Lc,Env,StrTp),
  verifyType(Lc,ss(Sx),StrTp,Tp,Env).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isTypeAnnotation(Term,Lc,L,R),!,
  parseType(R,Env,PTp),
  verifyType(Lc,ast(Term),PTp,Tp,Env),
  typeOfExp(L,PTp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isSuppress(Term,Lc,V),
  (isIden(V,VLc,N) ->
   Env=Ev,
   (getNRVar(Lc,N,Env,Exp,VTp) ->
    verifyType(Lc,ast(V),VTp,Tp,Env);
    reportError("variable '%s' not defined, expecting a %s",[V,Tp],VLc),
    Term=void);
   reportError("expecting an identifier, not '%s'",[V],Lc),
   typeOfExp(V,Tp,ErTp,Env,Ev,Exp,Opts,Path)).
typeOfExp(P,Tp,ErTp,Env,Ex,where(Lc,Ptn,Cond),Opts,Path) :-
  isWhere(P,Lc,L,C),!,
  typeOfExp(L,Tp,ErTp,Env,E0,Ptn,Opts,Path),
  checkGuard(some(C),ErTp,E0,Ex,some(Cond),Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isFieldAcc(Term,Lc,Rc,Fld),!,
  typeOfFieldAcc(Lc,Rc,Fld,Tp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,U,Opts,Path) :-
  isRecordUpdate(Term,Lc,Rc,Fld,Vl),!,
  typeOfRecordUpdate(Lc,Rc,Fld,Vl,Tp,ErTp,Env,Ev,U,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isTupleAcc(Term,Lc,Rc,Fld),!,
  typeOfTupleAcc(Lc,Rc,Fld,Tp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,cond(Lc,Test,Then,Else,Tp),Opts,Path) :-
  isConditional(Term,Lc,Tst,Th,El),!,
  checkGoal(Tst,ErTp,Env,E0,Test,Opts,Path),
  typeOfExp(Th,Tp,ErTp,E0,E1,Then,Opts,Path),
  typeOfExp(El,Tp,ErTp,Env,E2,Else,Opts,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,ErTp,Env,Ev,conj(Lc,Lhs,Rhs),Opts,Path) :-
  isConjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,ErTp,Env,E1,Lhs,Opts,Path),
  typeOfExp(R,LogicalTp,ErTp,E1,Ev,Rhs,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,disj(Lc,Lhs,Rhs),Opts,Path) :-
  isDisjunct(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,ErTp,Env,E1,Lhs,Opts,Path),
  typeOfExp(R,LogicalTp,ErTp,Env,E2,Rhs,Opts,Path),
  mergeDict(E1,E2,Env,Ev).
typeOfExp(Term,Tp,ErTp,Env,Env,implies(Lc,Lhs,Rhs),Opts,Path) :-
  isForall(Term,Lc,L,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(L,LogicalTp,ErTp,Env,E1,Lhs,Opts,Path),
  typeOfExp(R,LogicalTp,ErTp,E1,_Ev,Rhs,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,neg(Lc,Rhs),Opts,Path) :-
  isNegation(Term,Lc,R),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  typeOfExp(R,LogicalTp,ErTp,Env,_Ex,Rhs,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,match(Lc,Lhs,Rhs),Opts,Path) :-
  isMatch(Term,Lc,P,E),!,
  findType("boolean",Lc,Env,LogicalTp),
  verifyType(Lc,ast(Term),LogicalTp,Tp,Env),
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,ErTp,Env,Ev,Lhs,Opts,Path),
  typeOfExp(E,TV,ErTp,Env,_,Rhs,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,case(Lc,Bound,Eqns,Tp),Opts,Path) :-
  isCaseExp(Term,Lc,Bnd,Cases),
  checkCaseExp(Lc,Bnd,Cases,Tp,ErTp,Env,Ev,checker:typeOfExp,Bound,Eqns,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,cell(Lc,Exp),Opts,Path) :-
  isRef(Term,Lc,I),
  newTypeVar("r",RT),
  mkRefTp(RT,RTp),
  verifyType(Lc,ast(Term),RTp,Tp,Env),
  typeOfExp(I,RT,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,deref(Lc,Exp),Opts,Path) :-
  isCellRef(Term,Lc,I),
  mkRefTp(Tp,RTp),
  typeOfExp(I,RTp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Thnk,Opts,Path) :-
  isThunk(Term,Lc,Th),!,
  typeOfThunk(Lc,Th,Tp,Env,Thnk,Opts,Path),
  (is_member(traceCheck,Opts) -> 
   reportMsg("thunk expression  %s:%s",[can(Thnk),tpe(Tp)],Lc);
   true).
typeOfExp(Term,Tp,ErTp,Env,Ev,thnkRef(Lc,Exp,Tp),Opts,Path) :-
  isThunkRef(Term,Lc,Rf),!,
  thunkType(Tp,ThTp),
  typeOfExp(Rf,ThTp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Val,Opts,Path) :-
  isQBraceTuple(Term,Lc,Els),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkThetaBody(Tp,Lbl,Lc,Els,Env,Val,Opts,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Val,Opts,Path) :-
  isBraceTuple(Term,Lc,Els),
  reportError("anonymous brace expression %s not supported",[ast(Term)],Lc),
  tpName(Tp,Lbl),
  checkRecordBody(Tp,enm(Lc,Lbl,consType(Tp,Tp)),Lc,Els,Env,Val,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,Val,Opts,Path) :-
  isBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),ErTp,Env,E0,Fun,Opts,Path),
  checkRecordBody(FnTp,Fun,Lc,Els,E0,Val,Opts,Path).
%  reportMsg("labeled record %s:%s",[can(Val),tpe(Tp)],Lc).
typeOfExp(Term,Tp,ErTp,Env,Env,Val,Opts,Path) :-
  isQBraceTerm(Term,Lc,F,Els),
  newTypeVar("F",FnTp),
  typeOfExp(F,consType(FnTp,Tp),ErTp,Env,E0,Fun,Opts,Path),
  brceConLbl(Fun,Lbl),
  checkThetaBody(FnTp,Lbl,Lc,Els,E0,Val,Opts,Path).
typeOfExp(Term,Tp,ErTp,Ev,Ev,LetExp,Opts,Path) :-
  isLetDef(Term,Lc,Els,Ex),!,
  checkLetExp(Tp,ErTp,Lc,Els,Ex,Ev,LetExp,Opts,Path).
typeOfExp(Term,Tp,ErTp,Ev,Ev,LetExp,Opts,Path) :-
  isLetRec(Term,Lc,Els,Ex),!,
  checkLetRec(Tp,ErTp,Lc,Els,Ex,Ev,LetExp,Opts,Path).
typeOfExp(Trm,Tp,ErTp,Env,Ev,Exp,Opts,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  typeOfExp(Inner,Tp,ErTp,Env,Ev,Exp,Opts,Path).
typeOfExp(Trm,Tp,ErTp,Env,Ev,tple(Lc,Els),Opts,Path) :-
  isTuple(Trm,Lc,A),!,
  genTpVars(A,ArgTps),
  verifyType(Lc,ast(Trm),tplType(ArgTps),Tp,Env),
  typeOfExps(A,ArgTps,ErTp,Env,Ev,Lc,Els,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,capply(Lc,Fun,Args,Tp),Opts,Path) :-
  isEnum(Term,Lc,I),
  isRoundTerm(I,_,F,A),!,
  genTpVars(A,Vrs),
  At = tplType(Vrs),
  typeOfExp(F,consType(At,Tp),ErTp,Env,E0,Fun,Opts,Path),
  typeOfArgTerm(tuple(Lc,"()",A),At,ErTp,E0,_Ev,Args,Opts,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,Lam,Opts,Path) :-
  isEquation(Term,_Lc,_H,_R),
  typeOfLambda(Term,Tp,Env,Lam,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Ev,valof(Lc,Act,Tp),Opts,Path) :-
  isValof(Term,Lc,A),
  isBraceTuple(A,_,[Ac]),!,
  checkAction(Ac,Tp,ErTp,hasVal,Env,Ev,Act,Opts,Path),
  (is_member(traceCheck,Opts) -> 
   reportMsg("action %s:%s/%s",[cnact(Act),tpe(Tp),tpe(ErTp)],Lc);
   true).
typeOfExp(A,Tp,ErTp,Env,Env,tryCatch(Lc,Body,Trw,Hndlr),Opts,Path) :-
  isTryCatch(A,Lc,B,E,H),!,
  checkTryCatch(Lc,B,E,H,Tp,ErTp,Env,checker:typeOfExp,Body,Trw,Hndlr,Opts,Path).
typeOfExp(A,Tp,OErTp,Env,Env,over(Lc,raise(Lc,void,ErExp,Tp),raises(ErTp)),Opts,Path) :-
  isRaise(A,Lc,E),!,
  newTypeVar("E",ErTp),
  typeOfExp(E,ErTp,OErTp,Env,_,ErExp,Opts,Path).
typeOfExp(A,Tp,OErTp,Env,Env,try(Lc,Body,ErTp,Hndlr),Opts,Path) :-
  isTry(A,Lc,B,H),!,
  checkTry(B,H,ErTp,Tp,OErTp,Env,checker:typeOfExp,Body,Hndlr,Opts,Path).
typeOfExp(A,Tp,ErTp,Env,Env,throw(Lc,ErExp,Tp),Opts,Path) :-
  isThrow(A,Lc,E),!,
  typeOfExp(E,ErTp,voidType,Env,_,ErExp,Opts,Path).
typeOfExp(A,Tp,ErTp,Env,Env,suspend(Lc,T,M,Tp),Opts,Path) :-
  isSuspend(A,Lc,L,R),!,
  newTypeVar("M",MTp),
  fiberType(Tp,MTp,FTp),
  typeOfExp(L,FTp,ErTp,Env,_,T,Opts,Path),
  typeOfExp(R,MTp,ErTp,Env,_,M,Opts,Path).
typeOfExp(A,Tp,ErTp,Env,Env,retire(Lc,T,M,Tp),Opts,Path) :-
  isRetire(A,Lc,L,R),!,
  newTypeVar("T",TTp),
  newTypeVar("M",MTp),
  fiberType(TTp,MTp,FTp),
  typeOfExp(L,FTp,ErTp,Env,_,T,Opts,Path),
  typeOfExp(R,MTp,ErTp,Env,_,M,Opts,Path).
typeOfExp(A,Tp,ErTp,Env,Env,resume(Lc,T,M,Tp),Opts,Path) :-
  isResume(A,Lc,L,R),!,
  newTypeVar("M",MTp),
  fiberType(MTp,Tp,FTp),
  typeOfExp(L,FTp,ErTp,Env,_,T,Opts,Path),
  typeOfExp(R,MTp,ErTp,Env,_,M,Opts,Path).
typeOfExp(Term,Tp,ErTp,Env,Env,Exp,Opts,Path) :-
  isRoundTerm(Term,Lc,F,A),
  typeOfRoundTerm(Lc,F,A,Tp,ErTp,Env,Exp,Opts,Path).
typeOfExp(Term,Tp,_ErTp,Env,Env,void,_,_) :-!,
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
typeOfFieldAcc(Lc,Rc,Fld,Tp,ErTp,Env,Ev,dot(Lc,Rec,Fld,Tp),Opts,Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,ErTp,Env,Ev,Rec,Opts,Path).

% We only do partial type checking at this stage
typeOfRecordUpdate(Lc,Rc,Fld,Vl,Tp,ErTp,Env,Ev,update(Lc,Rec,Fld,Val),Opts,Path) :-
  typeOfExp(Rc,Tp,ErTp,Env,Ev0,Rec,Opts,Path),
  newTypeVar("_V",VT),
  typeOfExp(Vl,VT,ErTp,Ev0,Ev,Val,Opts,Path).

% same: we only partially check tuple index
typeOfTupleAcc(Lc,Rc,Fld,Tp,ErTp,Env,Ev,tdot(Lc,Rec,Fld,Tp),Opts,Path) :-
  newTypeVar("_R",AT),
  typeOfExp(Rc,AT,ErTp,Env,Ev,Rec,Opts,Path).

typeOfRoundTerm(Lc,F,A,Tp,ErTp,Env,Call,Opts,Path) :-
  newTypeVar("F",FnTp),
  genTpVars(A,Vrs),
  At = tplType(Vrs),
  typeOfExp(F,FnTp,ErTp,Env,E0,Fun,Opts,Path),
  (sameType(funType(At,Tp),FnTp,Lc,E0) ->
   typeOfArgTerm(tuple(Lc,"()",A),At,ErTp,E0,_Ev,Args,Opts,Path),
   Call=apply(Lc,Fun,Args,Tp);
   sameType(funType(At,Tp,ETp),FnTp,Lc,E0) ->
     (sameType(ETp,ErTp,Lc,Env) ->
	typeOfArgTerm(tuple(Lc,"()",A),At,ErTp,E0,_Ev,Args,Opts,Path),
	Call=tapply(Lc,Fun,Args,Tp,ErTp);
      reportError("%s throws %s which is not consistent with %s",[Fun,ETp,ErTp],Lc),
      Call=void);
   sameType(consType(At,Tp),FnTp,Lc,E0) ->
   typeOfArgTerm(tuple(Lc,"()",A),At,ErTp,E0,_Ev,Args,Opts,Path),
   Call=capply(Lc,Fun,Args,Tp);
   reportError("type of %s:\n%s\nnot consistent with:\n%s=>%s",[Fun,FnTp,At,Tp],Lc),
   Call=void).

typeOfLambda(Term,Tp,Env,lambda(Lc,Lbl,Cx,rule(Lc,Args,Guard,Exp),Tp),Opts,Path) :-
%  reportMsg("expected type of lambda %s = %s",[Term,Tp]),
  pushScope(Env,LEnv),
  getConstraints(Tp,Cx,LambdaTp),
  declareConstraints(Lc,Cx,LEnv,EvL),
  isEquation(Term,Lc,H,C,R),
  newTypeVar("_A",AT),
  newTypeVar("_E",RT),
  typeOfArgPtn(H,AT,voidType,EvL,E0,Args,Opts,Path),
  checkGuard(C,voidType,E0,E1,Guard,Opts,Path),
  verifyType(Lc,ast(Term),funType(AT,RT),LambdaTp,Env),
  lambdaLbl(Path,"λ",Lbl),
  typeOfExp(R,RT,voidType,E1,_,Exp,Opts,Path).

%% Translate thunks into uses of SAVars & lambda
% $$ E becomes
%
% valof{
%   SV = _SV;
%   valis () => (X^=SV ?? X || SV <- E)
% }

% where _SV is a new single assignment variable and ^= is a pattern match and
% <- is setting the SAV

typeOfThunk(Lc,Term,Tp,Env,
	    valof(Lc,
		  doSeq(Lc,
			doDefn(Lc,SavVar,newSV(Lc,SvTp)),
			doValis(Lc,lambda(Lc,Lbl,[],
					  rule(Lc,tple(Lc,[]),none,
					       cond(Lc, match(Lc,svGet(Lc,XVar,SvTp),SavVar),
						    XVar,
						    svSet(Lc,SavVar,Exp),VlTp)),
					  funType(tplType([]),VlTp)))),Tp),Opts,Path) :-
  newTypeVar("υ",VlTp),
  savType(VlTp,SvTp),
  genNewName(Path,"Σ",SavNm),
  SavVar = v(Lc,SavNm,SvTp),
  genNewName(Path,"σ",XNm),
  XVar = v(Lc,XNm,VlTp),
  lambdaLbl(Path,"λ",Lbl),
  thunkType(VlTp,ThTp),
  pushScope(Env,ThnkEnv),
  verifyType(Lc,ast(Term),ThTp,Tp,ThnkEnv),
  typeOfExp(Term,VlTp,voidType,ThnkEnv,_,Exp,Opts,Path).

checkAction(A,Tp,ErTp,HasVal,Env,Ev,As,Opts,Path) :-
  isBraceTuple(A,_,[S]),!,
  checkAction(S,Tp,ErTp,HasVal,Env,Ev,As,Opts,Path).
checkAction(A,_Tp,_ErTp,HasVal,Env,Env,doNop(Lc),_,_) :-
  isBraceTuple(A,Lc,[]),!,
  validLastAct(A,Lc,HasVal).
checkAction(A,Tp,ErTp,HasVal,Env,Ev,doSeq(Lc,L,R),Opts,Path) :-
  isActionSeq(A,Lc,A1,A2),!,
  checkAction(A1,Tp,ErTp,noVal,Env,E0,L,Opts,Path),
  checkAction(A2,Tp,ErTp,HasVal,E0,Ev,R,Opts,Path).
checkAction(A,Tp,ErTp,HasVal,Env,Ev,Ax,Opts,Path) :-
  isActionSeq(A,_,A1),!,
  checkAction(A1,Tp,ErTp,HasVal,Env,Ev,Ax,Opts,Path).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doLbld(Lc,Lb,Ax),Opts,Path) :-
  isLbldAction(A,Lc,L,AA),!,isIden(L,_,Lb),
  checkAction(AA,Tp,ErTp,HasVal,Env,_,Ax,Opts,Path).
checkAction(A,_,_,_,Env,Env,doBrk(Lc,Lb),_Opts,_Path) :-
  isBreak(A,Lc,L),!,isIden(L,_,Lb).
checkAction(A,Tp,ErTp,_HasVal,Env,Env,doValis(Lc,ValExp),Opts,Path) :-
  isValis(A,Lc,E),!,
  typeOfExp(E,Tp,ErTp,Env,_,ValExp,Opts,Path).
checkAction(A,_Tp,_ErTp,_HasVal,Env,Ev,doExp(Lc,Thrw),Opts,Path) :-
  isRaise(A,Lc,_E),!,
  newTypeVar("C",ErTp),
  typeOfExp(A,ErTp,voidType,Env,Ev,Thrw,Opts,Path).
checkAction(A,_Tp,ErTp,_HasVal,Env,Env,doThrow(Lc,Thrw),Opts,Path) :-
  isThrow(A,Lc,E),!,
  typeOfExp(E,ErTp,voidType,Env,_,Thrw,Opts,Path).
checkAction(A,_Tp,ErTp,HasVal,Env,Ev,doDefn(Lc,v(NLc,Nm,TV),Exp),Opts,Path) :-
  isDefn(A,Lc,L,R),
  isIden(L,NLc,Nm),!,
  newTypeVar("V",TV),
  typeOfExp(R,TV,ErTp,Env,_,Exp,Opts,Path),
  declareVr(NLc,Nm,TV,none,Env,Ev),
  validLastAct(A,Lc,HasVal).
checkAction(A,_Tp,ErTp,HasVal,Env,Ev,doMatch(Lc,Ptn,Exp),Opts,Path) :-
  isDefn(A,Lc,P,E),
  isTuple(P,_,_),!,
  newTypeVar("V",TV),
  typeOfPtn(P,TV,ErTp,Env,Ev,Ptn,Opts,Path),
  typeOfExp(E,TV,ErTp,Env,_,Exp,Opts,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,_Tp,ErTp,HasVal,Env,Ev,doMatch(Lc,Ptn,Exp),Opts,Path) :-
  isMatch(A,Lc,P,E),!,
  reportWarning("Use of .= in actions is deprecated",[],Lc),
  newTypeVar("V",TV),
  typeOfPtn(P,TV,ErTp,Env,Ev,Ptn,Opts,Path),
  typeOfExp(E,TV,ErTp,Env,_,Exp,Opts,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,_Tp,ErTp,HasVal,Env,Ev,Act,Opts,Path) :-
  isAssignment(A,Lc,P,E),!,
  checkAssignment(Lc,P,E,ErTp,Env,Ev,Act,Opts,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doTryCatch(Lc,Body,Trw,Hndlr),Opts,Path) :-
  isTryCatch(A,Lc,B,E,H),!,
  checkTryCatch(Lc,B,E,H,Tp,ErTp,Env,checker:tryAction(HasVal),Body,Trw,Hndlr,Opts,Path).
checkAction(A,Tp,OErTp,HasVal,Env,Env,doTry(Lc,Body,ErTp,Hndlr),Opts,Path) :-
  isTry(A,Lc,B,H),!,
  checkTry(B,H,ErTp,Tp,OErTp,Env,checker:tryAction(HasVal),Body,Hndlr,Opts,Path).
checkAction(A,Tp,ErTp,HasVal,Env,Ev,doIfThenElse(Lc,Tst,Thn,Els),Opts,Path) :-
  isIfThenElse(A,Lc,G,T,E),!,
  checkGoal(G,ErTp,Env,E0,Tst,Opts,Path),
  checkAction(T,Tp,ErTp,HasVal,E0,E1,Thn,Opts,Path),
  checkAction(E,Tp,ErTp,HasVal,Env,E2,Els,Opts,Path),
  mergeDict(E1,E2,Env,Ev).
checkAction(A,Tp,ErTp,_HasVal,Env,Env,doIfThenElse(Lc,Tst,Thn,doNop(Lc)),Opts,Path) :-
  isIfThen(A,Lc,G,T),!,
  checkGoal(G,ErTp,Env,E0,Tst,Opts,Path),
  checkAction(T,Tp,ErTp,noVal,E0,_,Thn,Opts,Path).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doWhile(Lc,Tst,Bdy),Opts,Path) :-
  isWhileDo(A,Lc,G,B),!,
  checkGoal(G,ErTp,Env,E0,Tst,Opts,Path),
  checkAction(B,Tp,ErTp,noVal,E0,_,Bdy,Opts,Path),
  validLastAct(A,Lc,HasVal).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doLet(Lc,Decls,Defs,Ac),Opts,Path) :-
  isLetDef(A,Lc,D,B),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  recordEnv(checker:letExport,Opts,ThPath,Lc,D,faceType([],[]),ThEnv,OEnv,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  checkAction(B,Tp,ErTp,HasVal,OEnv,_,Ac,Opts,ThPath).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doLetRec(Lc,Decls,Defs,Ac),Opts,Path) :-
  isLetRec(A,Lc,D,B),!,
  genNewName(Path,"Γ",ThPath),
  pushScope(Env,ThEnv),
  thetaEnv(checker:letExport,Opts,ThPath,Lc,D,faceType([],[]),ThEnv,OEnv,Defs,ThDecls),
  mergeDecls(ThDecls,Decls),
  checkAction(B,Tp,ErTp,HasVal,OEnv,_,Ac,Opts,ThPath).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doCase(Lc,Bound,Eqns,Tp),Opts,Path) :-
  isCaseExp(A,Lc,Bnd,Cases),
  newTypeVar("B",BVr),
  typeOfExp(Bnd,BVr,ErTp,Env,_,Bound,Opts,Path),
  checkCases(Cases,BVr,Tp,ErTp,Env,Eqns,Eqx,Eqx,[],checker:tryAction(HasVal),Opts,Path),!.
checkAction(A,Tp,ErTp,HasVal,Env,Env,doExp(Lc,suspend(Lc,T,M,TTp)),Opts,Path) :-
  isSuspend(A,Lc,L,R),!,
  newTypeVar("T",TTp),
  newTypeVar("M",MTp),
  fiberType(TTp,MTp,FTp),
  typeOfExp(L,FTp,ErTp,Env,_,T,Opts,Path),
  typeOfExp(R,MTp,ErTp,Env,_,M,Opts,Path),
  checkLastAction(A,Lc,HasVal,Tp,TTp,Env).
checkAction(A,Tp,ErTp,_HasVal,Env,Env,doExp(Lc,retire(Lc,T,M,Tp)),Opts,Path) :-
  isRetire(A,Lc,L,R),!,
  newTypeVar("T",TTp),
  newTypeVar("M",MTp),
  fiberType(TTp,MTp,FTp),
  typeOfExp(L,FTp,ErTp,Env,_,T,Opts,Path),
  typeOfExp(R,MTp,ErTp,Env,_,M,Opts,Path).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doExp(Lc,resume(Lc,T,M,TTp)),Opts,Path) :-
  isResume(A,Lc,L,R),!,
  newTypeVar("T",TTp),
  newTypeVar("M",MTp),
  fiberType(MTp,TTp,FTp),
  typeOfExp(L,FTp,ErTp,Env,_,T,Opts,Path),
  typeOfExp(R,MTp,ErTp,Env,_,M,Opts,Path),
  checkLastAction(A,Lc,HasVal,Tp,TTp,Env).
checkAction(A,Tp,ErTp,HasVal,Env,Env,doExp(Lc,Exp),Opts,Path) :-
  isRoundTerm(A,Lc,F,Args),!,
  newTypeVar("_",RTp),
  typeOfRoundTerm(Lc,F,Args,RTp,ErTp,Env,Exp,Opts,Path),
  checkLastAction(A,Lc,HasVal,Tp,RTp,Env).
checkAction(A,Tp,_ErTp,_HasVal,Env,Env,doNop(Lc),_,_) :-
  locOfAst(A,Lc),
  reportError("%s:%s illegal form of action",[ast(A),tpe(Tp)],Lc).

tryAction(HasVal,A,Tp,ErTp,Env,Ev,Act,Opts,Path) :-
  checkAction(A,Tp,ErTp,HasVal,Env,Ev,Act,Opts,Path).

checkAssignment(Lc,P,E,ErTp,Env,Ev,doDefn(Lc,Ptn,cell(Lc,Exp)),Opts,Path) :-
  isIden(P,Nm),
  \+  getVar(Lc,Nm,Env,_,_),!,
  newTypeVar("V",TV),
  mkRefTp(TV,RTp),
  typeOfPtn(P,RTp,ErTp,Env,Ev,Ptn,Opts,Path),
  typeOfExp(E,TV,ErTp,Env,_,Exp,Opts,Path).
checkAssignment(Lc,P,E,ErTp,Env,Ev,doAssign(Lc,Ptn,Exp),Opts,Path) :-
  newTypeVar("V",TV),
  mkRefTp(TV,RTp),
  typeOfExp(P,RTp,ErTp,Env,Ev,Ptn,Opts,Path),
  typeOfExp(E,TV,ErTp,Env,_,Exp,Opts,Path).

validLastAct(A,Lc,hasVal(_)) :-!,
  reportError("%s not permitted to be last action",[ast(A)],Lc).
validLastAct(_,_,_).

checkLastAction(_,_,noVal,_,_,_) :-!.
checkLastAction(A,Lc,hasVal,ETp,ATp,Env) :-
  verifyType(Lc,ast(A),ATp,ETp,Env).

checkGuard(none,_ErTp,Env,Env,none,_,_) :-!.
checkGuard(some(G),ErTp,Env,Ev,some(Goal),Opts,Path) :-
  checkGoal(G,ErTp,Env,Ev,Goal,Opts,Path).

checkTryCatch(Lc,B,E,Hs,Tp,OErTp,Env,Check,Body,v(Lc,ErNm,ErTp),Hndlr,Opts,Path) :-
  parseType(E,Env,ErTp),
  tryBlockName(Path,ErTp,ErNm),
  declareTryScope(Lc,ErTp,ErNm,Env,Ev2),
  call(Check,B,Tp,ErTp,Ev2,_,Body,Opts,Path),
  checkCases(Hs,ErTp,Tp,OErTp,Env,Hndlr,Eqx,Eqx,[],Check,Opts,Path),!.

checkTry(B,Hs,ErTp,Tp,OErTp,Env,Check,Body,Hndlr,Opts,Path) :-
  newTypeVar("ErTp",ErTp),
  call(Check,B,Tp,ErTp,E1,_,Body,Opts,Path),
  checkCases(Hs,ErTp,Tp,OErTp,Env,Hndlr,Eqx,Eqx,[],Check,Opts,Path),!.

tryBlockName(Path,Tp,TrBlkNm) :-
  tpName(Tp,TpNm),
  mangleName(Path,conTract,TpNm,TrBlkNm).

checkGoal(Term,ErTp,Env,Ex,conj(Lc,Lhs,Rhs),Opts,Path) :-
  isConjunct(Term,Lc,L,R),!,
  checkGoal(L,ErTp,Env,E1,Lhs,Opts,Path),
  checkGoal(R,ErTp,E1,Ex,Rhs,Opts,Path).
checkGoal(Term,ErTp,Env,Ev,disj(Lc,Lhs,Rhs),Opts,Path) :-
  isDisjunct(Term,Lc,L,R),!,
  checkGoal(L,ErTp,Env,E1,Lhs,Opts,Path),
  checkGoal(R,ErTp,Env,E2,Rhs,Opts,Path),
  mergeDict(E1,E2,Env,Ev).
checkGoal(Term,ErTp,Env,Env,implies(Lc,Lhs,Rhs),Opts,Path) :-
  isForall(Term,Lc,L,R),!,
  checkGoal(L,ErTp,Env,E1,Lhs,Opts,Path),
  checkGoal(R,ErTp,E1,_Ex,Rhs,Opts,Path).
checkGoal(Term,ErTp,Env,Env,neg(Lc,Rhs),Opts,Path) :-
  isNegation(Term,Lc,R),!,
  checkGoal(R,ErTp,Env,_Ex,Rhs,Opts,Path).
checkGoal(Term,ErTp,Env,Ev,match(Lc,Lhs,Rhs),Opts,Path) :-
  isMatch(Term,Lc,P,E),!,
  newTypeVar("_#",TV),
  typeOfPtn(P,TV,ErTp,Env,Ev,Lhs,Opts,Path),
  typeOfExp(E,TV,ErTp,Env,_,Rhs,Opts,Path).
checkGoal(Trm,ErTp,Env,Ev,Gl,Opts,Path) :-
  isTuple(Trm,_,[Inner]),
  \+ isTuple(Inner,_), !,
  checkGoal(Inner,ErTp,Env,Ev,Gl,Opts,Path).
checkGoal(G,ErTp,Env,Env,Goal,Opts,Path) :-
  locOfAst(G,Lc),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(G,LogicalTp,ErTp,Env,_Ev,Goal,Opts,Path).

checkCaseExp(_Lc,Bnd,Cases,Tp,ErTp,Env,Env,Checker,Bound,Eqns,Opts,Path) :-
  newTypeVar("_L",LhsTp),
  typeOfExp(Bnd,LhsTp,ErTp,Env,_,Bound,Opts,Path),
%  reportMsg("case governer: %s:%s",[Bound,LhsTp]),
  checkCases(Cases,LhsTp,Tp,ErTp,Env,Eqns,Eqx,Eqx,[],Checker,Opts,Path),!.

checkCases([],_,_,_,_,Eqs,Eqs,Dfx,Dfx,_,_,_).
checkCases([C|Ss],LhsTp,Tp,ErTp,Env,Eqns,Eqx,Df,Dfx,Checker,Opts,Path) :-
  isEquation(C,Lc,L,G,R),!,
  checkCase(Lc,L,G,R,LhsTp,Tp,ErTp,Env,Eqns,Eqs,Df,Df0,Checker,Opts,Path),
  checkCases(Ss,LhsTp,Tp,ErTp,Env,Eqs,Eqx,Df0,Dfx,Checker,Opts,Path).

checkCase(Lc,Lhs,G,R,LhsTp,Tp,ErTp,Env,Eqns,Eqns,Df,Defx,Checker,Opts,Path) :-
  isDefault(Lhs,_,DLhs),!,
  checkCase(Lc,DLhs,G,R,LhsTp,Tp,ErTp,Env,Df,Defx,_,_,Checker,Opts,Path).
checkCase(Lc,H,G,R,LhsTp,Tp,ErTp,Env,
	  [rule(Lc,Arg,Guard,Exp)|Eqns],Eqns,Dfx,Dfx,Checker,Opts,Path) :-
  typeOfPtn(H,LhsTp,ErTp,Env,E1,Arg,Opts,Path),
  checkGuard(G,ErTp,E1,E2,Guard,Opts,Path),
  call(Checker,R,Tp,ErTp,E2,_,Exp,Opts,Path).

genTpVars([],[]).
genTpVars([_|I],[Tp|More]) :-
  newTypeVar("__",Tp),
  genTpVars(I,More).

fieldInFace(faceType(Fields,_),Nm,_,Tp) :-
  is_member((Nm,Tp),Fields),!.
fieldInFace(Tp,Nm,Lc,anonType) :-
  reportError("field %s not declared in %s",[Nm,Tp],Lc).

typeOfExps([],[],_ErTp,Env,Env,_,[],_,_).
typeOfExps([],[T|_],_ErTp,Env,Env,Lc,[],_,_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfExps([A|_],[],_ErTp,Env,Env,_,[],_,_) :-
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfExps([A|As],[ETp|ElTypes],ErTp,Env,Ev,_,[Term|Els],Opts,Path) :-
  evidence(ETp,Env,_Q,ElTp),
  typeOfExp(A,ElTp,ErTp,Env,E0,Term,Opts,Path),
  % reportMsg("type of argument %s |= %s",[A,ETp]),
  % dispEnv(Env),
  locOfAst(A,Lc),
  typeOfExps(As,ElTypes,ErTp,E0,Ev,Lc,Els,Opts,Path).

typeOfPtns([],[],_ErTp,Env,Env,_,[],_,_) :-!.
typeOfPtns([],[T|_],_ErTp,Env,Env,Lc,[],_,_) :-
  reportError("insufficient arguments, expecting a %s",[T],Lc).
typeOfPtns([A|_],[],_ErTp,Env,Env,_,[],_,_) :-!,
  locOfAst(A,Lc),
  reportError("too many arguments: %s",[A],Lc).
typeOfPtns([A|As],[ETp|ElTypes],ErTp,Env,Ev,_,[Term|Els],Opts,Path) :-
  deRef(ETp,ElTp),
  typeOfPtn(A,ElTp,ErTp,Env,E0,Term,Opts,Path),
  locOfAst(A,Lc),
  typeOfPtns(As,ElTypes,ErTp,E0,Ev,Lc,Els,Opts,Path).

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

exportAcc(Tp,Export) :-
  tpName(Tp,TpNm),
  (marker(type,TpMrkr),
   splitLocalName(TpNm,TpMrkr,_,Nm),
   call(Export,tpe(Nm));
   marker(conTract,CnMrkr),
   splitLocalName(TpNm,CnMrkr,_,Nm),
   call(Export,con(Nm))),!.


