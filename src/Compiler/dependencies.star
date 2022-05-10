star.compiler.dependencies{
  import star.
  import star.topsort.
  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.operators.
  import star.compiler.meta.
  import star.compiler.wff.
  import star.compiler.misc.

  public dependencies:(cons[ast],reports) =>
    result[reports,
      (cons[(defnSp,visibility)],cons[ast],map[string,ast],cons[cons[defnSpec]])].
  dependencies(Dfs,Rp) => do{
    if traceDependencies! then
      logMsg("look for dependencies in $(Dfs)");
    (Defs,Pb,As,Opn) <- collectDefinitions(Dfs,Rp);
--    logMsg("definitions found: $(Defs)");
    AllRefs .= foldLeft((D,M)=>collectRef(D,M),[],Defs);
    InitDefs <- collectThetaRefs(Defs,AllRefs,As,[],Rp);
    Groups .= (topsort(InitDefs) // (Gp)=>(Gp//((definition(Sp,Lc,_,Els))=>defnSpec(Sp,Lc,Els))));
    if traceDependencies! then
      logMsg("groups $(Groups)");
    valis (Pb,Opn,As,Groups)
  }

  private collectRef:(defnSpec,map[defnSp,defnSp])=>map[defnSp,defnSp].
  collectRef(defnSpec(conSp(Nm),_,[St]),M) where (_,_,_,_,Els) ^= isCntrctStmt(St) =>
    foldLeft((El,MM) => ((_,N,_) ^= isTypeAnnotation(El) && (_,Id)^=isName(N) ?
	  MM[varSp(Id)->conSp(Nm)] || MM),
      M[conSp(Nm)->conSp(Nm)],Els).
  collectRef(defnSpec(N,_,_),M) => M[N->N].

  public recordDefs:(cons[ast],reports) =>
    result[reports,
      (cons[(defnSp,visibility)],cons[ast],map[string,ast],cons[defnSpec])].
  recordDefs(Dfs,Rp) => do{
    (Defs,Pb,As,Opn) <- collectDefinitions(Dfs,Rp);
    valis (Pb,Opn,As,Defs)
  }

  definitionSpec ::= definition(defnSp,option[locn],cons[defnSp],cons[ast]).

  implementation depends[definitionSpec->>defnSp] => {
    references(definition(_,_,Refs,_)) => Refs.
    defined(definition(Sp,_,_,_),Rf) => Sp==Rf.
  }

  implementation display[definitionSpec] => {
    disp(definition(Sp,Lc,Refs,_)) => "$(Sp)->$(Refs)".
  }

  collectDefinitions:(cons[ast],
    reports) => result[reports,(cons[defnSpec],cons[(defnSp,visibility)],
      map[string,ast],cons[ast])].
  collectDefinitions(Stmts,Rp) => collectDefs(Stmts,[],[],{},[],Rp).

  collectDefs:(cons[ast],cons[defnSpec],cons[(defnSp,visibility)],map[string,ast],cons[ast],reports) => result[reports,(cons[defnSpec],cons[(defnSp,visibility)],
      map[string,ast],cons[ast])].
  
  collectDefs([],Defs,Pb,As,Opn,Rp) => do{ valis (Defs,Pb,As,Opn) }.
  collectDefs([A,..Ss],Defs,Pb,As,Opn,Rp) where _ ^= isAnnotation(A) =>
    collectDefs(Ss,Defs,Pb,As,Opn,Rp).
  collectDefs([A,..Ss],Defs,Pb,As,Opn,Rp) => do{
    (SS1,Dfs1,Pb1,As1,Opn1) <- collectDefinition(A,Ss,Defs,Pb,As,Opn,.deFault,Rp);
    collectDefs(SS1,Dfs1,Pb1,As1,Opn1,Rp)
  }
    
  collectDefinition:(ast,
    cons[ast],
    cons[defnSpec],
    cons[(defnSp,visibility)],
    map[string,ast],
    cons[ast],
    visibility,
    reports) => result[reports,(cons[ast],cons[defnSpec],
      cons[(defnSp,visibility)],map[string,ast],cons[ast])].

  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      (_,Ai) ^= isPublic(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,.pUblic,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      (_,Ai) ^= isPrivate(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,.priVate,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      Spec ^= isOpen(A) => do{ valis (Stmts,Defs,Pb,As,[A,..Opn]) }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,V,T) ^= isTypeAnnotation(A) => do{
	if(ILc,Id) ^= isName(V) then{
	  if isConstructorStmt(T) then {
	    valis (Stmts,[defnSpec(cnsSp(Id),Lc,[T]),..Defs],
	      [(cnsSp(Id),Vz),..Pb],As[Id->T],Opn)
	  }
	  else
	  valis (Stmts,Defs,[(varSp(Id),Vz),..Pb],As[Id->T],Opn)
	}
	else
	raise reportError(Rp,"expecting an identifier, not $(V)",locOf(V))
      }
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Q,C,T,Els) ^= isCntrctStmt(A) => do{
	valis (Stmts,[defnSpec(conSp(typeName(T)),Lc,[A]),..Defs],
	  [(conSp(typeName(T)),Vz),..Pb],generateAnnotations(Els,Q,C,As),Opn)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,Cn,_) ^= isImplementationStmt(A) &&
      Sp .= implSp(implementedContractName(Cn)) =>
    do{ valis (Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn) }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,L,R) ^= isTypeExistsStmt(A) && Sp .= tpSp(typeName(L)) =>
    do{ valis (Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn)}.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,L,R) ^= isTypeFunStmt(A) && Sp .= tpSp(typeName(L)) =>
    do{ valis (Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn) }.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm,Rhs) ^= isDefn(A) && (_,Id) ^= isName(Nm) => do{
	Sp .= varSp(Id);
	valis (Ss,[defnSpec(Sp,Lc,[A]),..Defs],publishName(Sp,Vz,Pb),As,Opn)
	}.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,Tp,_) ^= isAccessorStmt(A) &&
      Sp .= accSp(typeName(Tp)) =>
    do{ valis (Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn) }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,Tp,_) ^= isUpdaterStmt(A) &&
      Sp .= updSp(typeName(Tp)) =>
    do{ valis (Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn) }.
    
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm) ^= ruleName(A) => do{
	if DLc ^= isDefined(varSp(Nm),Defs) then
	  raise reportError(Rp,"$(Nm) already defined at $(DLc)",Lc)
	else{
	  (Ss,Dfs) .= collectDefines(Stmts,Nm,[A]);
	  Sp .= varSp(Nm);
	  valis (Ss,[defnSpec(Sp,Lc,reverse(Dfs)),..Defs],publishName(Sp,Vz,Pb),As,Opn)
	}
      }.
  collectDefinition(A,_,_,_,_,_,_,Rp) => do{
    raise reportError(Rp,"cannot understand definition $(A)",locOf(A))
  }.

  isDefined:(defnSp,cons[defnSpec])=>option[locn].
  isDefined(_,[]) => .none.
  isDefined(Sp,[defnSpec(Sp,Lc,Sts),..Defs]) => Lc.
  isDefined(Sp,[_,..Defs]) => isDefined(Sp,Defs).

  publishName:(defnSp,visibility,cons[(defnSp,visibility)])=>
    cons[(defnSp,visibility)].
  publishName(Nm,_,Pb) where {? (Nm,_) in Pb ?} => Pb.
  publishName(Nm,Vz,Pb) => [(Nm,Vz),..Pb].

  collectDefines:(cons[ast],string,cons[ast]) => (cons[ast],cons[ast]).
  collectDefines([St,..Ss],Nm,Dfs) where
      (_,Nm) ^= ruleName(St) => collectDefines(Ss,Nm,[St,..Dfs]).
  collectDefines(Ss,Nm,Dfs) default => (Ss,Dfs).
	
  generateAnnotations:(cons[ast],cons[ast],cons[ast],map[string,ast]) =>
    map[string,ast].
  generateAnnotations([],_,_,As) => As.
  generateAnnotations([A,..Ss],Qs,Cs,As) where
      (Lc,V,T) ^= isTypeAnnotation(A) && (_,Id) ^= isName(V) =>
    generateAnnotations(Ss,Qs,Cs,As[Id->reUQuant(Lc,Qs,reConstrain(Cs,T))]).
  generateAnnotations([A,..Ss],Qs,Cs,As) =>
    generateAnnotations(Ss,Qs,Cs,As).

  collectThetaRefs:(cons[defnSpec],map[defnSp,defnSp],map[string,ast],
    cons[definitionSpec],reports) =>
    result[reports,cons[definitionSpec]].
  collectThetaRefs([],_,_,DSpecs,_) => do{ valis DSpecs }.
  collectThetaRefs([defnSpec(cnsSp(Nm),Lc,[Def]),..Defs],AllRefs,Annots,S,Rp) => do{
    Refs <- collectTypeRefs(Def,AllRefs,[],Rp);
    collectThetaRefs(Defs,AllRefs,Annots,[definition(cnsSp(Nm),Lc,Refs,[Def]),..S],Rp)
  }
  collectThetaRefs([defnSpec(Defines,Lc,Stmts),..Defs],AllRefs,Annots,S,Rp) => do{
    Refs <- collectStmtsRefs(Stmts,AllRefs,Annots,[],Rp);
    collectThetaRefs(Defs,AllRefs,Annots,[definition(Defines,Lc,Refs,Stmts),..S],Rp)
  }.

  collectEnvRefs:(cons[ast],map[defnSp,defnSp],map[string,ast],cons[defnSp],reports) =>
    result[reports,cons[defnSp]].
  collectEnvRefs(Defs,All,Annots,Rf,Rp) =>
    collectStmtsRefs(Defs,locallyDefined(Defs,All),Annots,Rf,Rp).

  locallyDefined:(cons[ast],map[defnSp,defnSp]) => map[defnSp,defnSp].
  locallyDefined([],All) => All.
  locallyDefined([St,..Stmts],All) =>
    locallyDefined(Stmts,removeLocalDef(St,All)).

  removeLocalDef(St,All) where (_,Id) ^= ruleName(St) =>
    All[~varSp(Id)].

  collectStmtsRefs([],_,_,Rf,_) => do{ valis Rf}.
  collectStmtsRefs([St,..Sts],All,Annots,Rf,Rp) => do{
    Rf0 <- collectStmtRefs(St,All,Annots,Rf,Rp);
    collectStmtsRefs(Sts,All,Annots,Rf0,Rp)
  }

  collectStmtRefs:(ast,map[defnSp,defnSp],map[string,ast],cons[defnSp],reports) =>
    result[reports,cons[defnSp]].
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,_,Tp) ^= isTypeAnnotation(A) =>
    collectTypeRefs(Tp,All,Rf,Rp).
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,I) ^= isPublic(A) =>
    collectStmtRefs(I,All,Annots,Rf,Rp).
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,I) ^= isPrivate(A) =>
    collectStmtRefs(I,All,Annots,Rf,Rp).
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,V,D) ^= isDefn(A) => do{
    Rf0 <- collectAnnotRefs(V,All,Annots,Rf,Rp);
    collectTermRefs(D,All,Rf0,Rp)
  }
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,V,D) ^= isAssignment(A) => do{
    Rf0 <- collectAnnotRefs(V,All,Annots,Rf,Rp);
    collectTermRefs(D,All,Rf0,Rp)
  }
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,some(Nm),_,H,C,R) ^= isEquation(A) => do{
    Rf0 <- collectAnnotRefs(Nm,All,Annots,Rf,Rp);
    Rf1 <- collectHeadRefs(H,C,All,Rf0,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectStmtRefs(A,All,Annots,Rf,Rp) where isConstructorStmt(A) =>
    collectTypeRefs(A,All,Rf,Rp).
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,L,R) ^= isTypeExistsStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(L,A0,Rf0,Rp);
	collectTypeRefs(R,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,L,R) ^= isTypeFunStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(L,A0,Rf0,Rp);
	collectTypeRefs(R,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,C,T,Els) ^= isCntrctStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf0 <- collectConstraintRefs(C,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(T,A0,Rf0,Rp);
	collectFaceTypes(Els,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,Tp,Exp) ^= isImplementationStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf1 <- collectConstraintRefs([Tp,..Cx],A0,Rf,Rp);
	collectTermRefs(Exp,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,Tp,Exp) ^= isAccessorStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(Tp,A0,Rf0,Rp);
	collectTermRefs(Exp,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,Tp,Exp) ^= isUpdaterStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(Tp,A0,Rf0,Rp);
	collectTermRefs(Exp,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) => do{
    raise reportError(Rp,"cannot fathom definition $(A)",locOf(A))
  }.

  collectHeadRefs(H,some(C),All,Rf,Rp) => do{
    Rf0 <- collectTermRefs(H,All,Rf,Rp);
    collectCondRefs(C,All,Rf0,Rp)
  }
  collectHeadRefs(H,.none,All,Rf,Rp) =>
    collectTermRefs(H,All,Rf,Rp).
    
  collectAnnotRefs(H,All,Annots,Rf,Rp) where Id^=headName(H) =>
    (Tp ^= Annots[Id] ?
      collectTypeRefs(Tp,All,Rf,Rp) ||
      do{ valis Rf}).
  collectAnnotRefs(H,_,_,_,Rp) => do{
    raise reportError(Rp,"not a head: $(H)",locOf(H))
  }.

  collectCondRefs:(ast,map[defnSp,defnSp],cons[defnSp],reports) => result[reports,cons[defnSp]].
  collectCondRefs(A,All,Rf,Rp) where (_,L,R) ^= isConjunct(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectCondRefs(A,All,Rf,Rp) where (_,L,R) ^= isDisjunct(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectCondRefs(A,All,Rf,Rp) where (_,L,R) ^= isImplies(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectCondRefs(A,All,Rf,Rp) where (_,R) ^= isNegation(A) => 
    collectCondRefs(R,All,Rf,Rp).
  collectCondRefs(A,All,Rf,Rp) where (_,T,L,R) ^= isConditional(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    Rf1 <- collectCondRefs(L,All,Rf0,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectCondRefs(A,All,Rf,Rp) where (_,[C]) ^= isTuple(A) => 
    collectCondRefs(C,All,Rf,Rp).
  collectCondRefs(E,All,Rf,Rp) => collectTermRefs(E,All,Rf,Rp).
    
  collectTermRefs:(ast,map[defnSp,defnSp],cons[defnSp],reports) => result[reports,cons[defnSp]].
  collectTermRefs(V,All,Rf,Rp) where (_,Id) ^= isName(V) => do{
    valis collectName(cnsSp(Id),All,collectName(varSp(Id),All,Rf))
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Id) ^= isEnumSymb(T) =>
    do { valis collectName(cnsSp(Id),All,Rf) }.
  collectTermRefs(T,All,Rf,Rp) where (_,Lhs,Rhs) ^= isTypeAnnotation(T) => do{
    Rf1 <- collectTermRefs(Lhs,All,Rf,Rp);
    collectTypeRefs(Rhs,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Env,Bnd) ^= isLetDef(T) => do{
    Rf1 <- collectTermRefs(Bnd,All,Rf,Rp);
    collectStmtsRefs(Env,All,[],Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Env,Bnd) ^= isLetRecDef(T) => do{
    Rf1 <- collectTermRefs(Bnd,All,Rf,Rp);
    collectStmtsRefs(Env,All,[],Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,I) ^= isCellRef(T) =>
    collectTermRefs(I,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,I) ^= isRef(T) =>
    collectTermRefs(I,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,Op,Args) ^= isRoundTerm(T) => do{
    Rf1 <- collectTermRefs(Op,All,Rf,Rp);
    collectTermListRefs(Args,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Args) ^= isTuple(T) => 
    collectTermListRefs(Args,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isCons(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Args) ^= isSqTuple(T) => 
    collectTermListRefs(Args,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,E,C) ^= isCase(T) => do{
    Rf0 <- collectTermRefs(E,All,Rf,Rp);
    collectCasesRefs(C,All,Rf0,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Sts) ^= isTheta(T) =>
    collectStmtsRefs(Sts,All,[],Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,Sts) ^= isQTheta(T) =>
    collectStmtsRefs(Sts,All,[],Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isMatch(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isOptionMatch(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isSearch(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,M,R) ^= isSlice(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    Rf2 <- collectTermRefs(L,All,Rf1,Rp);
    collectTermRefs(R,All,Rf2,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isCoerce(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTypeRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(A,All,Rf,Rp) where (_,L,R) ^= isConjunct(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(A,All,Rf,Rp) where (_,L,R) ^= isDisjunct(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(A,All,Rf,Rp) where (_,L,R) ^= isImplies(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(A,All,Rf,Rp) where (_,R) ^= isNegation(A) => 
    collectCondRefs(R,All,Rf,Rp).
  collectTermRefs(A,All,Rf,Rp) where (_,T,L,R) ^= isConditional(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    Rf1 <- collectTermRefs(L,All,Rf0,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,_,L,C,R) ^= isLambda(T) => do{
    Rf1 <- collectHeadRefs(L,C,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isWhere(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isOptionPtn(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isDoTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isResultTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isActionTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isTaskTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isValof(T) =>
    ((_,[As]) ^= isBrTuple(L) ?
      collectDoRefs(As,All,Rf,Rp) ||
      collectTermRefs(L,All,Rf,Rp)).
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isAbstraction(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,R,_,V) ^= isRecordUpdate(T) => do{
    Rf1 <- collectTermRefs(R,All,Rf,Rp);
    collectTermRefs(V,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isComma(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,_) ^= isFieldAcc(T) => 
    collectTermRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L,Stmts) ^= isLabeledTheta(T) => do{
    Rf1 <- collectStmtsRefs(Stmts,All,[],Rf,Rp);
    collectTermRefs(L,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,Stmts) ^= isLabeledRecord(T) => do{
    Rf1 <- collectStmtsRefs(Stmts,All,[],Rf,Rp);
    collectTermRefs(L,All,Rf1,Rp)
  }
  collectTermRefs(int(_,_),_,Rf,_) => do{ valis Rf}.
  collectTermRefs(str(_,_),_,Rf,_) => do{ valis Rf}.
  collectTermRefs(num(_,_),_,Rf,_) => do{ valis Rf}.
  collectTermRefs(T,_,_,Rp) => do{
    raise reportError(Rp,"cant parse $(T) for references",locOf(T))
  }.

  collectDoRefs:(ast,map[defnSp,defnSp],cons[defnSp],reports) => result[reports,cons[defnSp]].
  collectDoRefs(A,All,Rf,Rp) where (_,L,R) ^= isActionSeq(A) => do{
    Rf1 <- collectDoRefs(L,All,Rf,Rp);
    collectDoRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,L,R) ^= isBind(A) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,L,R) ^= isMatch(A) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,L,R) ^= isOptionMatch(A) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,R) ^= isValis(A) => 
    collectTermRefs(R,All,Rf,Rp).
  collectDoRefs(A,All,Rf,Rp) where (_,R) ^= isThrow(A) => 
    collectTermRefs(R,All,Rf,Rp).
  collectDoRefs(A,All,Rf,Rp) where (_,L,R) ^= isTryCatch(A) => do{
    Rf1 <- collectDoRefs(L,All,Rf,Rp);
    collectCatchRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L,R) ^= isIfThenElse(A) => do{
    Rf0 <- collectTermRefs(T,All,Rf,Rp);
    Rf1 <- collectDoRefs(L,All,Rf0,Rp);
    collectDoRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L) ^= isIfThen(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    collectDoRefs(L,All,Rf0,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L) ^= isWhileDo(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    collectDoRefs(L,All,Rf0,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L) ^= isForDo(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    collectDoRefs(L,All,Rf0,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,[S]) ^= isBrTuple(A) =>
    collectDoRefs(S,All,Rf,Rp).
  collectDoRefs(A,All,Rf,Rp) => collectTermRefs(A,All,Rf,Rp).

  collectCatchRefs(A,All,Rf,Rp) where (_,[St]) ^= isBrTuple(A) =>
    collectDoRefs(St,All,Rf,Rp).
  collectCatchRefs(A,All,Rf,Rp) => collectTermRefs(A,All,Rf,Rp).
  
  collectCasesRefs([],_,Rf,_) => do{ valis Rf }.
  collectCasesRefs([St,..Sts],All,Rf,Rp) => do{
    Rf0 <- collectCaseRefs(St,All,Rf,Rp);
    collectCasesRefs(Sts,All,Rf0,Rp)
  }
  collectCaseRefs(Cse,All,Rf,Rp) where (_,_,A,C,Rhs) ^= isLambda(Cse) => do{
    Rf1 <- collectHeadRefs(A,C,All,Rf,Rp);
    collectTermRefs(Rhs,All,Rf1,Rp)
  }
  collectCaseRefs(Cse,_,_,Rp) => do{
    raise reportError(Rp,"invalid case in case expression $(Cse)",locOf(Cse))
  }.
    
  collectTermListRefs:(cons[ast],map[defnSp,defnSp],cons[defnSp],reports) =>
    result[reports,cons[defnSp]].
  collectTermListRefs([],_,R,_) => do{ valis R }.
  collectTermListRefs([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTermRefs(T,All,Rf,Rp);
    collectTermListRefs(Ts,All,R1,Rp)
  }

  collectTypeRefs:(ast,map[defnSp,defnSp],cons[defnSp],reports) => result[reports,cons[defnSp]].
  collectTypeRefs(V,All,SoFar,Rp) where (_,Id) ^= isName(V) =>
    do{ valis collectName(tpSp(Id),All,SoFar) }.
  collectTypeRefs(T,All,SoFar,Rp) where (_,Op,Els) ^= isSquareTerm(T) => do{
    R1 <- collectTypeRefs(Op,All,SoFar,Rp);
    collectTypeList(Els,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isConstructorType(T) => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isFunctionType(T) => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,"->>") => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isComma(T) => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,R) ^= isUnary(T,"ref") => 
    collectTypeRefs(R,All,SoFar,Rp).
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,"~>") => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,"<~") => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,"|") => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,Q,I) ^= isQuantified(T) =>
    collectTypeRefs(I,filterOut(All,Q),SoFar,Rp).
  collectTypeRefs(T,All,SoFar,Rp) where (_,Q,I) ^= isXQuantified(T) =>
    collectTypeRefs(I,filterOut(All,Q),SoFar,Rp).
  collectTypeRefs(T,All,Rf,Rp) where (_,Cx,Tp) ^= isConstrained(T) => do{
    Rf0 <- collectTypeRefs(Tp,All,Rf,Rp);
    collectConstraintRefs(Cx,All,Rf0,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,Els) ^= isTuple(T) =>
    collectTypeList(Els,All,SoFar,Rp).
  collectTypeRefs(T,All,SoFar,Rp) where (_,Els) ^= isBrTuple(T) =>
    collectFaceTypes(Els,All,SoFar,Rp).
  collectTypeRefs(T,All,SoFar,Rp) where (_,Op,Els) ^= isBrTerm(T) => do{
    R1 <- collectTermRefs(Op,All,SoFar,Rp);
    collectFaceTypes(Els,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,Rc,_) ^= isFieldAcc(T) => 
    collectTermRefs(Rc,All,SoFar,Rp).
  collectTypeRefs(T,All,SoFar,Rp) where (_,Op,Els) ^= isRoundTerm(T) => do{
    R1 <- collectTypeRefs(Op,All,SoFar,Rp);
    collectTypeList(Els,All,R1,Rp)
  }
  collectTypeRefs(T,_,_,Rp) default => do{
    raise reportError(Rp,"cannot fathom type $(T)",locOf(T))
  }.
  
  collectTypeList:(cons[ast],map[defnSp,defnSp],cons[defnSp],reports) => result[reports,cons[defnSp]].
  collectTypeList([],_,R,_) => do{ valis R }.
  collectTypeList([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTypeRefs(T,All,Rf,Rp);
    collectTypeList(Ts,All,R1,Rp)
  }

  collectConstraintRefs:(cons[ast],map[defnSp,defnSp],cons[defnSp],reports) =>
    result[reports,cons[defnSp]].
  collectConstraintRefs([],_,R,_) => do{ valis R}.
  collectConstraintRefs([T,..Ts],All,Rf,Rp) where _ ^= isSquareTerm(T) => do {
    R1 <- collectContractRefs(T,All,Rf,Rp);
    collectConstraintRefs(Ts,All,R1,Rp)
  }
  collectConstraintRefs([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTypeRefs(T,All,Rf,Rp);
    collectConstraintRefs(Ts,All,R1,Rp)
  }

  collectContractRefs:(ast,map[defnSp,defnSp],cons[defnSp],reports) =>
    result[reports,cons[defnSp]].
  collectContractRefs(T,All,Rf,Rp) where (_,Op,Args) ^= isSquareTerm(T) => do {
    (_,Id) ^= isName(Op);
    R0 .= collectName(conSp(Id),All,Rf);
    collectTypeList(Args,All,R0,Rp)
  }
  collectContractRefs(T,All,Rf,Rp) where (_,Q,I) ^= isQuantified(T) =>
    collectContractRefs(I,filterOut(All,Q),Rf,Rp).
  collectContractRefs(T,All,Rf,Rp) where (_,Q,I) ^= isXQuantified(T) =>
    collectContractRefs(I,filterOut(All,Q),Rf,Rp).

  collectFaceTypes([],_,R,_) => do{ valis R}.
  collectFaceTypes([D,..Ds],All,R,Rp) => do{
    R1 <- collectFaceType(D,All,R,Rp);
    collectFaceTypes(Ds,All,R1,Rp)
  }

  collectFaceType(P,All,R,Rp) where (_,I) ^= isUnary(P,"type") =>
    collectFaceType(I,All,R,Rp).
  collectFaceType(P,All,R,Rp) where (_,L,T) ^= isTypeAnnotation(P) =>
    collectTypeRefs(T,All,R,Rp).
  collectFaceType(_,All,R,_) => do{ valis R}.

  collectName:(defnSp,map[defnSp,defnSp],cons[defnSp])=>cons[defnSp].
  collectName(Sp,All,SoFar) where Rf^=All[Sp] && {? ~Sp in SoFar ?} => [Sp,..SoFar].
  collectName(_,_,SoFar) default => SoFar.

  filterOut:(map[defnSp,defnSp],cons[ast]) => map[defnSp,defnSp].
  filterOut(M,Q) => let{.
    qName(V) where (_,Id) ^= isName(V) => some(Id).
    qName(V) where (_,L,_) ^= isBinary(V,"/") => qName(L).
    qName(_) default => .none.
  .} in foldLeft((V,MM) where Id^=qName(V) => MM[~varSp(Id)],M,Q).
}
