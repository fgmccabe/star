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
    either[reports,
      (cons[(defnSp,visibility)],cons[ast],cons[(string,ast)],cons[cons[defnSpec]])].
  dependencies(Dfs,Rp) => do{
    (Defs,Pb,As,Opn) <- collectDefinitions(Dfs,Rp);
    AllRefs .= (Defs//((defnSpec(Nm,_,_))=>Nm));
    InitDefs <- collectThetaRefs(Defs,AllRefs,As,[],Rp);

    Groups .= (topsort(InitDefs) // ((Gp)=>(Gp//((definition(Sp,Lc,_,Els))=>defnSpec(Sp,Lc,Els)))));
    
    valis (Pb,Opn,As,Groups)
  }

  public recordDefs:(cons[ast],reports) =>
    either[reports,
      (cons[(defnSp,visibility)],cons[ast],cons[(string,ast)],cons[defnSpec])].
  recordDefs(Dfs,Rp) => do{
    (Defs,Pb,As,Opn) <- collectDefinitions(Dfs,Rp);
    valis (Pb,Opn,As,Defs)
  }

  definitionSpec ::= definition(defnSp,locn,cons[defnSp],cons[ast]).

  implementation depends[definitionSpec->>defnSp] => {
    references(definition(_,_,Refs,_)) => Refs.
    defined(definition(Sp,_,_,_),Rf) => Sp==Rf.
  }

  implementation display[definitionSpec] => {.
    disp(definition(Sp,Lc,Refs,_)) => ssSeq([disp(Sp),ss("->"),disp(Refs)]).
  .}

  collectDefinitions:(cons[ast],
    reports) => either[reports,(cons[defnSpec],cons[(defnSp,visibility)],
      cons[(string,ast)],cons[ast])].
  collectDefinitions(Stmts,Rp) => collectDefs(Stmts,[],[],[],[],Rp).

  collectDefs:(cons[ast],cons[defnSpec],cons[(defnSp,visibility)],cons[(string,ast)],cons[ast],reports) => either[reports,(cons[defnSpec],cons[(defnSp,visibility)],
      cons[(string,ast)],cons[ast])].
  
  collectDefs([],Defs,Pb,As,Opn,Rp) => either((Defs,Pb,As,Opn)).
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
    cons[(string,ast)],
    cons[ast],
    visibility,
    reports) => either[reports,(cons[ast],cons[defnSpec],
      cons[(defnSp,visibility)],cons[(string,ast)],cons[ast])].

  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      (_,Ai) ^= isPublic(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,.pUblic,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      (_,Ai) ^= isPrivate(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,.priVate,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      Spec ^= isOpen(A) => either((Stmts,Defs,Pb,As,[A,..Opn])).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,V,T) ^= isTypeAnnotation(A) => do{
	-- special handling for private; because its priority is low
	if (_,Vr) ^= isPrivate(V) then{
	  collectDefinition(typeAnnotation(Lc,Vr,T),Stmts,Defs,Pb,As,Opn,.priVate,Rp)
	} else if(ILc,Id) ^= isName(V) then{
	  if _ ^= isConstructorType(T) then {
	    valis (Stmts,[defnSpec(cnsSp(Id),Lc,[T]),..Defs],
	      [(cnsSp(Id),Vz),..Pb],[(Id,T),..As],Opn)
	  }
	  else
	  valis (Stmts,Defs,[(varSp(Id),Vz),..Pb],[(Id,T),..As],Opn)
	}
	else
	throw reportError(Rp,"expecting an identifier, not $(V)",locOf(V))
      }
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,S,Els) ^= isContractStmt(A) &&
      (_,Nm,Qs,Cs,T) ^= isContractSpec(S)  =>
    either((Stmts,[defnSpec(conSp(Nm),Lc,[A]),..Defs],
	[(conSp(Nm),Vz),..Pb],
	generateAnnotations(Qs,Els,Cs,As),
	Opn)).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,Cn,_) ^= isImplementationStmt(A) &&
      Sp .= implSp(implementedContractName(Cn)) =>
    either((Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn)).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,L,R) ^= isTypeExistsStmt(A) && Sp .= tpSp(typeName(L)) =>
    either((Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn)).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,_,_,L,R) ^= isTypeFunStmt(A) && Sp .= tpSp(typeName(L)) =>
    either((Stmts,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn)).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Q,Cx,H,R) ^= isAlgebraicTypeStmt(A) => do{
	(Dfs1,Pb1,As1) <- reformAlgebraic(Lc,Q,Cx,H,R,Defs,Pb,As,Vz,Rp);
	valis (Stmts,Dfs1,Pb1,As1,Opn)
      }.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm,Rhs) ^= isDefn(A) && (_,Id) ^= isName(Nm) => do{
	Sp .= varSp(Id);
	valis (Ss,[defnSpec(Sp,Lc,[A]),..Defs],publishName(Sp,Vz,Pb),As,Opn)
	}.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm,Rhs) ^= isAssignment(A) && (LLc,Id) ^= isName(Nm) => do{
	Sp .= varSp(Id); -- map X:=E to X=!!E
	valis (Ss,[defnSpec(Sp,Lc,[binary(Lc,"=",Nm,unary(Lc,"!!",Rhs))]),..Defs],
	  publishName(Sp,Vz,Pb),As,Opn)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm) ^= ruleName(A) => do{
	(Ss,Dfs) .= collectDefines(Stmts,Nm,[A]);
	Sp .= varSp(Nm);
	valis (Ss,[defnSpec(Sp,Lc,reverse(Dfs)),..Defs],publishName(Sp,Vz,Pb),As,Opn)
      }.
  collectDefinition(A,_,_,_,_,_,_,Rp) =>
    other(reportError(Rp,"cannot understand definition $(A)",locOf(A))).

  publishName:(defnSp,visibility,cons[(defnSp,visibility)])=>
    cons[(defnSp,visibility)].
  publishName(Nm,_,Pb) where (Nm,_) in Pb => Pb.
  publishName(Nm,Vz,Pb) => [(Nm,Vz),..Pb].

  collectDefines:(cons[ast],string,cons[ast]) => (cons[ast],cons[ast]).
  collectDefines([St,..Ss],Nm,Dfs) where
      (_,Nm) ^= ruleName(St) => collectDefines(Ss,Nm,[St,..Dfs]).
  collectDefines(Ss,Nm,Dfs) default => (Ss,Dfs).
	
  generateAnnotations:(cons[ast],cons[ast],cons[ast],cons[(string,ast)]) =>
    cons[(string,ast)].
  generateAnnotations([],_,_,As) => As.
  generateAnnotations([A,..Ss],Qs,Cs,As) where
      (_,V,T) ^= isTypeAnnotation(A) && (_,Id) ^= isName(V) =>
    generateAnnotations(Ss,Qs,Cs,[(Id,reUQuant(Qs,reConstrain(Cs,T))),..As]).
  generateAnnotations([A,..Ss],Qs,Cs,As) =>
    generateAnnotations(Ss,Qs,Cs,As).

  collectThetaRefs:(cons[defnSpec],cons[defnSp],cons[(string,ast)],
    cons[definitionSpec],reports) =>
    either[reports,cons[definitionSpec]].
  collectThetaRefs([],_,_,DSpecs,_) => either(DSpecs).
  collectThetaRefs([defnSpec(cnsSp(Nm),Lc,[Def]),..Defs],AllRefs,Annots,S,Rp) => do{
    Refs <- collectTypeRefs(Def,AllRefs,[],Rp);
    collectThetaRefs(Defs,AllRefs,Annots,[definition(cnsSp(Nm),Lc,Refs,[Def]),..S],Rp)
  }
  collectThetaRefs([defnSpec(Defines,Lc,Stmts),..Defs],AllRefs,Annots,S,Rp) => do{
    Refs <- collectStmtsRefs(Stmts,AllRefs,Annots,[],Rp);
    collectThetaRefs(Defs,AllRefs,Annots,[definition(Defines,Lc,Refs,Stmts),..S],Rp)
  }.

  collectEnvRefs:(cons[ast],cons[defnSp],cons[(string,ast)],cons[defnSp],reports) =>
    either[reports,cons[defnSp]].
  collectEnvRefs(Defs,All,Annots,Rf,Rp) =>
    collectStmtsRefs(Defs,locallyDefined(Defs,All),Annots,Rf,Rp).

  locallyDefined:(cons[ast],cons[defnSp]) => cons[defnSp].
  locallyDefined([],All) => All.
  locallyDefined([St,..Stmts],All) =>
    locallyDefined(Stmts,removeLocalDef(St,All)).

  removeLocalDef(St,All) where (_,Id) ^= ruleName(St) =>
    _delMem(varSp(Id),All).

  collectStmtsRefs([],_,_,Rf,_) => either(Rf).
  collectStmtsRefs([St,..Sts],All,Annots,Rf,Rp) => do{
    Rf0 <- collectStmtRefs(St,All,Annots,Rf,Rp);
    collectStmtsRefs(Sts,All,Annots,Rf0,Rp)
  }

  collectStmtRefs:(ast,cons[defnSp],cons[(string,ast)],cons[defnSp],reports) =>
    either[reports,cons[defnSp]].
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
  collectStmtRefs(A,All,Annots,Rf,Rp) where (_,H,R) ^= isEquation(A) => do{
    Rf0 <- collectAnnotRefs(H,All,Annots,Rf,Rp);
    Rf1 <- collectHeadRefs(H,All,Rf0,Rp);
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
      (_,Tp,Els) ^= isContractStmt(A) => do{
	Rf0 <- collectContractRefs(Tp,All,Rf,Rp);
	collectFaceTypes(Els,All,Rf0,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,Tp,Exp) ^= isImplementationStmt(A) => do{
	A0 .= filterOut(All,Q);
	Rf1 <- collectConstraintRefs([Tp,..Cx],A0,Rf,Rp);
	collectTermRefs(Exp,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Cond) ^= isIntegrity(A) => collectCondRefs(Cond,All,Rf,Rp).
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Exp) ^= isShow(A) => collectTermRefs(Exp,All,Rf,Rp).
  collectStmtRefs(A,All,Annots,Rf,Rp) =>
    other(reportError(Rp,"cannot fathom definition $(A)",locOf(A))).

  isConstructorStmt(A) where (_,_,I) ^= isQuantified(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,_,I) ^= isXQuantified(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,I) ^= isPrivate(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,I) ^= isPublic(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) => _ ^= isBinary(A,"<=>").

  collectHeadRefs(Hd,All,Rf,Rp) where (_,H,C) ^= isWhere(Hd) => do{
    Rf0 <- collectTermRefs(H,All,Rf,Rp);
    collectCondRefs(C,All,Rf0,Rp)
  }
  collectHeadRefs(Hd,All,Rf,Rp) => 
    collectTermRefs(Hd,All,Rf,Rp).
    
  collectAnnotRefs(H,All,Annots,Rf,Rp) where Id^=headName(H) =>
    ((Id,Tp) in Annots ?
	collectTypeRefs(Tp,All,Rf,Rp) ||
	either(Rf)).
  collectAnnotRefs(H,_,_,_,Rp) => other(reportError(Rp,"not a head: $(H)",locOf(H))).

  collectCondRefs:(ast,cons[defnSp],cons[defnSp],reports) => either[reports,cons[defnSp]].
  collectCondRefs(A,All,Rf,Rp) where (_,L,R) ^= isConjunct(A) => do{
    Rf1 <- collectCondRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectCondRefs(A,All,Rf,Rp) where (_,L,R) ^= isDisjunct(A) => do{
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
    
  collectTermRefs:(ast,cons[defnSp],cons[defnSp],reports) => either[reports,cons[defnSp]].
  collectTermRefs(V,All,Rf,Rp) where (_,Id) ^= isName(V) =>
    either(collectName(varSp(Id),All,Rf)).
  collectTermRefs(T,All,Rf,Rp) where (_,Id) ^= isEnumSymb(T) =>
    either(collectName(cnsSp(Id),All,Rf)).
  collectTermRefs(T,All,Rf,Rp) where (_,Lhs,Rhs) ^= isTypeAnnotation(T) => do{
    Rf1 <- collectTermRefs(Lhs,All,Rf,Rp);
    collectTypeRefs(Rhs,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Env,Bnd) ^= isLetDef(T) => do{
    Rf1 <- collectTermRefs(Bnd,All,Rf,Rp);
    collectStmtsRefs(Env,All,[],Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Op,Args) ^= isRoundTerm(T) => do{
    Rf1 <- collectTermRefs(Op,All,Rf,Rp);
    collectTermListRefs(Args,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Args) ^= isTuple(T) => 
    collectTermListRefs(Args,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,Args) ^= isSqTuple(T) => 
    collectTermListRefs(Args,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isCons(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Bnd,Cond) ^= isComprehension(T) => do{
    Rf1 <- collectTermRefs(Bnd,All,Rf,Rp);
    collectCondRefs(Cond,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,E,C) ^= isCaseExp(T) => do{
    Rf0 <- collectTermRefs(E,All,Rf,Rp);
    collectCasesRefs(C,All,Rf0,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Sts) ^= isBrTuple(T) =>
    collectStmtsRefs(Sts,All,[],Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,Sts) ^= isQBrTuple(T) =>
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
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isIndex(T) => do{
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
  collectTermRefs(A,All,Rf,Rp) where (_,R) ^= isNegation(A) => 
    collectCondRefs(R,All,Rf,Rp).
  collectTermRefs(A,All,Rf,Rp) where (_,T,L,R) ^= isConditional(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    Rf1 <- collectTermRefs(L,All,Rf0,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isEquation(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
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
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isActionTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isLazyTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isTaskTerm(T) =>
    collectDoRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L) ^= isValof(T) =>
    collectTermRefs(L,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isAbstraction(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectCondRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isRecordUpdate(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,L,R) ^= isComma(T) => do{
    Rf1 <- collectTermRefs(L,All,Rf,Rp);
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectTermRefs(_,_,Rf,_) default => either(Rf).

  collectDoRefs:(ast,cons[defnSp],cons[defnSp],reports) => either[reports,cons[defnSp]].
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
    collectTermRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L,R) ^= isIfThenElse(A) => do{
    Rf0 <- collectTermRefs(T,All,Rf,Rp);
    Rf1 <- collectDoRefs(L,All,Rf0,Rp);
    collectDoRefs(R,All,Rf1,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L) ^= isIfThen(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    collectDoRefs(L,All,Rf,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L) ^= isWhileDo(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    collectDoRefs(L,All,Rf,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,T,L) ^= isForDo(A) => do{
    Rf0 <- collectCondRefs(T,All,Rf,Rp);
    collectDoRefs(L,All,Rf,Rp)
  }
  collectDoRefs(A,All,Rf,Rp) where (_,[S]) ^= isBrTuple(A) =>
    collectDoRefs(S,All,Rf,Rp).
  collectDoRefs(A,All,Rf,Rp) => collectTermRefs(A,All,Rf,Rp).
  
  collectCasesRefs([],_,Rf,_) => either(Rf).
  collectCasesRefs([St,..Sts],All,Rf,Rp) => do{
    Rf0 <- collectCaseRefs(St,All,Rf,Rp);
    collectCasesRefs(Sts,All,Rf0,Rp)
  }
  collectCaseRefs(Cse,All,Rf,Rp) where (_,Lhs,Rhs) ^= isEquation(Cse) => do{
    Rf1 <- collectTermRefs(Lhs,All,Rf,Rp);
    collectTermRefs(Rhs,All,Rf1,Rp)
  }
  collectCaseRefs(Cse,_,_,Rp) =>
    other(reportError(Rp,"invalid case in case expression $(Cse)",locOf(Cse))).
    
  collectTermListRefs:(cons[ast],cons[defnSp],cons[defnSp],reports) =>
    either[reports,cons[defnSp]].
  collectTermListRefs([],_,R,_) => either(R).
  collectTermListRefs([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTermRefs(T,All,Rf,Rp);
    collectTermListRefs(Ts,All,R1,Rp)
  }

  collectTypeRefs:(ast,cons[defnSp],cons[defnSp],reports) => either[reports,cons[defnSp]].
  collectTypeRefs(V,All,SoFar,Rp) where (_,Id) ^= isName(V) =>
    either(collectName(tpSp(Id),All,SoFar)).
  collectTypeRefs(T,All,SoFar,Rp) where (_,Op,Els) ^= isSquareTerm(T) => do{
    R1 <- collectTypeRefs(Op,All,SoFar,Rp);
    collectTypeList(Els,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,">") => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,"<=>") => do{
    R1 <- collectTypeRefs(L,All,SoFar,Rp);
    collectTypeRefs(R,All,R1,Rp)
  }
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,"=>") => do{
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
  collectTypeRefs(T,All,SoFar,Rp) where (_,Op,Els) ^= isRoundTerm(T) => do{
    R1 <- collectTypeRefs(Op,All,SoFar,Rp);
    collectTypeList(Els,All,R1,Rp)
  }
  collectTypeRefs(T,_,_,Rp) default =>
    other(reportError(Rp,"cannot fathom type $(T)",locOf(T))).
  
  collectTypeList:(cons[ast],cons[defnSp],cons[defnSp],reports) => either[reports,cons[defnSp]].
  collectTypeList([],_,R,_) => either(R).
  collectTypeList([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTypeRefs(T,All,Rf,Rp);
    collectTypeList(Ts,All,R1,Rp)
  }

  collectConstraintRefs:(cons[ast],cons[defnSp],cons[defnSp],reports) =>
    either[reports,cons[defnSp]].
  collectConstraintRefs([],_,R,_) => either(R).
  collectConstraintRefs([T,..Ts],All,Rf,Rp) where _ ^= isSquareTerm(T) => do {
    R1 <- collectContractRefs(T,All,Rf,Rp);
    collectConstraintRefs(Ts,All,R1,Rp)
  }
  collectConstraintRefs([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTypeRefs(T,All,Rf,Rp);
    collectConstraintRefs(Ts,All,R1,Rp)
  }

  collectContractRefs:(ast,cons[defnSp],cons[defnSp],reports) =>
    either[reports,cons[defnSp]].
  collectContractRefs(T,All,Rf,Rp) where (_,Op,Args) ^= isSquareTerm(T) => do {
    (_,Id) ^= isName(Op);
    R0 .= collectName(conSp(Id),All,Rf);
    collectTypeList(Args,All,R0,Rp)
  }
  collectContractRefs(T,All,Rf,Rp) where (_,Q,I) ^= isQuantified(T) =>
    collectContractRefs(I,filterOut(All,Q),Rf,Rp).
  collectContractRefs(T,All,Rf,Rp) where (_,Q,I) ^= isXQuantified(T) =>
    collectContractRefs(I,filterOut(All,Q),Rf,Rp).

  collectFaceTypes([],_,R,_) => either(R).
  collectFaceTypes([D,..Ds],All,R,Rp) => do{
    R1 <- collectFaceType(D,All,R,Rp);
    collectFaceTypes(Ds,All,R1,Rp)
  }

  collectFaceType(P,All,R,Rp) where (_,I) ^= isUnary(P,"type") =>
    collectFaceType(I,All,R,Rp).
  collectFaceType(P,All,R,Rp) where (_,_,T) ^= isTypeAnnotation(P) =>
    collectTypeRefs(T,All,R,Rp).
  collectFaceType(_,All,R,_) => either(R).

  collectName(Sp,All,SoFar) where Sp in All && !Sp in SoFar => [Sp,..SoFar].
  collectName(_,_,SoFar) default => SoFar.

  filterOut:(cons[defnSp],cons[ast]) => cons[defnSp].
  filterOut([],_) => [].
  filterOut([tpSp(Nm),..As],Q) where V in Q && (_,Nm)^=isName(V) =>
    filterOut(As,Q).
  filterOut([Sp,..As],Q) => [Sp,..filterOut(As,Q)].
}
