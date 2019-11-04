star.compiler.dependencies{
  import star.
  import star.topsort.
  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.keywords.
  import star.compiler.meta.
  import star.compiler.wff.
  import star.compiler.misc.

  public dependencies:(list[ast],reports) =>
    either[reports,
      (list[(defnSp,visibility)],list[ast],list[(string,ast)],list[list[defnSpec]])].
  dependencies(Dfs,Rp) => do{
    (Defs,Pb,As,Opn) <- collectDefinitions(Dfs,Rp);
    AllRefs = Defs//((defnSpec(Nm,_,_))=>Nm);

    logMsg("found defs $(AllRefs)");

    InitDefs <- collectThetaRefs(Defs,AllRefs,As,[],Rp);
    Groups = topsort(InitDefs) // ((Gp)=>(Gp//((definition(Sp,Lc,_,Els))=>defnSpec(Sp,Lc,Els))));
    
    valis (Pb,Opn,As,Groups)
  }

  definitionSpec ::= definition(defnSp,locn,list[defnSp],list[ast]).

  implementation depends[definitionSpec->>defnSp] => {
    references(definition(_,_,Refs,_)) => Refs.
    defined(definition(Sp,_,_,_),Rf) => Sp==Rf.
  }

  collectDefinitions:(list[ast],
    reports) => either[reports,(list[defnSpec],list[(defnSp,visibility)],
      list[(string,ast)],list[ast])].
  collectDefinitions(Stmts,Rp) => collectDefs(Stmts,[],[],[],[],Rp).

  collectDefs:(list[ast],list[defnSpec],list[(defnSp,visibility)],list[(string,ast)],list[ast],reports) => either[reports,(list[defnSpec],list[(defnSp,visibility)],
      list[(string,ast)],list[ast])].
  
  collectDefs([],Defs,Pb,As,Opn,Rp) => either((Defs,Pb,As,Opn)).
  collectDefs([A,..Ss],Defs,Pb,As,Opn,Rp) => do{
    (SS1,Dfs1,Pb1,As1,Opn1) <- collectDefinition(A,Ss,Defs,Pb,As,Opn,deFault,Rp);
    collectDefs(SS1,Dfs1,Pb1,As1,Opn1,Rp)
  }
    
  collectDefinition:(ast,
    list[ast],
    list[defnSpec],
    list[(defnSp,visibility)],
    list[(string,ast)],
    list[ast],
    visibility,
    reports) => either[reports,(list[ast],list[defnSpec],
      list[(defnSp,visibility)],list[(string,ast)],list[ast])].

  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      Spec ^= isOpen(A) => either((Stmts,Defs,Pb,As,[A,..Opn])).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,V,T) ^= isTypeAnnotation(A) => do{
	if _ ^= isConstructorType(T) then {
	  if (_,V1) ^= isPrivate(V) && (ILc,Id) ^= isName(V1) then {
	    valis (Stmts,[defnSpec(cnsSp(Id),Lc,[T]),..Defs],
	      [(cnsSp(Id),priVate),..Pb],[(Id,T),..As],Opn)
	  }
	  else if (ILc,Id) ^= isName(V) then{
	    valis (Stmts,[defnSpec(cnsSp(Id),Lc,[T]),..Defs],
	      [(cnsSp(Id),Vz),..Pb],[(Id,T),..As],Opn)
	  }
	  else
	  throw reportError(Rp,"cannot fathom type annotation $(A)",Lc)
	} else if (VLc,Id) ^= isName(V) then{
	  valis (Stmts,Defs,[(varSp(Id),Vz),..Pb],[(Id,T),..As],Opn)
	} else
	  throw reportError(Rp,"cannot fathom type annotation $(A)",Lc)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      (_,Ai) ^= isPublic(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,pUblic,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,_,Rp) where
      (_,Ai) ^= isPrivate(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Opn,priVate,Rp).
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
	Sp = varSp(Id);
	valis (Ss,[defnSpec(Sp,Lc,[A]),..Defs],[(Sp,Vz),..Pb],As,Opn)
      }.
  collectDefinition(A,Ss,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm,Rhs) ^= isAssignment(A) && (LLc,Id) ^= isName(Nm) => do{
	Sp = varSp(Id); -- map X:=E to X=!!E
	valis (Ss,[defnSpec(Sp,Lc,[binary(Lc,"=",Nm,unary(Lc,"!!",Rhs))]),..Defs],
	  [(Sp,Vz),..Pb],As,Opn)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Opn,Vz,Rp) where
      (Lc,Nm) ^= ruleName(A) => do{
	(Ss,Dfs) = collectDefines(Stmts,Nm,[]);
	Sp = varSp(Nm);
	valis (Ss,[defnSpec(Sp,Lc,[A,..Dfs]),..Defs],[(Sp,Vz),..Pb],As,Opn)
      }.

  collectDefines:(list[ast],string,list[ast]) => (list[ast],list[ast]).
  collectDefines([St,..Ss],Nm,Dfs) where
      (_,Nm) ^= ruleName(St) => collectDefines(Ss,Nm,[Dfs..,St]).
  collectDefines(Ss,Nm,Dfs) default => (Ss,Dfs).
	
  generateAnnotations:(list[ast],list[ast],list[ast],list[(string,ast)]) =>
    list[(string,ast)].
  generateAnnotations([],_,_,As) => As.
  generateAnnotations([A,..Ss],Qs,Cs,As) where
      (_,V,T) ^= isTypeAnnotation(A) && (_,Id) ^= isName(V) =>
    generateAnnotations(Ss,Qs,Cs,[(Id,reUQuant(Qs,reConstrain(Cs,T))),..As]).
  generateAnnotations([A,..Ss],Qs,Cs,As) =>
    generateAnnotations(Ss,Qs,Cs,As).

  

  collectThetaRefs:(list[defnSpec],list[defnSp],list[(string,ast)],
    list[definitionSpec],reports) =>
    either[reports,list[definitionSpec]].
  collectThetaRefs([],_,_,DSpecs,_) => either(DSpecs).
  collectThetaRefs([defnSpec(cnsSp(Nm),Lc,[Def]),..Defs],AllRefs,Annots,S,Rp) => do{
    Refs <- collectTypeRefs(Def,AllRefs,[],Rp);
    collectThetaRefs(Defs,AllRefs,Annots,[definition(cnsSp(Nm),Lc,Refs,[Def]),..S],Rp)
  }
  collectThetaRefs([defnSpec(Defines,Lc,Stmts),..Defs],AllRefs,Annots,S,Rp) => do{
    Refs <- collectStmtsRefs(Stmts,AllRefs,Annots,[],Rp);
    collectThetaRefs(Defs,AllRefs,Annots,[definition(Defines,Lc,Refs,Stmts),..S],Rp)
  }.

  collectEnvRefs:(list[ast],list[defnSp],list[(string,ast)],list[defnSp],reports) =>
    either[reports,list[defnSp]].
  collectEnvRefs(Defs,All,Annots,Rf,Rp) =>
    collectStmtsRefs(Defs,locallyDefined(Defs,All),Annots,Rf,Rp).

  locallyDefined:(list[ast],list[defnSp]) => list[defnSp].
  locallyDefined([],All) => All.
  locallyDefined([St,..Stmts],All) =>
    locallyDefined(Stmts,removeLocalDef(St,All)).

  removeLocalDef(St,All) where (_,Id) ^= ruleName(St) =>
    subtract(varSp(Id),All).

  collectStmtsRefs([],_,_,Rf,_) => either(Rf).
  collectStmtsRefs([St,..Sts],All,Annots,Rf,Rp) => do{
    Rf0 <- collectStmtRefs(St,All,Annots,Rf,Rp);
    collectStmtsRefs(Sts,All,Annots,Rf0,Rp)
  }

  collectStmtRefs:(ast,list[defnSp],list[(string,ast)],list[defnSp],reports) =>
    either[reports,list[defnSp]].
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
	A0 = filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(L,A0,Rf0,Rp);
	collectTypeRefs(R,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,L,R) ^= isTypeFunStmt(A) => do{
	A0 = filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(L,A0,Rf0,Rp);
	collectTypeRefs(R,A0,Rf1,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Tp,Els) ^= isContractStmt(A) => do{
	Rf0 <- collectTypeRefs(Tp,All,Rf,Rp);
	collectFaceTypes(Els,All,Rf0,Rp)
      }.
  collectStmtRefs(A,All,Annots,Rf,Rp) where
      (_,Q,Cx,Tp,Exp) ^= isImplementationStmt(A) => do{
	A0 = filterOut(All,Q);
	Rf0 <- collectConstraintRefs(Cx,A0,Rf,Rp);
	Rf1 <- collectTypeRefs(Tp,A0,Rf0,Rp);
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

  collectCondRefs:(ast,list[defnSp],list[defnSp],reports) => either[reports,list[defnSp]].
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
    
  collectTermRefs:(ast,list[defnSp],list[defnSp],reports) => either[reports,list[defnSp]].
  collectTermRefs(V,All,Rf,Rp) where (_,Id) ^= isName(V) =>
    (varSp(Id) in All ?
	either(collectName(varSp(Id),All,Rf)) ||
	either(collectName(cnsSp(Id),All,Rf))).
  collectTermRefs(T,All,Rf,Rp) where (_,Lhs,Rhs) ^= isTypeAnnotation(T) => do{
    Rf1 <- collectTermRefs(Lhs,All,Rf,Rp);
    collectTypeRefs(Rhs,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Env,Bnd) ^= isLetDef(T) => do{
    Rf1 <- collectTermRefs(Bnd,All,Rf,Rp);
    collectTermRefs(Env,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Op,Args) ^= isRoundTerm(T) => do{
    Rf1 <- collectTermRefs(Op,All,Rf,Rp);
    collectTermListRefs(Args,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,Args) ^= isTuple(T) => 
    collectTermListRefs(Args,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,Args) ^= isSqTuple(T) => 
    collectTermListRefs(Args,All,Rf,Rp).
  collectTermRefs(T,All,Rf,Rp) where (_,Bnd,Cond) ^= isComprehension(T) => do{
    Rf1 <- collectTermRefs(Bnd,All,Rf,Rp);
    collectCondRefs(Cond,All,Rf1,Rp)
  }
  collectTermRefs(T,All,Rf,Rp) where (_,E,C) ^= isCaseExp(T) => do{
    Rf0 <- collectTermRefs(E,All,Rf,Rp);
    collectCasesRefs(C,All,Rf0,Rp)
  }

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
    
  collectTermRefs(_,_,Rf,_) default => either(Rf).

  collectTermListRefs:(list[ast],list[defnSp],list[defnSp],reports) =>
    either[reports,list[defnSp]].
  collectTermListRefs([],_,R,_) => either(R).
  collectTermListRefs([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTermRefs(T,All,Rf,Rp);
    collectTermListRefs(Ts,All,R1,Rp)
  }

  collectTypeRefs:(ast,list[defnSp],list[defnSp],reports) => either[reports,list[defnSp]].
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
  collectTypeRefs(T,All,SoFar,Rp) where (_,L,R) ^= isBinary(T,",") => do{
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
  
  collectTypeList:(list[ast],list[defnSp],list[defnSp],reports) => either[reports,list[defnSp]].
  collectTypeList([],_,R,_) => either(R).
  collectTypeList([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTypeRefs(T,All,Rf,Rp);
    collectTypeList(Ts,All,R1,Rp)
  }

  collectConstraintRefs:(list[ast],list[defnSp],list[defnSp],reports) =>
    either[reports,list[defnSp]].
  collectConstraintRefs([],_,R,_) => either(R).
  collectConstraintRefs([T,..Ts],All,Rf,Rp) => do {
    R1 <- collectTypeRefs(T,All,Rf,Rp);
    collectConstraintRefs(Ts,All,R1,Rp)
  }

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

  collectName(Sp,All,SoFar) where Sp in All && \+ Sp in SoFar => [Sp,..SoFar].
  collectName(_,_,SoFar) default => SoFar.

  filterOut:(list[defnSp],list[ast]) => list[defnSp].
  filterOut([],_) => [].
  filterOut([tpSp(Nm),..As],Q) where V in Q && (_,Nm)^=isName(V) =>
    filterOut(As,Q).
  filterOut([Sp,..As],Q) => [Sp,..filterOut(As,Q)].
}
