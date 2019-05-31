star.compiler.wff{
  import star.
  import star.pkg.
  import star.topsort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.

  public isQuantified:(ast)=>option[(locn,list[ast],ast)].
  isQuantified(T) where
      (Lc,Lh,B)^=isBinary(T,"~~") && (_,V)^=isUnary(Lh,"all") =>
    some((Lc,deComma(V),B)).
  isQuantified(_) default => none.

  public reUQuant:(list[ast],ast) => ast.
  reUQuant([],T) => T.
  reUQuant([Q,..Qs],T) where Lc .= locOf(T) =>
    unary(Lc,"all",binary(Lc,"~~",reComma(Qs,Q),T)).

  public isXQuantified:(ast)=>option[(locn,list[ast],ast)].
  isXQuantified(T) where
      (Lc,Lh,B)^=isBinary(T,"~~") && (_,V)^=isUnary(Lh,"exists") =>
    some((Lc,deComma(V),B)).
  isXQuantified(_) default => none.

  public reXQuant:(list[ast],ast) => ast.
  reXQuant([],T) => T.
  reXQuant([Q,..Qs],T) where Lc .= locOf(T) =>
    unary(Lc,"exists",binary(Lc,"~~",reComma(Qs,Q),T)).

  public isConstrained:(ast) => option[(locn,list[ast],ast)].
  isConstrained(T) where
      (Lc,Lh,B) ^= isBinary(T,"|:") => some((Lc,deComma(Lh),B)).
  isConstrained(_) default => none.

  public reConstrain:(list[ast],ast) => ast.
  reConstrain([],T) => T.
  reConstrain([C,..Cs],T) => binary(locOf(T),"|:",reComma(Cs,C),T).

  public isConstructorType:(ast) => option[(locn,ast,ast)].
  isConstructorType(A) => isBinary(A,"<=>").
    
  public deComma:(ast) => list[ast].
  deComma(Trm) => let{
    deC(T,SoF) where (_,Lh,Rh)^=isBinary(T,",") =>
      deC(Rh,deC(Lh,SoF)).
    deC(T,SoF) => [T,..SoF].
  } in deC(Trm,[]).

  public reComma:(list[ast],ast) => ast.
  reComma([],A) => A.
  reComma([A,..As],B) =>
    binary(locOf(A),",",B,reComma(As,A)).

  public isDepends:(ast) => option[(list[ast],list[ast])].
  isDepends(T) where (_,Lh,Rh)^=isBinary(T,"->>") =>
    some((deComma(Lh),deComma(Rh))).
  isDepends(_) default => none.

  public isTypeLambda:(ast) => option[(locn,ast,ast)].
  isTypeLambda(A) => isBinary(A,"~>").

  public isTypeAnnotation:(ast)=>option[(locn,ast,ast)].
  isTypeAnnotation(A)=>isBinary(A,":").

  public isTypeExistsStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isTypeExistsStmt(A) where
      (Lc,Q,I) ^= isQuantified(A) &&
      (_,_,Cx,L,R) ^= isTypeExistsStmt(I) => some((Lc,Q,Cx,L,R)).
  isTypeExistsStmt(A) where
      (Lc,C,I) ^= isBinary(A,"|:") &&
      (_,Q,_,L,R) ^= isTypeExistsStmt(I) => some((Lc,Q,deComma(C),L,R)).
  isTypeExistsStmt(A) where
      (Lc,H,I) ^= isBinary(A,"<~") =>
    some((Lc,getQuantifiers(H),[],H,I)).
  isTypeExistsStmt(A) default => none.

  public isTypeFunStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isTypeFunStmt(A) where
      (Lc,Q,I) ^= isQuantified(A) &&
      (_,_,Cx,L,R) ^= isTypeFunStmt(I) => some((Lc,Q,Cx,L,R)).
  isTypeFunStmt(A) where
      (Lc,C,I) ^= isBinary(A,"|:") &&
      (_,Q,_,L,R) ^= isTypeFunStmt(I) => some((Lc,Q,deComma(C),L,R)).
  isTypeFunStmt(A) where
      (Lc,H,I) ^= isBinary(A,"~>") =>
    some((Lc,getQuantifiers(H),[],H,I)).
  isTypeFunStmt(A) default => none.

  public isAlgebraicTypeStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isAlgebraicTypeStmt(A) where
      (Lc,Q,I) ^= isQuantified(A) &&
      (_,_,Cx,L,R) ^= isAlgebraicTypeStmt(I) => some((Lc,Q,Cx,L,R)).
  isAlgebraicTypeStmt(A) where
      (Lc,C,I) ^= isBinary(A,"|:") &&
      (_,Q,_,L,R) ^= isAlgebraicTypeStmt(I) => some((Lc,Q,deComma(C),L,R)).
  isAlgebraicTypeStmt(A) where
      (Lc,H,I) ^= isBinary(A,"::=") =>
    some((Lc,getQuantifiers(H),[],H,I)).
  isAlgebraicTypeStmt(A) default => none.

  exportFn ~> (defnSp,list[defnSp])=>list[defnSp].

  reformAlgebraic:(locn,list[ast],list[ast],ast,ast,
    list[defnSpec],
    list[defnSp],
    list[(string,ast)],exportFn,reports) =>
    either[reports,(list[defnSpec],
	list[defnSp],list[(string,ast)])].
  reformAlgebraic(Lc,Q,Cx,H,R,Defs,Pb,As,Exp,Rp) => do{
    Nm = typeName(H);
    Face <- algebraicFace(R,Rp);
    ExTp = reUQuant(Q,reConstrain(Cx,binary(Lc,"<~",H,Face)));
    Spec = tpSp(Nm);
    buildConstructors(R,Q,Cx,H,[defnSpec(Spec,Lc,[ExTp]),..Defs],Exp(Spec,Pb),As,Exp,Rp)
  }

  algebraicFace:(ast,reports) => either[reports,ast].
  algebraicFace(A,Rp) where (_,L,R) ^= isBinary(A,"|") => do{
    Lhs <- algebraicFace(L,Rp);
    Rhs <- algebraicFace(R,Rp);
    combineFaces(Lhs,Rhs,Rp)
  }
  algebraicFace(A,Rp) where (Lc,_,_) ^= isRoundTerm(A) => either(brTuple(Lc,[])).
  algebraicFace(A,Rp) where (Lc,_) ^= isName(A) => either(brTuple(Lc,[])).
  algebraicFace(A,Rp) where (Lc,_,Els) ^= isBrTerm(A) => either(brTuple(Lc,Els)).

  combineFaces(F1,F2,Rp) where (_,[]) ^= isBrTuple(F1) => either(F2).
  combineFaces(F1,F2,Rp) where (_,[]) ^= isBrTuple(F2) => either(F1).
  combineFaces(F1,F2,Rp) => other(reportError(Rp,"only one record constructor allowed",
      locOf(F1))).

  public isLetDef:(ast) => option[(locn,ast,ast)].
  isLetDef(A) where (Lc,Lh,Rh) ^= isBinary(A,"in") &&
      app(_,nme(_,"let"),Body) .= Lh &&
      (_ ^= isBrTuple(Body) || _ ^= isQBrTuple(Body)) => some((Lc,Body,Rh)).
  isLetDef(_) default => none.

  public isComprehension:(ast) => option[(locn,ast,ast)].
  isComprehension(A) where (Lc,[T]) ^= isBrTuple(A) &&
      (_,Bnd,Body) ^= isBinary(T,"|") => some((Lc,Bnd,Body)).
  isComprehension(A) => none.

  public isConjunct(A) => isBinary(A,"&&").

  public isDisjunct(A) => isBinary(A,"||").

  public isNegation(A) => isUnary(A,"\\+").

  public isConditional(A) where
      (Lc,Tst,Rhs) ^= isBinary(A,"?") &&
      (_,Th,El) ^= isBinary(Rhs,"||") => some((Lc,Tst,Th,El)).
  isConditional(_) => none.

  buildConstructors:(ast,
    list[ast],list[ast],ast,
    list[defnSpec],
    list[defnSp],
    list[(string,ast)],
    exportFn,
    reports
  ) => either[reports,(list[defnSpec],
      list[defnSp],list[(string,ast)])].
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) where
      (Lc,L,R) ^= isBinary(A,"|") => do{
	(Dfs1,Pb1,As1) <- buildConstructors(L,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp);
	(Dfs2,Pb2,As2) <- buildConstructors(R,Qs,Cx,Tp,Dfs1,Pb1,As1,Exp,Rp);
	valis (Dfs2,Pb2,As2)
      }.
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) => let{
	Con = reUQuant(Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",reXQuant(XQs,
		reConstrain(XCx,brTuple(Lc,Els))),Tp))).
	Sp = cnsSp(Nm).
	Def = defnSpec(Sp,Lc,[Con]).
      } in either(([Def,..Defs],Exp(Sp,Pb),[(Nm,Con),..As])).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isRoundCon(A) => let{
	Con = reUQuant(Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",rndTuple(Lc,Els),Tp))).
	Sp = cnsSp(Nm).
	Def = defnSpec(Sp,Lc,[Con]).
      } in either(([Def,..Defs],Exp(Sp,Pb),[(Nm,Con),..As])).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) where
      (Lc,Nm) ^= isName(A) => let{
	Con = reUQuant(Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",rndTuple(Lc,[]),Tp))).
	Sp = cnsSp(Nm).
	Def = defnSpec(Sp,Lc,[Con]).
      } in either(([Def,..Defs],Exp(Sp,Pb),[(Nm,Con),..As])).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) where
      (_,I) ^= isPrivate(A) => 
    buildConstructors(I,Qs,Cx,Tp,Defs,Pb,As,noExport,Rp).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) where
      (_,I) ^= isPublic(A) => 
    buildConstructors(I,Qs,Cx,Tp,Defs,Pb,As,collectExport,Rp).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Exp,Rp) =>
    other(reportError(Rp,"cannot fathom constructor $(A)",locOf(A))).
    
  isBraceCon:(ast) => option[(locn,string,list[ast],list[ast],list[ast])].
  isBraceCon(A) => isCon(A,isBrTerm).

  isRoundCon:(ast) => option[(locn,string,list[ast],list[ast],list[ast])].
  isRoundCon(A) => isCon(A,isRoundTerm).

  isCon:(ast,(ast)=>option[(locn,ast,list[ast])]) => option[(locn,string,list[ast],list[ast],list[ast])].
  isCon(A,P) where
      (Lc,Nm,Els) ^= P(A) && (_,Id) ^= isName(Nm) => some((Lc,Id,[],[],Els)).
  isCon(A,P) where
      (Lc,Q,I) ^= isXQuantified(A) &&
      (_,Nm,_,Cx,Els) ^= isCon(I,P) =>
    some((Lc,Nm,Q,Cx,Els)).
  isCon(A,P) where
      (Lc,Cx,I) ^= isConstrained(A) &&
      (_,Nm,Q,_,Els) ^= isCon(I,P) =>
    some((Lc,Nm,Q,Cx,Els)).
  isCon(_,_) default => none.
    
  
  getQuantifiers(T) where
      (_,Q,_) ^= isQuantified(T) => Q.
  getQuantifiers(T) where
      (_,_,A) ^= isSquareTerm(T) => A.
  getQuantifiers(_) default => [].

  public isFieldAcc:(ast) => option[(locn,ast,ast)].
  isFieldAcc(A) => isBinary(A,".").

  public isPublic:(ast) => option[(locn,ast)].
  isPublic(A) => isUnary(A,"public").

  public isPrivate:(ast) => option[(locn,ast)].
  isPrivate(A) => isUnary(A,"private").

  public isImport:(ast)=> option[importSpec].
  isImport(A) where (Lc,I) ^= isPublic(A) =>
    (pkgImp(_,_,Im) ^= isImport(I) ?
	some(pkgImp(Lc,pUblic,Im)) ||
	none).
  isImport(A) where (Lc,I) ^= isPrivate(A) =>
    (pkgImp(_,_,Im) ^= isImport(I) ?
	some(pkgImp(Lc,priVate,Im)) ||
	none).
  isImport(A) where (Lc,I) ^= isUnary(A,"import") => do{
    Pkg <- pkgName(I);
    valis pkgImp(Lc,priVate,Pkg)
  }
  isImport(_) default => none.

  public pkgName:(ast) => option[pkg].
  pkgName(A) where (_,L,R) ^= isBinary(A,"#") => do{
    Nm <- dottedName(L);
    Vr <- dottedName(R);
    valis pkg(Nm,vers(Vr))
  }
  pkgName(A) => do{
    Nm <- dottedName(A);
    valis pkg(Nm,defltVersion)
  }

  dottedName:(ast) => option[string].
  dottedName(N) where (_,Id) ^= isName(N) => some(Id).
  dottedName(N) where (_,L,R) ^= isBinary(N,".") => do{
    LL <- dottedName(L);
    RR <- dottedName(R);
    valis "$(LL).$(RR)"
  }
  dottedName(_) default => none.

  public isOpen:(ast)=> option[importSpec].
  isOpen(A) where (Lc,Nm) ^= isUnary(A,"open") => some(openStmt(Lc,Nm)).
  isOpen(_) default => none.

  public isIntegrity:(ast)=> option[(locn,ast)].
  isIntegrity(A) => isUnary(A,"assert").

  public isShow:(ast) => option[(locn,ast)].
  isShow(A) => isUnary(A,"show").

  public isTypeAnnotation:(ast) => option[(locn,ast,ast)].
  isTypeAnnotation(A) => isBinary(A,":").

  public isCoerce:(ast) => option[(locn,ast,ast)].
  isCoerce(A) => isBinary(A,"::").

  public isContractStmt:(ast) => option[(locn,ast,list[ast])].
  isContractStmt(A) where
      (Lc,I) ^= isUnary(A,"contract") &&
      (_,Lhs,B) ^= isBinary(I,"::=") &&
      (_,Els) ^= isBrTuple(B) => some((Lc,B,Els)).
  isContractStmt(A) default => none.

  isContractSpec:(ast) => option[(locn,string,list[ast],list[ast])].
  isContractSpec(A) where
      (Lc,Quants,I) ^= isQuantified(A) &&
      (_,Nm,_,II) ^= isContractSpec(I) => some((Lc,Nm,Quants,II)).
  isContractSpec(A) where
      (Lc,Lhs,Rhs) ^= isBinary(A,"|:") &&
      (_,Nm,_,II) ^= isContractSpec(Rhs) => some((Lc,Nm,[],II++deComma(Lhs))).
  isContractSpec(A) where
      (Lc,Nm,_) ^= isSquareTerm(A) &&
      (_,Id) ^= isName(Nm) => some((Lc,Id,[],[A])).
  isContractSpec(_) default => none.

  public isImplementationStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isImplementationStmt(A) where
      (Lc,I) ^= isUnary(A,"implementation") => isImplSpec(locOf(A),[],[],I).
  isImplementationStmt(_) default => none.

  isImplSpec(Lc,_,Cs,T) where
      (_,Qs,In) ^= isQuantified(T) =>
    isImplSpec(Lc,Qs,Cs,T).
  isImplSpec(Lc,Qs,_,T) where
      (_,Lhs,Rhs) ^= isBinary(T,"|:") =>
    isImplSpec(Lc,Qs,deComma(Lhs),Rhs).
  isImplSpec(_,Qs,Cs,T) where
      (Lc,Cn,Exp) ^= isBinary(T,"=>") =>
    some((Lc,Qs,Cs,Cn,Exp)).
  isImplSpec(_,_,_,_) default => none.

  implementedContractName:(ast) => string.
  implementedContractName(A) where
      (_,O,As) ^= isSquareApply(A) =>
    ssSeq([ss(O),..surfaceNames(As,markerString(overMark))])::string.

  surfaceNames([],_) => [].
  surfaceNames([T,.._],Sep) where (_,L,_) ^= isBinary(T,"->>") =>
    surfaceNames(deComma(L),Sep).
  surfaceNames([T,..Ts],Sep) =>
    [ss(Sep),ss(surfaceName(T)),..surfaceNames(Ts,Sep)].

  surfaceName(T) where (_,Id) ^= isName(T) => Id.
  surfaceName(T) where (_,Id,_) ^= isSquareApply(T) => Id.
  surfaceName(T) where (_,_,I) ^= isQuantified(T) => surfaceName(I).
  surfaceName(T) where (_,Els) ^= isTuple(T) => "()$(size(Els))".

  typeName(Tp) where (_,Id) ^= isName(Tp) => Id.
  typeName(Tp) where (_,Id,_) ^= isSquareApply(Tp) => Id.
  typeName(Tp) where (_,Els) ^= isTuple(Tp) => "()$(size(Els))".

  public collectDefinitions:(list[ast],
    list[defnSpec],
    list[defnSp],
    list[(string,ast)],
    list[importSpec],
    list[ast],
    reports) => either[reports,(list[defnSpec],
      list[defnSp],list[(string,ast)],list[importSpec],list[ast])].
  collectDefinitions([],Defs,Pb,As,Imp,Oth,Rp) => either((Defs,Pb,As,Imp,Oth)).
  collectDefinitions([A,..Ss],Defs,Pb,As,Imp,Oth,Rp) => do{
    (SS1,Dfs1,Pb1,As1,Imp1,Oth1) <- collectDefinition(A,Ss,Defs,Pb,As,Imp,Oth,noExport,Rp);
    collectDefinitions(SS1,Dfs1,Pb1,As1,Imp1,Oth1,Rp)
  }
    
  collectDefinition:(ast,
    list[ast],
    list[defnSpec],
    list[defnSp],
    list[(string,ast)],
    list[importSpec],
    list[ast],
    exportFn,
    reports) => either[reports,(list[ast],list[defnSpec],
      list[defnSp],list[(string,ast)],list[importSpec],list[ast])].

  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,_,Rp) where
      Spec ^= isImport(A) => either((Stmts,Defs,Pb,As,[Spec,..Imp],Oth)).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,_,Rp) where
      Spec ^= isOpen(A) => either((Stmts,Defs,Pb,As,[Spec,..Imp],Oth)).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,_,Rp) where
      _ ^= isIntegrity(A) => either((Stmts,Defs,Pb,As,Imp,[A,..Oth])).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,_,Rp) where
      _ ^= isShow(A) => either((Stmts,Defs,Pb,As,Imp,[A,..Oth])).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,V,T) ^= isTypeAnnotation(A) => do{
	if _ ^= isConstructorType(T) then {
	  if (_,V1) ^= isPrivate(V) && (ILc,Id) ^= isName(V1) then {
	    valis (Stmts,[defnSpec(cnsSp(Id),Lc,[T]),..Defs],
	      Pb,[(Id,T),..As],Imp,Oth)
	  }
	    else if (ILc,Id) ^= isName(V) then{
	      valis (Stmts,[defnSpec(cnsSp(Id),Lc,[T]),..Defs],
		Ex(cnsSp(Id),Pb),[(Id,T),..As],Imp,Oth)
	      }
		else
		  throw reportError(Rp,"cannot fathom type annotation $(A)",Lc)
		  
	} else if (VLc,Id) ^= isName(V) then{
	  valis (Stmts,Defs,Ex(varSp(Id),Pb),[(Id,T),..As],Imp,Oth)
	} else
	  throw reportError(Rp,"cannot fathom type annotation $(A)",Lc)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (_,Ai) ^= isPublic(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Imp,Oth,collectExport,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (_,Ai) ^= isPrivate(A) =>
    collectDefinition(Ai,Stmts,Defs,Pb,As,Imp,Oth,noExport,Rp).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,S,Els) ^= isContractStmt(A) &&
      (_,Nm,Qs,Cs) ^= isContractSpec(S)  =>
    either((Stmts,[defnSpec(conSp(Nm),Lc,[A]),..Defs],
	Ex(conSp(Nm),Pb),
	generateAnnotations(Qs,Els,Cs,As),
	Imp,
	Oth)).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,_,_,Cn,_) ^= isImplementationStmt(A) &&
      Sp .= implSp(implementedContractName(Cn)) =>
    either((Stmts,[defnSpec(Sp,Lc,[A]),..Defs],Ex(Sp,Pb),As,Imp,Oth)).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,_,_,L,R) ^= isTypeExistsStmt(A) && Sp .= tpSp(typeName(L)) =>
    either((Stmts,[defnSpec(Sp,Lc,[A]),..Defs],Ex(Sp,Pb),As,Imp,Oth)).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,_,_,L,R) ^= isTypeFunStmt(A) && Sp .= tpSp(typeName(L)) =>
    either((Stmts,[defnSpec(Sp,Lc,[A]),..Defs],Ex(Sp,Pb),As,Imp,Oth)).
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,Q,Cx,H,R) ^= isAlgebraicTypeStmt(A) => do{
	(Dfs1,Pb1,As1) <- reformAlgebraic(Lc,Q,Cx,H,R,Defs,Pb,As,Ex,Rp);
	valis (Stmts,Dfs1,Pb1,As1,Imp,Oth)
      }.
  collectDefinition(A,Stmts,Defs,Pb,As,Imp,Oth,Ex,Rp) where
      (Lc,Nm) ^= ruleName(A) => do{
	(Ss,Dfs) = collectDefines(Stmts,Nm,[]);
	Sp = varSp(Nm);
	valis (Ss,[defnSpec(Sp,Lc,[A,..Dfs]),..Defs],Ex(Sp,Pb),As,Imp,Oth)
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
  
  collectExport:(defnSp,list[defnSp]) => list[defnSp].
  collectExport(Sp,Pb) => [Sp,..Pb].

  noExport:(defnSp,list[defnSp]) => list[defnSp].
  noExport(_,Pb) => Pb.

  public ruleName:(ast) => option[(locn,string)].
  ruleName(A) where
      (Lc,Hd) ^= headOfRule(A) &&
      Id ^= headName(Hd) => some((Lc,Id)).
  ruleName(_) default => none.

  headOfRule:(ast) => option[(locn,ast)].
  headOfRule(A) where
      (Lc,Hd,_) ^= isDefn(A) => some((Lc,Hd)).
  headOfRule(A) where
      (Lc,Hd,_) ^= isAssignment(A) => some((Lc,Hd)).
  headOfRule(A) where
      (Lc,Hd,_,_) ^= isEquation(A) => some((Lc,Hd)).
  headOfRule(_) default => none.

  public headName:(ast) => option[string].
  headName(A) where
      (_,Nm,_) ^= isRoundTerm(A) => headName(Nm).
  headName(A) where
      (_,Id) ^= isName(A) => some(Id).
  headName(A) where
      (_,D) ^= isDefault(A) =>
    headName(D).
  headName(_) default => none.

  public isDefn:(ast) => option[(locn,ast,ast)].
  isDefn(A) => isBinary(A,"=").

  public isAssignment:(ast) => option[(locn,ast,ast)].
  isAssignment(A) => isBinary(A,":=").

  public isEquation:(ast) => option[(locn,ast,ast,ast)].
  isEquation(A) where
      (Lc,L,Rhs) ^= isBinary(A,"=>") =>
    ((_,Lhs,Cond) ^= isWhere(L) ?
	some((Lc,Lhs,Cond,Rhs)) ||
	some((Lc,L,nme(Lc,"true"),Rhs))).
  isEquation(_) default => none.

  public isWhere:(ast) => option[(locn,ast,ast)].
  isWhere(A) => isBinary(A,"where").

  public isDefault:(ast) => option[(locn,ast)].
  isDefault(A) => isUnary(A,"default").


}
