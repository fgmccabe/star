star.compiler.wff{
  import star.
  import star.pkg.
  import star.topsort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.

  public genName:(locn,string) => ast.
  genName(Lc,V) => nme(Lc,genSym(V)).

  public isQuantified:(ast)=>option[(locn,list[ast],ast)].
  isQuantified(T) where
      (Lc,Lh,B)^=isBinary(T,"~~") && (_,V)^=isUnary(Lh,"all") =>
    some((Lc,deComma(V),B)).
  isQuantified(_) default => none.

  public reUQuant:(list[ast],ast) => ast.
  reUQuant([],T) => T.
  reUQuant([Q,..Qs],T) where Lc .= locOf(T) =>
    binary(Lc,"~~",unary(Lc,"all",reComma(Qs,Q)),T).

  public isXQuantified:(ast)=>option[(locn,list[ast],ast)].
  isXQuantified(T) where
      (Lc,Lh,B)^=isBinary(T,"~~") && (_,V)^=isUnary(Lh,"exists") =>
    some((Lc,deComma(V),B)).
  isXQuantified(_) default => none.

  public reXQuant:(list[ast],ast) => ast.
  reXQuant([],T) => T.
  reXQuant([Q,..Qs],T) where Lc .= locOf(T) =>
    binary(Lc,"~~",unary(Lc,"exists",reComma(Qs,Q)),T).

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

  public isTypeExists:(ast) => option[(locn,ast,ast)].
  isTypeExists(A) => isBinary(A,"<~").

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
      (Lc,H,I) ^= isBinary(A,"<~") &&
      (Q,T) .= getQuantifiers(H) =>
    some((Lc,Q,[],T,I)).
  isTypeExistsStmt(A) default => none.

  public isTypeFunStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isTypeFunStmt(A) where
      (Lc,Q,I) ^= isQuantified(A) &&
      (_,_,Cx,L,R) ^= isTypeFunStmt(I) => some((Lc,Q,Cx,L,R)).
  isTypeFunStmt(A) where
      (Lc,C,I) ^= isBinary(A,"|:") &&
      (_,Q,_,L,R) ^= isTypeFunStmt(I) => some((Lc,Q,deComma(C),L,R)).
  isTypeFunStmt(A) where
      (Lc,H,I) ^= isBinary(A,"~>") &&
      (Q,T) .= getQuantifiers(H) =>
    some((Lc,Q,[],T,I)).
  isTypeFunStmt(A) default => none.

  public isAlgebraicTypeStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isAlgebraicTypeStmt(A) where
      (Lc,Q,I) ^= isQuantified(A) &&
      (_,_,Cx,L,R) ^= isAlgebraicTypeStmt(I) => some((Lc,Q,Cx,L,R)).
  isAlgebraicTypeStmt(A) where
      (Lc,C,I) ^= isBinary(A,"|:") &&
      (_,Q,_,L,R) ^= isAlgebraicTypeStmt(I) => some((Lc,Q,deComma(C),L,R)).
  isAlgebraicTypeStmt(A) where
      (Lc,H,I) ^= isBinary(A,"::=") &&
      (Q,T) .= getQuantifiers(H) =>
    some((Lc,Q,[],T,I)).
  isAlgebraicTypeStmt(A) default => none.

  public reformAlgebraic:(locn,list[ast],list[ast],ast,ast,
    list[defnSpec],
    list[(defnSp,visibility)],
    list[(string,ast)],visibility,reports) =>
    either[reports,(list[defnSpec],
	list[(defnSp,visibility)],list[(string,ast)])].
  reformAlgebraic(Lc,Q,Cx,H,R,Defs,Pb,As,Vz,Rp) => do{
    Nm = typeName(H);
    Face <- algebraicFace(R,Rp);
    ExTp = reUQuant(Q,reConstrain(Cx,binary(Lc,"<~",H,Face)));
    Spec = tpSp(Nm);
    buildConstructors(R,Q,Cx,H,[defnSpec(Spec,Lc,[ExTp]),..Defs],[(Spec,Vz),..Pb],As,Vz,Rp)
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

  public isLetDef:(ast) => option[(locn,list[ast],ast)].
  isLetDef(A) where (Lc,Lh,Rh) ^= isBinary(A,"in") &&
      app(_,nme(_,"let"),Body) .= Lh &&
      (_,Els) ^= isBrTuple(Body) => some((Lc,Els,Rh)).
  isLetDef(_) default => none.

  public isQLetDef:(ast) => option[(locn,list[ast],ast)].
  isQLetDef(A) where (Lc,Lh,Rh) ^= isBinary(A,"in") &&
      app(_,nme(_,"let"),Body) .= Lh &&
      (_,Els) ^= isQBrTuple(Body) => some((Lc,Els,Rh)).
  isQLetDef(_) default => none.
  
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

  public isMatch(A) => isBinary(A,".=").

  public
  isSearch(A) where (Lc,P,G) ^= isBinary(A,"in") && \+ app(_,nme(_,"let"),Body) .= P => some((Lc,P,G)).
  isSearch(_) default => none.

  buildConstructors:(ast,
    list[ast],list[ast],ast,
    list[defnSpec],
    list[(defnSp,visibility)],
    list[(string,ast)],
    visibility,
    reports
  ) => either[reports,(list[defnSpec],
      list[(defnSp,visibility)],list[(string,ast)])].
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Vz,Rp) where
      (Lc,L,R) ^= isBinary(A,"|") => do{
	(Dfs1,Pb1,As1) <- buildConstructors(L,Qs,Cx,Tp,Defs,Pb,As,Vz,Rp);
	(Dfs2,Pb2,As2) <- buildConstructors(R,Qs,Cx,Tp,Dfs1,Pb1,As1,Vz,Rp);
	valis (Dfs2,Pb2,As2)
      }.
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) => let{
	Con = reUQuant(Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",reXQuant(XQs,
		reConstrain(XCx,brTuple(Lc,Els))),Tp))).
	Sp = cnsSp(Nm).
	Def = defnSpec(Sp,Lc,[Con]).
      } in either(([Def,..Defs],[(Sp,Vz),..Pb],[(Nm,Con),..As])).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isRoundCon(A) => let{
	Con = reUQuant(Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",rndTuple(Lc,Els),Tp))).
	Sp = cnsSp(Nm).
	Def = defnSpec(Sp,Lc,[Con]).
      } in either(([Def,..Defs],[(Sp,Vz),..Pb],[(Nm,Con),..As])).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Vz,Rp) where
      (Lc,Nm) ^= isName(A) => let{
	Con = reUQuant(Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",rndTuple(Lc,[]),Tp))).
	Sp = cnsSp(Nm).
	Def = defnSpec(Sp,Lc,[Con]).
      } in either(([Def,..Defs],[(Sp,Vz),..Pb],[(Nm,Con),..As])).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,_,Rp) where
      (_,I) ^= isPrivate(A) => 
    buildConstructors(I,Qs,Cx,Tp,Defs,Pb,As,priVate,Rp).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,_,Rp) where
      (_,I) ^= isPublic(A) => 
    buildConstructors(I,Qs,Cx,Tp,Defs,Pb,As,pUblic,Rp).
  buildConstructors(A,Qs,Cx,Tp,Defs,Pb,As,Vz,Rp) =>
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
      (_,Q,I) ^= isQuantified(T) => (Q,I).
  getQuantifiers(T) where
      (_,_,A) ^= isSquareTerm(T) => (A,T).
  getQuantifiers(T) default => ([],T).

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
  isImport(A) where (Lc,I) ^= isUnary(A,"import") && either(Pkg) .= pkgeName(I) =>
    some(pkgImp(Lc,priVate,Pkg)).
  isImport(_) default => none.

  public pkgeName:(ast) => either[(),pkg].
  pkgeName(A) where (_,L,R) ^= isBinary(A,"#") => do{
    Nm <- dottedName(L);
    Vr <- dottedName(R);
    valis pkg(Nm,vers(Vr))
  }
  pkgeName(A) => do{
    Nm <- dottedName(A);
    valis pkg(Nm,defltVersion)
  }

  dottedName:(ast) => either[(),string].
  dottedName(N) where (_,Id) ^= isName(N) => either(Id).
  dottedName(N) where (_,L,R) ^= isBinary(N,".") => do{
    LL <- dottedName(L);
    RR <- dottedName(R);
    valis "#(LL).#(RR)"
  }
  dottedName(_) default => other(()).

  public isOpen:(ast)=> option[(locn,ast)].
  isOpen(A) => isUnary(A,"open").

  public isIntegrity:(ast)=> option[(locn,ast)].
  isIntegrity(A) => isUnary(A,"assert").

  public isShow:(ast) => option[(locn,ast)].
  isShow(A) => isUnary(A,"show").

  public isTypeAnnotation:(ast) => option[(locn,ast,ast)].
  isTypeAnnotation(A) => isBinary(A,":").

  public isCoerce:(ast) => option[(locn,ast,ast)].
  isCoerce(A) => isBinary(A,"::").

  public isIndex:(ast) => option[(locn,ast,ast)].
  isIndex(A) where (Lc,Op,[Ix]) ^= isSquareTerm(A) && \+ _^=isBinary(Ix,":") => some((Lc,Op,Ix)).
  isIndex(A) where (Lc,L,R) ^= isBinary(A,"!") && (_,[Ix]) ^= isSqTuple(R) =>
    some((Lc,unary(Lc,"!",L),Ix)).
  isIndex(_) default => none.

  public isSlice:(ast) => option[(locn,ast,ast,ast)].
  isSlice(A) where (Lc,Op,[Ix]) ^= isSquareTerm(A) && (_,F,T) ^= isBinary(Ix,":") =>
    some((Lc,Op,F,T)).
  isSlice(_) default => none.

  public hasPromotion:(ast) => boolean.
  hasPromotion(A) where (_,_,Els) ^= isRoundTerm(A) =>
    E in Els && (_,_) ^= isUnary(E,"^").
  hasPromotion(_) default => false.

  public promoteOption:(ast) => ast.
  promoteOption(A) where (Lc,Op,Els) ^= isRoundTerm(A) => valof action{
    V = genName(Lc,"_V");
    (NEls,XV) = promoteArgs(Els,[],V);
    valis binary(Lc,">>=",XV,
      binary(Lc,"=>",rndTuple(Lc,[V]),roundTerm(Lc,Op,NEls)))
  }

  promoteArgs:(list[ast],list[ast],ast) => (list[ast],ast).
  promoteArgs([],Els,V) => (Els,V).
  promoteArgs([E,..Es],XEs,V) where (_,A) ^= isUnary(E,"^") =>
    ([XEs..,V]++Es,A).
  promoteArgs([E,..Es],XEs,V) => promoteArgs(Es,[XEs..,E],V).

  public isContractStmt:(ast) => option[(locn,ast,list[ast])].
  isContractStmt(A) where
      (Lc,I) ^= isUnary(A,"contract") &&
      (_,Lhs,B) ^= isBinary(I,"::=") &&
      (_,Els) ^= isBrTuple(B) => some((Lc,Lhs,Els)).
  isContractStmt(A) default => none.

  public isContractSpec:(ast) => option[(locn,string,list[ast],list[ast],ast)].
  isContractSpec(A) where
      (Lc,Quants,I) ^= isQuantified(A) &&
      (_,Nm,_,II,T) ^= isContractSpec(I) => some((Lc,Nm,Quants,II,T)).
  isContractSpec(A) where
      (Lc,Lhs,Rhs) ^= isBinary(A,"|:") &&
      (_,Nm,Q,II,T) ^= isContractSpec(Rhs) => some((Lc,Nm,Q,II++deComma(Lhs),T)).
  isContractSpec(A) where
      (Lc,Nm,Els) ^= isSquareTerm(A) &&
      (_,Id) ^= isName(Nm) => some((Lc,Id,Els,[],A)).
  isContractSpec(_) default => none.

  public isImplementationStmt:(ast) => option[(locn,list[ast],list[ast],ast,ast)].
  isImplementationStmt(A) where
      (Lc,I) ^= isUnary(A,"implementation") => isImplSpec(Lc,[],[],I).
  isImplementationStmt(_) default => none.

  isImplSpec(Lc,_,Cs,T) where
      (_,Qs,In) ^= isQuantified(T) =>
    isImplSpec(Lc,Qs,Cs,In).
  isImplSpec(Lc,Qs,_,T) where
      (_,Lhs,Rhs) ^= isBinary(T,"|:") =>
    isImplSpec(Lc,Qs,deComma(Lhs),Rhs).
  isImplSpec(_,Qs,Cs,T) where
      (Lc,Cn,Exp) ^= isBinary(T,"=>") =>
    some((Lc,Qs,Cs,Cn,Exp)).
  isImplSpec(_,_,_,_) default => none.

  public implementedContractName:(ast) => string.
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

  public typeName(Tp) where (_,Id) ^= isName(Tp) => Id.
  typeName(Tp) where (_,Id,_) ^= isSquareApply(Tp) => Id.
  typeName(Tp) where (_,Els) ^= isTuple(Tp) => "()$(size(Els))".

  public collectImports:(list[ast],
    list[importSpec],
    list[ast],
    reports) => either[reports,(list[importSpec],list[ast])].
  collectImports([],Imp,Oth,Rp) => either((Imp,Oth)).
  collectImports([A,..Ss],Imp,Oth,Rp) => do{
    if Spec ^= isImport(A) then{
      collectImports(Ss,[Imp..,Spec],Oth,Rp)
    } else{
      collectImports(Ss,Imp,[Oth..,A],Rp)
    }
  }

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
      (Lc,Hd,_) ^= isEquation(A) => some((Lc,Hd)).
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

  public isEquation:(ast) => option[(locn,ast,ast)].
  isEquation(A) => isBinary(A,"=>").

  public areEquations:(list[ast]) => boolean.
  areEquations(L) => E in L *> _ ^= isEquation(E).

  public isCaseExp:(ast) => option[(locn,ast,list[ast])].
  isCaseExp(A) where (Lc,L) ^= isUnary(A,"case") &&
      (_,Lhs,Rhs) ^= isBinary(L,"in") &&
      (_,Els) ^= isBrTuple(Rhs) => some((Lc,Lhs,Els)).
  isCaseExp(_) => none.

  public splitHead:(ast) => option[(string,ast,boolean)].
  splitHead(A) where (_,[I]) ^= isTuple(A) => splitHd(I,false).
  splitHead(A) => splitHd(A,false).

  splitHd(A,_) where (_,I) ^= isDefault(A) => splitHd(I,true).
  splitHd(A,D) where (Lc,Nm,As) ^= isRoundTerm(A) && (_,Id) ^= isName(Nm) => some((Id,rndTuple(Lc,As),D)).
  splitHd(A,D) where (Lc,Id) ^= isName(A) => some((Id,rndTuple(Lc,[]),D)).
  splitHd(A,D) where (Lc,L,C) ^= isWhere(A) && (Nm,Arg,Df) ^= splitHd(L,D) =>
    some((Nm,binary(Lc,"where",Arg,C),D)).
  
  public isWhere:(ast) => option[(locn,ast,ast)].
  isWhere(A) => isBinary(A,"where").

  public mkWhereEquality:(ast) =>ast.
  mkWhereEquality(Nm) where Lc.=locOf(Nm) && V.=genName(Lc,"_W") =>
    binary(Lc,"where",V,binary(Lc,"==",Nm,V)).
    
  public mkWhere:(locn,string) =>ast.
  mkWhere(Lc,Op) where V.=genName(Lc,"_W") =>
    binary(Lc,"where",V,unary(Lc,Op,V)).

  public mkWherePtn:(locn,ast,ast) => ast.
  mkWherePtn(Lc,Ptn,Op) where V.=genName(Lc,"_P") =>
    binary(Lc,"where",V,binary(Lc,".=",unary(Lc,"some",Ptn),roundTerm(Lc,Op,[V]))).

  public isOptionPtn:(ast) => option[(locn,ast,ast)].
  isOptionPtn(A) => isBinary(A,"^").
    
  public isDefault:(ast) => option[(locn,ast)].
  isDefault(A) => isUnary(A,"default").

  public isDoTerm:(ast) => option[(locn,ast)].
  isDoTerm(A) where (Lc,Op,Args) ^= isBrTerm(A) && (_,"do") ^= isName(Op) =>
    some((Lc,brTuple(Lc,Args))).
  isDoTerm(_) default => none.

  public isActionTerm:(ast) => option[(locn,ast)].
  isActionTerm(A) where (Lc,Op,Args) ^= isBrTerm(A) && (_,"action") ^= isName(Op) =>
    some((Lc,brTuple(Lc,Args))).
  isActionTerm(_) default => none.

  public isLazyTerm:(ast) => option[(locn,ast)].
  isLazyTerm(A) where (Lc,Op,Args) ^= isBrTerm(A) && (_,"lazy") ^= isName(Op) =>
    some((Lc,brTuple(Lc,Args))).
  isLazyTerm(_) default => none.

  public isTaskTerm:(ast) => option[(locn,ast)].
  isTaskTerm(A) where (Lc,Op,Args) ^= isBrTerm(A) && (_,"task") ^= isName(Op) =>
    some((Lc,brTuple(Lc,Args))).
  isTaskTerm(_) default => none.

  public isActionSeq:(ast) => option[(locn,ast,ast)].
  isActionSeq(A) => isBinary(A,";").

  public isBind:(ast) => option[(locn,ast,ast)].
  isBind(A) => isBinary(A,"<-").

  public isValis:(ast) => option[(locn,ast)].
  isValis(A) => isUnary(A,"valis").

  public isValof:(ast) => option[(locn,ast)].
  isValof(A) => isUnary(A,"valof").

  public isThrow:(ast) => option[(locn,ast)].
  isThrow(A) => isUnary(A,"throw").

  public isTryCatch:(ast) => option[(locn,ast,ast)].
  isTryCatch(A) where (Lc,I) ^= isUnary(A,"try") => isBinary(I,"catch").
  isTryCatch(_) default => none.

  public isIfThenElse:(ast) => option[(locn,ast,ast,ast)].
  isIfThenElse(A) where
      (Lc,Lhs,El) ^= isBinary(A,"else") &&
      (_,LL,Th) ^= isBinary(Lhs,"then") &&
      (_, Ts) ^= isUnary(LL,"if") => some((Lc,Ts,Th,El)).
  isIfThenElse(_) default => none.

  public isIfThen:(ast) => option[(locn,ast,ast)].
  isIfThen(A) where
      (Lc,LL,Th) ^= isBinary(A,"then") &&
      (_, Ts) ^= isUnary(LL,"if") => some((Lc,Ts,Th)).
  isIfThen(_) default => none.

  public isWhileDo:(ast) => option[(locn,ast,ast)].
  isWhileDo(A) where
      (Lc,LL,Bd) ^= isBinary(A,"do") &&
      (_, Ts) ^= isUnary(LL,"while") => some((Lc,Ts,Bd)).
  isWhileDo(_) default => none.

  public isForDo:(ast) => option[(locn,ast,ast)].
  isForDo(A) where
      (Lc,LL,Bd) ^= isBinary(A,"do") &&
      (_, Ts) ^= isUnary(LL,"for") => some((Lc,Ts,Bd)).
  isForDo(_) default => none.

  public isLCons:(ast) => option[(locn,ast,ast)].
  isLCons(A) => isBinary(A,"..,").

  public isCons:(ast) => option[(locn,ast,ast)].
  isCons(A) => isBinary(A,",..").
  
  public isComma:(ast) => option[(locn,ast,ast)].
  isComma(A) => isBinary(A,",").

  public isAbstraction:(ast) => option[(locn,ast,ast)].
  isAbstraction(A) where (Lc,[T]) ^= isSqTuple(A) &&
      (_,B,C) ^= isBinary(T,"|") => some((Lc,B,C)).
  isAbstraction(_) default => none.

  public isTheta:(ast) => option[(locn,list[ast])].
  isTheta(A) where (Lc,Els) ^= isBrTuple(A) && \+ _ ^= isAbstraction(A) =>
    some((Lc,Els)).
  isTheta(_) default => none.

  public isQTheta:(ast) => option[(locn,list[ast])].
  isQTheta(A) => isQBrTuple(A).

  public macroSquarePtn:(locn,list[ast]) => ast.
  macroSquarePtn(Lc,Els) =>
    macroListEntries(Lc,Els,(Lx)=>mkWhere(Lx,"_eof"),
      (Lx,H,T) => mkWherePtn(Lx,tpl(Lx,"()",[H,T]),nme(Lx,"_hdtl")),
      (Lx,L,R) => mkWherePtn(Lx,tpl(Lx,"()",[L,R]),nme(Lx,"_back"))).

  public macroSquareExp:(locn,list[ast]) => ast.
  macroSquareExp(Lc,Els) =>
    macroListEntries(Lc,Els,(Lx)=>nme(Lx,"_nil"),
      (Lx,H,T) => binary(Lx,"_cons",H,T),
      (Lx,L,R) => binary(Lx,"_apnd",L,R)).

  macroListEntries:(locn,list[ast],(locn)=>ast,(locn,ast,ast)=>ast,(locn,ast,ast)=>ast) => ast.
  macroListEntries(Lc,[],End,_,_) => End(Lc).
  macroListEntries(_,[Cns],_,_,Tail) where (Lc,H,T) ^= isLCons(Cns) =>
    macroLList(Lc,T,H,Tail).
  macroListEntries(_,[Cns],_,Hed,_) where (Lc,H,T) ^= isCons(Cns) =>
    Hed(Lc,H,T).
  macroListEntries(Lc,[El,..Rest],Eof,Hed,Tail) =>
    Hed(Lc,El,macroListEntries(Lc,Rest,Eof,Hed,Tail)).

  macroLList(_,T,S,Tail) where (Lc,H,Tl) ^= isComma(T) =>
    macroLList(Lc,Tl,Tail(Lc,S,H),Tail).
  macroLList(Lc,T,S,Tail) => Tail(Lc,S,T).

}
