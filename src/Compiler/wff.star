star.compiler.wff{
  import star.
  import star.pkg.
  import star.topsort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.operators.

  public isName:(ast) => option[(locn,string)].
  isName(nme(Lc,Id)) where ~ keyword(Id) => some((Lc,Id)).
  isName(qnm(Lc,Id)) => some((Lc,Id)).
  isName(tpl(_,"()",[nme(Lc,Id)])) => some((Lc,Id)).
  isName(tpl(_,"()",[qnm(Lc,Id)])) => some((Lc,Id)).
  isName(_) default => .none.

  public isKeyword:(ast) => option[(locn,string)].
  isKeyword(nme(Lc,Id)) where keyword(Id) => some((Lc,Id)).
  isKeyword(_) default => .none.
  
  public isLitAst:(ast) => boolean.
  isLitAst(int(_,_)) => .true.
  isLitAst(num(_,_)) => .true.
  isLitAst(str(_,_)) => .true.
  isLitAst(_) default => .false.

  public isSquareTerm:(ast) => option[(locn,ast,cons[ast])].
  isSquareTerm(app(Lc,Op,tpl(_,"[]",A))) => some((Lc,Op,A)).
  isSquareTerm(_) default => .none.

  public squareTerm:(locn,ast,cons[ast])=>ast.
  squareTerm(Lc,Op,Args) => app(Lc,Op,tpl(Lc,"[]",Args)).
  
  public isSquareApply:(ast) => option[(locn,string,cons[ast])].
  isSquareApply(app(Lc,Op,tpl(_,"[]",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isSquareApply(_) default => .none.

  public qBrTuple:(locn,cons[ast]) => ast.
  qBrTuple(Lc,Els) => tpl(Lc,"{..}",Els).

  public isBrTerm:(ast) => option[(locn,ast,cons[ast])].
  isBrTerm(app(Lc,Op,tpl(_,"{}",A))) => some((Lc,Op,A)).
  isBrTerm(_) default => .none.

  public mkBrTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"{}",Els)).

  public isBrApply:(ast,string) => option[(locn,cons[ast])].
  isBrApply(app(Lc,Op,tpl(_,"{}",A)),Id) where
      (_,Id) ^= isName(Op) => some((Lc,A)).
  isBrApply(_,_) default => .none.

  public isQBrTerm:(ast) => option[(locn,ast,cons[ast])].
  isQBrTerm(app(Lc,Op,tpl(_,"{..}",A))) => some((Lc,Op,A)).
  isQBrTerm(_) default => .none.

  public mkQBrTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"{..}",Els)).

  public isQBrApply:(ast) => option[(locn,string,cons[ast])].
  isQBrApply(app(Lc,Op,tpl(_,"{..}",A))) where
      (_,Id) ^= isName(Op) => some((Lc,Id,A)).
  isQBrApply(_) default => .none.

  public isRoundTerm:(ast) => option[(locn,ast,cons[ast])].
  isRoundTerm(app(Lc,Op,tpl(_,"()",A))) where ~_^=isKeyword(Op) => some((Lc,Op,A)).
  isRoundTerm(_) default => .none.

  public roundTerm:(locn,ast,cons[ast]) => ast.
  roundTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"()",Els)).

  public braceTerm:(locn,ast,cons[ast]) => ast.
  braceTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"{}",Els)).

  public qbraceTerm(Lc,Op,Els) => app(Lc,Op,tpl(Lc,"{..}",Els)).

  public isQuantified:(ast)=>option[(locn,cons[ast],ast)].
  isQuantified(T) where
      (Lc,Lh,B)^=isBinary(T,"~~") && (_,V)^=isUnary(Lh,"all") =>
    some((Lc,deComma(V),B)).
  isQuantified(_) default => .none.

  public reUQuant:(locn,cons[ast],ast) => ast.
  reUQuant(_,[],T) => T.
  reUQuant(Lc,[Q,..Qs],T) =>
    binary(Lc,"~~",unary(Lc,"all",reComma([Q,..Qs])),T).

  public isXQuantified:(ast)=>option[(locn,cons[ast],ast)].
  isXQuantified(T) where
      (Lc,Lh,B)^=isBinary(T,"~~") && (_,V)^=isUnary(Lh,"exists") =>
    some((Lc,deComma(V),B)).
  isXQuantified(_) default => .none.

  public reXQuant:(locn,cons[ast],ast) => ast.
  reXQuant(_,[],T) => T.
  reXQuant(Lc,[Q,..Qs],T) =>
    binary(Lc,"~~",unary(Lc,"exists",reComma([Q,..Qs])),T).

  public isConstrained:(ast) => option[(locn,cons[ast],ast)].
  isConstrained(T) where
      (Lc,Lh,B) ^= isBinary(T,"|:") => some((Lc,deComma(Lh),B)).
  isConstrained(_) default => .none.

  public reConstrain:(cons[ast],ast) => ast.
  reConstrain([],T) => T.
  reConstrain([C,..Cs],T) => binary(locOf(T),"|:",reComma([C,..Cs]),T).

  public isConstructorType:(ast) => option[(locn,ast,ast)].
  isConstructorType(A) => isBinary(A,"<=>").

  public isFunctionType:(ast) => option[(locn,ast,ast)].
  isFunctionType(A) => isBinary(A,"=>").

  public isConstructorStmt(A) where (_,_,I) ^= isQuantified(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,_,I) ^= isXQuantified(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,I) ^= isPrivate(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,I) ^= isPublic(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) => _ ^= isBinary(A,"<=>").

  public mkConstructorType(Lc,L,R) => binary(Lc,"<=>",L,R).

  public mkFunctionType(Lc,L,R) => binary(Lc,"=>",L,R).
  
  public deComma:(ast) => cons[ast].
  deComma(Trm) => let{
    deC(T,SoF) where (_,Lh,Rh)^=isBinary(T,",") =>
      deC(Rh,[Lh,..SoF]).
    deC(T,SoF) => reverse([T,..SoF]).
  } in deC(Trm,[]).

  public reComma:(cons[ast]) => ast.
  reComma([A]) => A.
  reComma([A,..As]) =>
    binary(locOf(A),",",A,reComma(As)).

  public isDepends:(ast) => option[(cons[ast],cons[ast])].
  isDepends(T) where (_,Lh,Rh)^=isBinary(T,"->>") =>
    some((deComma(Lh),deComma(Rh))).
  isDepends(_) default => .none.

  public isAnnotation:(ast) => option[(locn,ast,ast)].
  isAnnotation(A) where (Lc,L,R) ^= isBinary(A,"@") =>
    some((Lc,L,R)).
  isAnnotation(A) where (Lc,Rh) ^= isUnary(A,"@") => some((Lc,str(Lc,""),Rh)).
  isAnnotation(_) default => .none.

  public isTypeLambda:(ast) => option[(locn,ast,ast)].
  isTypeLambda(A) => isBinary(A,"~>").

  public mkTypeLambda(Lc,L,R) => binary(Lc,"~>",L,R).

  public isTypeExists:(ast) => option[(locn,ast,ast)].
  isTypeExists(A) => isBinary(A,"<~").

  public mkTypeExists(Lc,L,R) => binary(Lc,"<~",L,R).

  public isTypeAnnotation:(ast)=>option[(locn,ast,ast)].
  isTypeAnnotation(A) => isBinary(A,":").

  public typeAnnotation:(locn,ast,ast)=>ast.
  typeAnnotation(Lc,V,T) => binary(Lc,":",V,T).

  public isTypeExistsStmt:(ast) => option[(locn,cons[ast],cons[ast],ast,ast)].
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
  isTypeExistsStmt(A) default => .none.

  public isTypeFunStmt:(ast) => option[(locn,cons[ast],cons[ast],ast,ast)].
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
  isTypeFunStmt(A) default => .none.

  public isTypeStatement:(ast) => option[(locn,ast,ast)].
  isTypeStatement(A) where (Lc,R) ^= isUnary(A,"type") &&
      (_,V,T) ^= isTypeAnnotation(R) => some((Lc,V,T)).
  isTypeStatement(_) default => .none.

  public mkTypeStatement(Lc,V,T) => unary(Lc,"type",typeAnnotation(Lc,V,T)).

  public isAlgebraicTypeStmt:(ast) => 
    option[(locn,visibility,cons[ast],cons[ast],ast,ast)].
  isAlgebraicTypeStmt(A) => isAlgebraic(A,.deFault).

  isAlgebraic:(ast,visibility) =>
    option[(locn,visibility,cons[ast],cons[ast],ast,ast)].
  isAlgebraic(A,Vz) where
      (Lc,Q,I) ^= isQuantified(A) &&
      (_,Viz,_,Cx,L,R) ^= isAlgebraic(I,Vz) => some((Lc,Viz,Q,Cx,L,R)).
  isAlgebraic(A,Vz) where
      (Lc,C,I) ^= isBinary(A,"|:") &&
      (_,Viz,Q,_,L,R) ^= isAlgebraic(I,Vz) => some((Lc,Viz,Q,deComma(C),L,R)).
  isAlgebraic(A,Vz) where
      (Lc,H,I) ^= isBinary(A,"::=") &&
      (Q,T) .= getQuantifiers(H) =>
    some((Lc,Vz,Q,[],T,I)).
  isAlgebraic(A,Vz) where (_,I) ^= isPrivate(A) =>
    isAlgebraic(I,.priVate).
  isAlgebraic(A,Vz) where (_,I) ^= isPublic(A) =>
    isAlgebraic(I,.pUblic).
  isAlgebraic(A,_) default => .none.

  public isLetDef:(ast) => option[(locn,cons[ast],ast)].
  isLetDef(A) where (Lc,Lh,Rh) ^= isBinary(A,"in") &&
      app(_,nme(_,"let"),Body) .= Lh &&
      (_,Els) ^= isBrTuple(Body) => some((Lc,Els,Rh)).
  isLetDef(_) default => .none.

  public mkLetDef(Lc,Els,Bnd) =>
    binary(Lc,"in",braceTerm(Lc,nme(Lc,"let"),Els),Bnd).

  public isQLetDef:(ast) => option[(locn,cons[ast],ast)].
  isQLetDef(A) where (Lc,Lh,Rh) ^= isBinary(A,"in") &&
      app(_,nme(_,"let"),Body) .= Lh &&
      (_,Els) ^= isQBrTuple(Body) => some((Lc,Els,Rh)).
  isQLetDef(_) default => .none.

  public mkQLetDef:(locn,cons[ast],ast) => ast.
  mkQLetDef(Lc,Els,Bnd) =>
    binary(Lc,"in",qbraceTerm(Lc,nme(Lc,"let"),Els),Bnd).
  
  public isComprehension:(ast) => option[(locn,ast,ast)].
  isComprehension(A) where (Lc,[T]) ^= isBrTuple(A) &&
      (_,Bnd,Body) ^= isBinary(T,"|") => some((Lc,Bnd,Body)).
  isComprehension(A) => .none.

  public isListComprehension:(ast) => option[(locn,ast,ast)].
  isListComprehension(A) where (Lc,[T]) ^= isSqTuple(A) &&
      (_,Bnd,Body) ^= isBinary(T,"|") => some((Lc,Bnd,Body)).
  isListComprehension(A) => .none.

  public isConjunct(A) => isBinary(A,"&&").

  public mkConjunct(Lc,L,R) => binary(Lc,"&&",L,R).

  public isDisjunct(A) => isBinary(A,"||").

  public mkDisjunct(Lc,L,R) => binary(Lc,"||",L,R).

  public isNegation(A) => isUnary(A,"~").

  public negated(Lc,A) => unary(Lc,"~",A).

  public isImplies(A) => isBinary(A,"*>").
  
  public mkImplies(Lc,L,R) => binary(Lc,"*>",L,R).

  public isConditional(A) where
      (Lc,Tst,Rhs) ^= isBinary(A,"?") &&
      (_,Th,El) ^= isBinary(Rhs,"||") => some((Lc,Tst,Th,El)).
  isConditional(_) => .none.

  public mkConditional:(locn,ast,ast,ast) => ast.
  mkConditional(Lc,T,Th,El) =>
    binary(Lc,"?",T,binary(Lc,"||",Th,El)).

  public isMatch(A) => isBinary(A,".=").

  public mkMatch(Lc,L,R) => binary(Lc,".=",L,R).

  public isOptionMatch(A) => isBinary(A,"^=").

  public isEnum(A) => isUnary(A,".").

  public isEnumSymb(A) where (Lc,nme(_,N))^=isUnary(A,".") => some((Lc,N)).
  isEnumSymb(_) default => .none.

  public enum(Lc,Nm) => unary(Lc,".",nme(Lc,Nm)).

  public
  isSearch(A) where (Lc,P,G) ^= isBinary(A,"in") && ~ app(_,nme(_,"let"),Body) .= P => some((Lc,P,G)).
  isSearch(_) default => .none.

  public mkSearch(Lc,P,S) => binary(Lc,"in",P,S).

  getQuantifiers(T) where
      (_,Q,I) ^= isQuantified(T) => (Q,I).
  getQuantifiers(T) where
      (_,_,A) ^= isSquareTerm(T) => (A,T).
  getQuantifiers(T) default => ([],T).

  public isFieldAcc:(ast) => option[(locn,ast,ast)].
  isFieldAcc(A) => isBinary(A,".").

  public mkFieldAcc(Lc,R,F) => binary(Lc,".",R,F).

  public isPublic:(ast) => option[(locn,ast)].
  isPublic(A) => isUnary(A,"public").

  public isPrivate:(ast) => option[(locn,ast)].
  isPrivate(A) => isUnary(A,"private").

  public isImport:(ast)=> option[importSpec].
  isImport(A) where (Lc,I) ^= isPublic(A) =>
    (pkgImp(_,_,Im) ^= isImport(I) ?
	some(pkgImp(Lc,.pUblic,Im)) ||
	.none).
  isImport(A) where (Lc,I) ^= isPrivate(A) =>
    (pkgImp(_,_,Im) ^= isImport(I) ?
	some(pkgImp(Lc,.priVate,Im)) ||
	.none).
  isImport(A) where (Lc,I) ^= isUnary(A,"import") => some(pkgImp(Lc,.priVate,pkgeName(I))).
  isImport(_) default => .none.

  public pkgeName:(ast) => pkg.
  pkgeName(A) where (_,L,R) ^= isBinary(A,"#") => 
    pkg(dottedName(L),vers(dottedName(R))).
  pkgeName(A) => pkg(dottedName(A),.defltVersion).

  dottedName:(ast) => string.
  dottedName(N) where (_,Id) ^= isName(N) => Id.
  dottedName(N) where (_,L,R) ^= isBinary(N,".") => "#(dottedName(L)).#(dottedName(R))".
  dottedName(A) default => disp(A).

  public isOpen:(ast)=> option[(locn,ast)].
  isOpen(A) => isUnary(A,"open").

  public isIntegrity:(ast)=> option[(locn,ast)].
  isIntegrity(A) => isUnary(A,"assert").

  public isShow:(ast) => option[(locn,ast)].
  isShow(A) => isUnary(A,"show").

  public isCoerce:(ast) => option[(locn,ast,ast)].
  isCoerce(A) => isBinary(A,"::").

  public mkCoercion(Lc,L,R) => binary(Lc,"::",L,R).

  public isOptCoerce:(ast) => option[(locn,ast,ast)].
  isOptCoerce(A) => isBinary(A,":?").

  public mkOptCoercion(Lc,L,R) => binary(Lc,":?",L,R).

  public isRef:(ast) => option[(locn,ast)].
  isRef(A) => isUnary(A,"ref").

  public mkRef(Lc,A) => unary(Lc,"ref",A).
  
  public isCellRef:(ast) => option[(locn,ast)].
  isCellRef(A) => isUnary(A,"!").

  public refCell:(locn,ast) => ast.
  refCell(Lc,I) => unary(Lc,"!",I).

  public isMemo(A) => isUnary(A,"$$").

  public isFetch(A) => isUnary(A,"!!").

  public isIndexTerm:(ast) => option[(locn,ast,ast)].
  isIndexTerm(A) where (Lc,Op,[Ix]) ^= isSquareTerm(A) &&
      ~ _^=isBinary(Ix,":") => some((Lc,Op,Ix)).
  isIndexTerm(A) where (Lc,L,R) ^= isBinary(A,"!") && (_,[Ix]) ^= isSqTuple(R) =>
    some((Lc,unary(Lc,"!",L),Ix)).
  isIndexTerm(_) default => .none.

  public isSlice:(ast) => option[(locn,ast,ast,ast)].
  isSlice(A) where (Lc,Op,[Ix]) ^= isSquareTerm(A) && (_,F,T) ^= isBinary(Ix,":") =>
    some((Lc,Op,F,T)).
  isSlice(_) default => .none.

  public isSplice:(ast) => option[(locn,ast,ast,ast,ast)]. -- S[F:T] := R
  isSplice(A) where (Lc,Lhs,R)^= isAssignment(A) && (_,S,F,T) ^= isSlice(Lhs) =>
    some((Lc,S,F,T,R)).
  isSplice(_) default => .none.

  public hasPromotion:(ast) => boolean.
  hasPromotion(A) where (_,_,Els) ^= isRoundTerm(A) =>
    {? E in Els && (_,_) ^= isUnary(E,"^") ?}.
  hasPromotion(_) default => .false.

  public isPromotion:(ast) => option[(locn,ast)].
  isPromotion(A) => isUnary(A,"^").

  public mkPromotion(Lc,A) => unary(Lc,"^",A).

  public promoteOption:(ast) => ast.
  promoteOption(A) where (Lc,Op,Els) ^= isRoundTerm(A) => valof action{
    V .= genName(Lc,"_V");
    (NEls,XV) .= promoteArgs(Els,[],V);
    valis binary(Lc,">>=",XV,
      binary(Lc,"=>",rndTuple(Lc,[V]),roundTerm(Lc,Op,NEls)))
  }

  promoteArgs:(cons[ast],cons[ast],ast) => (cons[ast],ast).
  promoteArgs([],Els,V) => (reverse(Els),V).
  promoteArgs([E,..Es],XEs,V) where (_,A) ^= isUnary(E,"^") =>
    ([V,..XEs]++Es,A).
  promoteArgs([E,..Es],XEs,V) => promoteArgs(Es,[E,..XEs],V).

  public isContractStmt:(ast) => option[(locn,ast,cons[ast])].
  isContractStmt(A) where
      (Lc,I) ^= isUnary(A,"contract") &&
      (_,Lhs,B) ^= isBinary(I,"::=") &&
      (_,Els) ^= isBrTuple(B) => some((Lc,Lhs,Els)).
  isContractStmt(A) default => .none.

  public mkContractStmt(Lc,T,Els) =>
    unary(Lc,"contract",binary(Lc,"::=",T,brTuple(Lc,Els))).

  public isContractSpec:(ast) => option[(locn,string,cons[ast],cons[ast],ast)].
  isContractSpec(A) where
      (Lc,Quants,I) ^= isQuantified(A) &&
      (_,Nm,_,II,T) ^= isContractSpec(I) => some((Lc,Nm,Quants,II,T)).
  isContractSpec(A) where
      (Lc,Lhs,Rhs) ^= isBinary(A,"|:") &&
      (_,Nm,Q,II,T) ^= isContractSpec(Rhs) => some((Lc,Nm,Q,II++deComma(Lhs),T)).
  isContractSpec(A) where
      (Lc,Nm,Els) ^= isSquareTerm(A) &&
      (_,Id) ^= isName(Nm) => some((Lc,Id,Els,[],A)).
  isContractSpec(_) default => .none.

  public isImplementationStmt:(ast) => option[(locn,cons[ast],cons[ast],ast,ast)].
  isImplementationStmt(A) where
      (Lc,I) ^= isUnary(A,"implementation") => isImplSpec(Lc,[],[],I).
  isImplementationStmt(_) default => .none.

  isImplSpec(Lc,_,Cs,T) where
      (_,Qs,In) ^= isQuantified(T) =>
    isImplSpec(Lc,Qs,Cs,In).
  isImplSpec(Lc,Qs,_,T) where
      (_,Lhs,Rhs) ^= isBinary(T,"|:") =>
    isImplSpec(Lc,Qs,deComma(Lhs),Rhs).
  isImplSpec(_,Qs,Cs,T) where
      (Lc,Cn,Exp) ^= isBinary(T,"=>") =>
    some((Lc,Qs,Cs,Cn,Exp)).
  isImplSpec(_,_,_,_) default => .none.

  public implementedContractName:(ast) => string.
  implementedContractName(A) where
      (_,O,As) ^= isSquareApply(A) => "#(O)#(surfaceNames(As,markerString(.overMark))*)".

  surfaceNames([],_) => [].
  surfaceNames([T,.._],Sep) where (_,L,_) ^= isBinary(T,"->>") =>
    surfaceNames(deComma(L),Sep).
  surfaceNames([T,..Ts],Sep) =>
    [Sep,surfaceName(T),..surfaceNames(Ts,Sep)].

  surfaceName(T) where (_,Id) ^= isName(T) => Id.
  surfaceName(T) where (_,Id,_) ^= isSquareApply(T) => Id.
  surfaceName(T) where (_,_,I) ^= isQuantified(T) => surfaceName(I).
  surfaceName(T) where (_,Els) ^= isTuple(T) => "()$(size(Els))".

  public mkImplementationStmt:(locn,cons[ast],cons[ast],ast,ast) => ast.
  mkImplementationStmt(Lc,Q,Cx,T,E) =>
    unary(Lc,"implementation",reUQuant(Lc,Q,reConstrain(Cx,binary(Lc,"=>",T,E)))).

  public typeName:(ast)=>string.
  typeName(Tp) where (_,Id) ^= isName(Tp) => Id.
  typeName(Tp) where (_,Id,_) ^= isSquareApply(Tp) => Id.
  typeName(Tp) where (_,Els) ^= isTuple(Tp) => "()$(size(Els))".

  public collectImports:(cons[ast], cons[importSpec], cons[ast]) => (cons[importSpec],cons[ast]).
  collectImports([],Imp,Oth) => (Imp,reverse(Oth)).
  collectImports([A,..Ss],Imp,Oth) => (
    Spec ^= isImport(A) ?
      collectImports(Ss,[Spec,..Imp],Oth) ||
      collectImports(Ss,Imp,[A,..Oth])).

  public ruleName:(ast) => option[(locn,string)].
  ruleName(A) where (_,some(Nm),_,_,_,_) ^= isEquation(A) && (Lc,Id)^=isName(Nm) => some((Lc,Id)).
  ruleName(_) default => .none.

  public headName:(ast) => option[string].
  headName(A) where
      (_,Nm,_) ^= isRoundTerm(A) => headName(Nm).
  headName(A) where
      (_,Id) ^= isName(A) => some(Id).
  headName(A) where
      (_,D) ^= isDefault(A) => headName(D).
  headName(A) where (_,H,_) ^= isWhere(A) => headName(H).
  headName(A) where (_,[E]) ^= isTuple(A) => headName(E).
  headName(_) default => .none.

  public isDefn:(ast) => option[(locn,ast,ast)].
  isDefn(A) => isBinary(A,"=").

  public mkDefn(Lc,L,R) => binary(Lc,"=",L,R).

  public isAssignment:(ast) => option[(locn,ast,ast)].
  isAssignment(A) => isBinary(A,":=").

  public mkAssignment(Lc,L,R) => binary(Lc,":=",L,R).

  public isEquation:(ast) => option[(locn,option[ast],boolean,ast,option[ast],ast)].
  isEquation(A) where (Lc,L,R) ^= isBinary(A,"=>") &&
      (N,H,C,D) ^= splitHead(L,.none,.none,.false) => some((Lc,N,D,H,C,R)).
  isEquation(_) default => .none.

  public mkEquation:(locn,option[ast],boolean,ast,option[ast],ast)=>ast.
  mkEquation(Lc,Nm,Deflt,Args,Cond,Rep) =>
    binary(Lc,"=>",mkLhs(Lc,Nm,Deflt,Args,Cond),Rep).

  mkHed(Lc,some(Nm),Args) => app(Lc,Nm,Args).
  mkHed(_,.none,Args) => Args.

  mkLhs(Lc,Nm,.true,Args,.none) => unary(Lc,"default",mkHed(Lc,Nm,Args)).
  mkLhs(Lc,Nm,.true,Args,some(C)) =>
    unary(Lc,"default",binary(Lc,"where",mkHed(Lc,Nm,Args),C)).
  mkLhs(Lc,Nm,.false,Args,.none) => mkHed(Lc,Nm,Args).
  mkLhs(Lc,Nm,.false,Args,some(C)) => binary(Lc,"where",mkHed(Lc,Nm,Args),C).

  splitHead:(ast,option[ast],option[ast],boolean) => option[(option[ast],ast,option[ast],boolean)].
  splitHead(A,N,C,_) where (_,I) ^= isDefault(A) => splitHead(I,N,C,.true).
  splitHead(A,_,C,D) where (Lc,Nm,As) ^= isRoundTerm(A) => some((some(Nm),rndTuple(Lc,As),C,D)).
  splitHead(A,N,C,D) where (Lc,[E]) ^= isTuple(A) => splitHead(E,N,C,D).
  splitHead(A,N,_,D) where (Lc,L,C) ^= isWhere(A) => splitHead(L,N,some(C),D).
  splitHead(A,N,C,D) => some((N,A,C,D)).

  splitHd:(ast,option[ast],option[ast],boolean) => option[(option[ast],ast,option[ast],boolean)].
  splitHd(A,N,C,_) where (_,I) ^= isDefault(A) => splitHd(I,N,C,.true).
  splitHd(A,_,C,D) where (Lc,Nm,As) ^= isRoundTerm(A) => some((some(Nm),rndTuple(Lc,As),C,D)).
  splitHd(A,N,_,D) where (Lc,L,C) ^= isWhere(A) => splitHd(L,N,some(C),D).
  splitHd(A,N,C,D) => some((N,A,C,D)).

  public isLambda:(ast) => option[(locn,boolean,ast,option[ast],ast)].
  isLambda(A) where (Lc,L,R) ^= isBinary(A,"=>") &&
      (H,C,D) ^= splitLHead(L,.none,.false) => some((Lc,D,H,C,R)).
  isLambda(_) default => .none.

  splitLHead:(ast,option[ast],boolean) => option[(ast,option[ast],boolean)].
  splitLHead(A,C,_) where (_,I) ^= isDefault(A) => splitLHead(I,C,.true).
  splitLHead(A,_,D) where (Lc,L,C) ^= isWhere(A) => splitLHead(L,some(C),D).
  splitLHead(A,C,D) => some((A,C,D)).

  public mkLambda:(locn,boolean,ast,option[ast],ast) => ast.
  mkLambda(Lc,Deflt,Arg,Cond,Rep) =>
    binary(Lc,"=>",mkLambdaHead(Lc,Arg,Deflt,Cond),Rep).

  mkLambdaHead(Lc,Arg,.true,.none) => unary(Lc,"default",Arg).
  mkLambdaHead(Lc,Arg,.false,some(C)) => binary(Lc,"where",Arg,C).
  mkLambdaHead(Lc,Arg,.false,.none) => Arg.

  public areEquations:(cons[ast]) => boolean.
  areEquations(L) => {? E in L *> _ ^= isEquation(E) ?}.

  public equation:(locn,ast,ast)=>ast.
  equation(Lc,Hd,Rep) => binary(Lc,"=>",Hd,Rep).

  public isCase:(ast) => option[(locn,ast,cons[ast])].
  isCase(A) where (Lc,L) ^= isUnary(A,"case") &&
      (_,Lhs,Rhs) ^= isBinary(L,"in") &&
      (_,Els) ^= isBrTuple(Rhs) => some((Lc,Lhs,Els)).
  isCase(_) => .none.

  public mkCaseExp(Lc,E,C) =>
    unary(Lc,"case",binary(Lc,"in",E,brTuple(Lc,C))).
  
  public isWhere:(ast) => option[(locn,ast,ast)].
  isWhere(A) => isBinary(A,"where").

  public mkWhere(Lc,L,R) => binary(Lc,"where",L,R).

  public mkWhereEquality:(ast) =>ast.
  mkWhereEquality(Nm) where Lc.=locOf(Nm) && V.=genName(Lc,"_W") =>
    binary(Lc,"where",V,binary(Lc,"==",Nm,V)).
    
  public mkWhereTest:(locn,string) =>ast.
  mkWhereTest(Lc,Op) where V.=genName(Lc,"_W") =>
    binary(Lc,"where",V,unary(Lc,Op,V)).

  public mkWherePtn:(locn,ast,ast) => ast.
  mkWherePtn(Lc,Ptn,Op) where V.=genName(Lc,"_P") =>
    binary(Lc,"where",V,binary(Lc,".=",unary(Lc,"some",Ptn),roundTerm(Lc,Op,[V]))).

  public isOptionPtn:(ast) => option[(locn,ast,ast)].
  isOptionPtn(A) => isBinary(A,"^").
    
  public isDefault:(ast) => option[(locn,ast)].
  isDefault(A) => isUnary(A,"default").

  public isDoTerm:(ast) => option[(locn,ast)].
  isDoTerm(A) where (Lc,Arg) ^= isUnary(A,"do") && _ ^= isBrTuple(Arg) =>
    some((Lc,Arg)).
  isDoTerm(_) default => .none.

  public isActionTerm:(ast) => option[(locn,ast)].
  isActionTerm(A) where (Lc,Args) ^= isBrApply(A,"action") =>
    some((Lc,brTuple(Lc,Args))).
  isActionTerm(_) default => .none.

  public isLazyTerm:(ast) => option[(locn,ast)].
  isLazyTerm(A) where (Lc,Args) ^= isBrApply(A,"lazy") =>
    some((Lc,brTuple(Lc,Args))).
  isLazyTerm(_) default => .none.

  public isTaskTerm:(ast) => option[(locn,ast)].
  isTaskTerm(A) where (Lc,Args) ^= isBrApply(A,"task") =>
    some((Lc,brTuple(Lc,Args))).
  isTaskTerm(_) default => .none.

  public isActionSeq:(ast) => option[(locn,ast,ast)].
  isActionSeq(A) => isBinary(A,";").

  public actionSeq(Lc,L,R) => binary(Lc,";",L,R).

  public isBind:(ast) => option[(locn,ast,ast)].
  isBind(A) => isBinary(A,"<-").

  public isValis:(ast) => option[(locn,ast)].
  isValis(A) => isUnary(A,"valis").

  public isValof:(ast) => option[(locn,ast)].
  isValof(A) => isUnary(A,"valof").

  public mkValof(Lc,I) => unary(Lc,"valof",I).

  public isThrow:(ast) => option[(locn,ast)].
  isThrow(A) => isUnary(A,"throw").

  public isRaise:(ast) => option[(locn,ast)].
  isRaise(A) => isUnary(A,"raise").

  public isPerform:(ast) => option[(locn,ast)].
  isPerform(A) => isUnary(A,"perform").

  public isTryCatch:(ast) => option[(locn,ast,ast)].
  isTryCatch(A) where (Lc,I) ^= isUnary(A,"try") => isBinary(I,"catch").
  isTryCatch(_) default => .none.

  public isIfThenElse:(ast) => option[(locn,ast,ast,ast)].
  isIfThenElse(A) where
      (Lc,Lhs,El) ^= isBinary(A,"else") &&
      (_,LL,Th) ^= isBinary(Lhs,"then") &&
      (_, Ts) ^= isUnary(LL,"if") => some((Lc,Ts,Th,El)).
  isIfThenElse(_) default => .none.

  public mkIfThenElse(Lc,T,Th,El) =>
    binary(Lc,"else",binary(Lc,"then",unary(Lc,"if",T),Th),El).

  public isIfThen:(ast) => option[(locn,ast,ast)].
  isIfThen(A) where
      (Lc,LL,Th) ^= isBinary(A,"then") &&
      (_, Ts) ^= isUnary(LL,"if") => some((Lc,Ts,Th)).
  isIfThen(_) default => .none.

  public mkIfThen(Lc,T,Th) =>
    binary(Lc,"then",unary(Lc,"if",T),Th).

  public isWhileDo:(ast) => option[(locn,ast,ast)].
  isWhileDo(A) where
      (Lc,LL,Bd) ^= isBinary(A,"do") &&
      (_, Ts) ^= isUnary(LL,"while") => some((Lc,Ts,Bd)).
  isWhileDo(_) default => .none.

  public isForDo:(ast) => option[(locn,ast,ast)].
  isForDo(A) where
      (Lc,LL,Bd) ^= isBinary(A,"do") &&
      (_, Ts) ^= isUnary(LL,"for") => some((Lc,Ts,Bd)).
  isForDo(_) default => .none.

  public isIgnore:(ast) => option[(locn,ast)].
  isIgnore(A) => isUnary(A,"ignore").

  public isCons:(ast) => option[(locn,ast,ast)].
  isCons(A) => isBinary(A,",..").
  
  public mkCons(Lc,L,R) => binary(Lc,",..",L,R).
  
  public isComma:(ast) => option[(locn,ast,ast)].
  isComma(A) => isBinary(A,",").

  public mkComma(Lc,L,R) => binary(Lc,",",L,R).
  
  public isAbstraction:(ast) => option[(locn,ast,ast)].
  isAbstraction(A) where (Lc,[T]) ^= isBrTuple(A) &&
      (_,B,C) ^= isBinary(T,"|") => some((Lc,B,C)).
  isAbstraction(_) default => .none.

  public isTheta:(ast) => option[(locn,cons[ast])].
  isTheta(A) where (Lc,Els) ^= isBrTuple(A) && ~ _ ^= isAbstraction(A) =>
    some((Lc,Els)).
  isTheta(_) default => .none.

  public mkTheta(Lc,Els) => brTuple(Lc,Els).

  public isQTheta:(ast) => option[(locn,cons[ast])].
  isQTheta(A) => isQBrTuple(A).

  public mkQTheta(Lc,Els) => qbrTuple(Lc,Els).

  public isLabeledTheta:(ast) => option[(locn,ast,cons[ast])].
  isLabeledTheta(A) => isBrTerm(A).

  public mkLabeledTheta(Lc,Lb,Els) => mkBrTerm(Lc,Lb,Els).

  public isLabeledRecord:(ast) => option[(locn,ast,cons[ast])].
  isLabeledRecord(A) => isQBrTerm(A).

  public mkLabeledRecord(Lc,Lb,Els) => mkQBrTerm(Lc,Lb,Els).

  public isRecordUpdate:(ast) => option[(locn,ast,ast)].
  isRecordUpdate(A) => isBinary(A,"<<-").

  public mkRecordUpdate(Lc,L,R) => binary(Lc,"<<-",L,R).

  public implementation coercion[locn,ast]=>{
    _coerce(Lc where locn(Pkg,Line,Col,Off,Ln).=Lc)=>
      some(roundTerm(Lc,nme(Lc,"locn"),[str(Lc,Pkg),
	    int(Lc,Line), int(Lc,Col), int(Lc,Off), int(Lc,Ln)])).
  }
}
