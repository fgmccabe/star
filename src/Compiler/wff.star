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

  public isName:(ast) => option[(option[locn],string)].
  isName(.nme(Lc,Id)) where ~ keyword(Id) => .some((Lc,Id)).
  isName(.qnm(Lc,Id)) => .some((Lc,Id)).
  isName(.tpl(_,"()",[.nme(Lc,Id)])) => .some((Lc,Id)).
  isName(.tpl(_,"()",[.qnm(Lc,Id)])) => .some((Lc,Id)).
  isName(_) default => .none.

  public dollarName:(ast) => ast.
  dollarName(N) where (Lc,Id) ?= isName(N) => .nme(Lc,"$"++Id).

  public dotId:(ast) => ast.
  dotId(.nme(Lc,Id)) => .nme(Lc,dotName(Id)).

  public hashName:(ast) => ast.
  hashName(.nme(Lc,Id)) => .nme(Lc,"#"++Id).

  public isKeyword:(ast) => option[(option[locn],string)].
  isKeyword(.nme(Lc,Id)) where keyword(Id) => .some((Lc,Id)).
  isKeyword(_) default => .none.
  
  public isLitAst:(ast) => boolean.
  isLitAst(.int(_,_)) => .true.
  isLitAst(.num(_,_)) => .true.
  isLitAst(.big(_,_)) => .true.
  isLitAst(.chr(_,_)) => .true.
  isLitAst(.str(_,_)) => .true.
  isLitAst(_) default => .false.

  public isSquareTerm:(ast) => option[(option[locn],ast,cons[ast])].
  isSquareTerm(.app(Lc,Op,.tpl(_,"[]",A))) => .some((Lc,Op,A)).
  isSquareTerm(_) default => .none.

  public squareTerm:(option[locn],ast,cons[ast])=>ast.
  squareTerm(Lc,Op,Args) => .app(Lc,Op,.tpl(Lc,"[]",Args)).

  public squareTermName:(ast) => option[ast].
  squareTermName(A) where (_,Op,_) ?= isSquareTerm(A) => .some(Op).
  squareTermName(_) default => .none.
  
  public isSquareApply:(ast) => option[(option[locn],string,cons[ast])].
  isSquareApply(.app(Lc,Op,.tpl(_,"[]",A))) where
      (_,Id) ?= isName(Op) => .some((Lc,Id,A)).
  isSquareApply(_) default => .none.

  public squareApply:(option[locn],string,cons[ast])=>ast.
  squareApply(Lc,Nm,Args) =>
    squareTerm(Lc,.nme(Lc,Nm),Args).

  public qBrTuple:(option[locn],cons[ast]) => ast.
  qBrTuple(Lc,Els) => .tpl(Lc,"{..}",Els).

  public isBrTerm:(ast) => option[(option[locn],ast,cons[ast])].
  isBrTerm(.app(Lc,Op,.tpl(_,"{}",A))) => .some((Lc,Op,A)).
  isBrTerm(_) default => .none.

  public mkBrTerm(Lc,Op,Els) => .app(Lc,Op,.tpl(Lc,"{}",Els)).

  public isBrApply:(ast,string) => option[(option[locn],cons[ast])].
  isBrApply(.app(Lc,Op,.tpl(_,"{}",A)),Id) where
      (_,Id) ?= isName(Op) => .some((Lc,A)).
  isBrApply(_,_) default => .none.

  public brApply(Lc,Nm,Els) => .app(Lc,.nme(Lc,Nm),.tpl(Lc,"{}",Els)).

  public isQBrTerm:(ast) => option[(option[locn],ast,cons[ast])].
  isQBrTerm(.app(Lc,Op,.tpl(_,"{..}",A))) => .some((Lc,Op,A)).
  isQBrTerm(_) default => .none.

  public mkQBrTerm(Lc,Op,Els) => .app(Lc,Op,.tpl(Lc,"{..}",Els)).

  public isQBrApply:(ast) => option[(option[locn],string,cons[ast])].
  isQBrApply(.app(Lc,Op,.tpl(_,"{..}",A))) where
      (_,Id) ?= isName(Op) => .some((Lc,Id,A)).
  isQBrApply(_) default => .none.

  public isRoundTerm:(ast) => option[(option[locn],ast,cons[ast])].
  isRoundTerm(.app(Lc,Op,.tpl(_,"()",A))) where ~_?=isKeyword(Op) => .some((Lc,Op,A)).
  isRoundTerm(_) default => .none.

  public roundTerm:(option[locn],ast,cons[ast]) => ast.
  roundTerm(Lc,Op,Els) => .app(Lc,Op,.tpl(Lc,"()",Els)).

  public braceTerm:(option[locn],ast,cons[ast]) => ast.
  braceTerm(Lc,Op,Els) => .app(Lc,Op,.tpl(Lc,"{}",Els)).

  public qbraceTerm(Lc,Op,Els) => .app(Lc,Op,.tpl(Lc,"{..}",Els)).

  public isQuantified:(ast)=>option[(option[locn],cons[ast],ast)].
  isQuantified(T) where
      (Lc,Lh,B)?=isBinary(T,"~~") && (_,V)?=isUnary(Lh,"all") =>
    .some((Lc,deComma(V),B)).
  isQuantified(_) default => .none.

  public reUQuant:(option[locn],cons[ast],ast) => ast.
  reUQuant(_,[],T) => T.
  reUQuant(Lc,[Q,..Qs],T) =>
    binary(Lc,"~~",unary(Lc,"all",reComma([Q,..Qs])),T).

  public isXQuantified:(ast)=>option[(option[locn],cons[ast],ast)].
  isXQuantified(T) where
      (Lc,Lh,B)?=isBinary(T,"~~") && (_,V)?=isUnary(Lh,"exists") =>
    .some((Lc,deComma(V),B)).
  isXQuantified(_) default => .none.

  public reXQuant:(option[locn],cons[ast],ast) => ast.
  reXQuant(_,[],T) => T.
  reXQuant(Lc,[Q,..Qs],T) =>
    binary(Lc,"~~",unary(Lc,"exists",reComma([Q,..Qs])),T).

  public isTypeFunVar:(ast) => option[(option[locn],ast,ast)].
  isTypeFunVar(A) => isBinary(A,"/").

  public mkTypeFunVar:(option[locn],ast,ast)=>ast.
  mkTypeFunVar(Lc,N,A) => binary(Lc,"/",N,A).

  public isConstrained:(ast) => option[(option[locn],cons[ast],ast)].
  isConstrained(T) where
      (Lc,Lh,B) ?= isBinary(T,"|:") => .some((Lc,deComma(Lh),B)).
  isConstrained(_) default => .none.

  public reConstrain:(cons[ast],ast) => ast.
  reConstrain([],T) => T.
  reConstrain([C,..Cs],T) => binary(locOf(T),"|:",reComma([C,..Cs]),T).

  public isSuppress:(ast)=>option[(option[locn],ast)].
  isSuppress(A) => isUnary(A,"λ").

  public mkSuppress:(option[locn],ast)=>ast.
  mkSuppress(Lc,A) => unary(Lc,"λ",A).

  public isConstructorType:(ast) => option[(option[locn],ast,ast)].
  isConstructorType(A) => isBinary(A,"<=>").

  public isFunctionType:(ast) => option[(option[locn],ast,ast)].
  isFunctionType(A) => isBinary(A,"=>").

  public isContinType:(ast) => option[(option[locn],ast,ast)].
  isContinType(A) => isBinary(A,"=>>").

  public mkContinType(Lc,A,B) => binary(Lc,"=>>",A,B).

  public mkContType:(option[locn],ast) => ast.
  mkContType(Lc,Lhs) => mkContinType(Lc,Lhs,unit(Lc)).

  public isConstructorStmt(A) where (_,_,I) ?= isQuantified(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,_,I) ?= isXQuantified(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,I) ?= isPrivate(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) where (_,I) ?= isPublic(A) =>
    isConstructorStmt(I).
  isConstructorStmt(A) => _ ?= isBinary(A,"<=>").

  public mkConstructorType(Lc,L,R) => binary(Lc,"<=>",L,R).

  public mkFunctionType(Lc,L,R) => binary(Lc,"=>",L,R).
  
  public deComma:(ast) => cons[ast].
  deComma(Trm) => let{.
    deC(T,SoF) where (_,Lh,Rh)?=isBinary(T,",") =>
      deC(Rh,[Lh,..SoF]).
    deC(T,SoF) => reverse([T,..SoF]).
  .} in deC(Trm,[]).

  public reComma:(cons[ast]) => ast.
  reComma([A]) => A.
  reComma([A,..As]) =>
    binary(locOf(A),",",A,reComma(As)).

  public isDepends:(ast) => option[(option[locn],cons[ast],cons[ast])].
  isDepends(T) where (Lc,Lh,Rh)?=isBinary(T,"->>") =>
    .some((Lc,deComma(Lh),deComma(Rh))).
  isDepends(_) default => .none.

  public mkDepends(Lc,L,R) => binary(Lc,"->>",reComma(L),reComma(R)).

  public isRaises:(ast) => option[(option[locn],ast)].
  isRaises(T) => isUnary(T,"raises").

  public mkRaises(Lc,R) => unary(Lc,"raises",R).

  public isAnnotation:(ast) => option[(option[locn],ast,ast)].
  isAnnotation(A) where (Lc,L,R) ?= isBinary(A,"@") =>
    .some((Lc,L,R)).
  isAnnotation(A) where (Lc,Rh) ?= isUnary(A,"@") => .some((Lc,.str(Lc,""),Rh)).
  isAnnotation(_) default => .none.

  public isTypeLambda:(ast) => option[(option[locn],ast,ast)].
  isTypeLambda(A) => isBinary(A,"~>").

  public mkTypeLambda(Lc,L,R) => binary(Lc,"~>",L,R).

  public isTypeExists:(ast) => option[(option[locn],ast,ast)].
  isTypeExists(A) => isBinary(A,"<~").

  public mkTypeExists(Lc,L,R) => binary(Lc,"<~",L,R).

  public isTypeAnnotation:(ast)=>option[(option[locn],ast,ast)].
  isTypeAnnotation(A) => isBinary(A,":").

  public mkTypeAnnotation:(option[locn],ast,ast)=>ast.
  mkTypeAnnotation(Lc,V,T) => binary(Lc,":",V,T).

  public isTypeExistsStmt:(ast) => option[(option[locn],cons[ast],cons[ast],ast,ast)].
  isTypeExistsStmt(A) where
      (Lc,Q,I) ?= isQuantified(A) &&
      (_,_,Cx,L,R) ?= isTypeExistsStmt(I) => .some((Lc,Q,Cx,L,R)).
  isTypeExistsStmt(A) where
      (Lc,C,I) ?= isBinary(A,"|:") &&
      (_,Q,_,L,R) ?= isTypeExistsStmt(I) => .some((Lc,Q,deComma(C),L,R)).
  isTypeExistsStmt(A) where
      (Lc,H,I) ?= isBinary(A,"<~") &&
      (Q,T) .= getQuantifiers(H) =>
    .some((Lc,Q,[],T,I)).
  isTypeExistsStmt(A) default => .none.

  public mkTypeExistsStmt(Lc,Q,C,L,R) =>
    reUQuant(Lc,Q,reConstrain(C,binary(Lc,"<~",L,R))).

  public isTypeFunStmt:(ast) => option[(option[locn],cons[ast],cons[ast],ast,ast)].
  isTypeFunStmt(A) where
      (Lc,Q,I) ?= isQuantified(A) &&
      (_,_,Cx,L,R) ?= isTypeFunStmt(I) => .some((Lc,Q,Cx,L,R)).
  isTypeFunStmt(A) where
      (Lc,C,I) ?= isBinary(A,"|:") &&
      (_,Q,_,L,R) ?= isTypeFunStmt(I) => .some((Lc,Q,deComma(C),L,R)).
  isTypeFunStmt(A) where
      (Lc,H,I) ?= isBinary(A,"~>") &&
      (Q,T) .= getQuantifiers(H) =>
    .some((Lc,Q,[],T,I)).
  isTypeFunStmt(A) default => .none.

  public mkTypeFunStmt(Lc,Q,C,L,R) =>
    reUQuant(Lc,Q,reConstrain(C,binary(Lc,"~>",L,R))).

  public isTypeStatement:(ast) => option[(option[locn],ast,ast)].
  isTypeStatement(A) where (Lc,R) ?= isUnary(A,"type") &&
      (_,V,T) ?= isTypeAnnotation(R) => .some((Lc,V,T)).
  isTypeStatement(_) default => .none.

  public mkTypeStatement(Lc,V,T) => unary(Lc,"type",mkTypeAnnotation(Lc,V,T)).

  public isAlgebraicTypeStmt:(ast) => 
    option[(option[locn],cons[ast],cons[ast],ast,ast)].
  isAlgebraicTypeStmt(A) => isAlgebraic(A).

  isAlgebraic:(ast) => option[(option[locn],cons[ast],cons[ast],ast,ast)].
  isAlgebraic(A) where
      (Lc,Q,I) ?= isQuantified(A) &&
      (_,_,Cx,L,R) ?= isAlgebraic(I) => .some((Lc,Q,Cx,L,R)).
  isAlgebraic(A) where
      (Lc,C,I) ?= isBinary(A,"|:") &&
      (_,Q,_,L,R) ?= isAlgebraic(I) => .some((Lc,Q,deComma(C),L,R)).
  isAlgebraic(A) where
      (Lc,H,I) ?= isBinary(A,"::=") &&
      (Q,T) .= getQuantifiers(H) =>
    .some((Lc,Q,[],T,I)).
  isAlgebraic(A) default => .none.

  public mkAlgebraicTypeStmt(Lc,Q,C,H,B) =>
    binary(Lc,"::=",reUQuant(Lc,Q,reConstrain(C,H)),B).

  public isLetDef:(ast) => option[(option[locn],cons[ast],ast)].
  isLetDef(A) where (Lc,Lh,Rh) ?= isBinary(A,"in") &&
      (_,Body) ?= isUnary(Lh,"let") &&
      (_,Els) ?= isBrTuple(Body) => .some((Lc,Els,Rh)).
  isLetDef(_) default => .none.

  public mkLetDef(Lc,Els,Bnd) =>
    binary(Lc,"in",unary(Lc,"let",brTuple(Lc,Els)),Bnd).

  public isLetRecDef:(ast) => option[(option[locn],cons[ast],ast)].
  isLetRecDef(A) where (Lc,Lh,Rh) ?= isBinary(A,"in") &&
      (_,Body) ?= isUnary(Lh,"let") &&
      (_,Els) ?= isQBrTuple(Body) => .some((Lc,Els,Rh)).
  isLetRecDef(_) default => .none.

  public mkLetRecDef:(option[locn],cons[ast],ast) => ast.
  mkLetRecDef(Lc,Els,Bnd) =>
    binary(Lc,"in",unary(Lc,"let",qbrTuple(Lc,Els)),Bnd).

  public isMapLiteral:(ast)=>option[(option[locn],cons[ast])].
  isMapLiteral(A) where (Lc,[I]) ?= isBrTuple(A) &&
      {? Pr in deComma(I) *> _ ?= isPair(Pr) ?} => .some((Lc,deComma(I))).
  isMapLiteral(A) where (Lc,[]) ?= isBrTuple(A) => .some((Lc,[])).
  isMapLiteral(_) default => .none.

  public mkMapLiteral(Lc,Els) => brTuple(Lc,[reComma(Els)]).
  
  public isComprehension:(ast) => option[(option[locn],ast,ast)].
  isComprehension(A) where (Lc,[T]) ?= isBrTuple(A) &&
      (_,Bnd,Body) ?= isBinary(T,"|") && ~ _ ?= isBinary(Bnd,"<*") => .some((Lc,Bnd,Body)).
  isComprehension(A) => .none.

  public mkComprehension(Lc,Exp,Cond) =>
    brTuple(Lc,[binary(Lc,"|",Exp,Cond)]).

  public isIotaComprehension:(ast) => option[(option[locn],ast,ast)].
  isIotaComprehension(A) where (Lc,I) ?= isUnary(A,"{!!}") &&
      (_,Bnd,Body) ?= isBinary(I,"|") => .some((Lc,Bnd,Body)).
  isIotaComprehension(A) => .none.

  public mkIotaComprehension(Lc,Exp,Cond) =>
    unary(Lc,"{!!}",binary(Lc,"|",Exp,Cond)).

  public isTotalizerComprehension:(ast) => option[(option[locn],ast,ast,ast,ast)].
  isTotalizerComprehension(A) where (Lc,[T]) ?= isBrTuple(A) &&
      (_,Bnd,Body) ?= isBinary(T,"|") &&
	  (_,L,Zr) ?= isBinary(Bnd,"<*") &&
	      (_,Fn,El) ?= isBinary(L,"<*")  => .some((Lc,Fn,El,Zr,Body)).
  isTotalizerComprehension(A) => .none.

  public mkTotalizerComprehension(Lc,Fn,El,Zr,Cond) =>
    brTuple(Lc,[binary(Lc,"|",binary(Lc,"<*",binary(Lc,"<*",Fn,El),Zr),Cond)]).

  public isTestComprehension(A) => isUnary(A,"{??}").

  public mkTestComprehension(Lc,C) => unary(Lc,"{??}",C).

  public isAnonBraceTuple(Trm) where (Lc,Els) ?= isBrTuple(Trm) &&
      {? D in Els *> isDefinition(D) ?} => .some((Lc,Els)).
  isAnonBraceTuple(_) default => .none.

  isDefinition(A) => (_?=isDefn(A) || _?=isTypeExistsStmt(A) ||
    _?=isTypeAnnotation(A) || _?= isTypeFunStmt(A) || _?=isEquation(A) ||
    _ ?= isAlgebraicTypeStmt(A)).

  public isConjunct(A) => isBinary(A,"&&").

  public mkConjunct(Lc,L,R) => binary(Lc,"&&",L,R).

  public isDisjunct(A) => isBinary(A,"||").

  public mkDisjunct(Lc,L,R) => binary(Lc,"||",L,R).

  public isNegation(A) => isUnary(A,"~").

  public negated(Lc,A) => unary(Lc,"~",A).

  public isImplies(A) => isBinary(A,"*>").
  
  public mkImplies(Lc,L,R) => binary(Lc,"*>",L,R).

  public isConditional(A) where
      (Lc,Tst,Rhs) ?= isBinary(A,"??") &&
      (_,Th,El) ?= isBinary(Rhs,"||") => .some((Lc,Tst,Th,El)).
  isConditional(_) => .none.

  public mkConditional:(option[locn],ast,ast,ast) => ast.
  mkConditional(Lc,T,Th,El) =>
    binary(Lc,"??",T,binary(Lc,"||",Th,El)).

  public isMatch(A) => isBinary(A,".=").

  public mkMatch(Lc,L,R) => binary(Lc,".=",L,R).

  public isOption(A) => isUnary(A,"?").

  public mkOption(Lc,A) => mkCon(Lc,"some",[A]).

  public isOptionMatch(A) where R ?=isBinary(A,"?=") => .some(R).
  isOptionMatch(A) where (Lc,L,R) ?= isBinary(A,".=") &&
      (_,Op,[LL])?=isEnumCon(L) &&
	  (_,"some") ?= isName(Op) => .some((Lc,LL,R)).
  isOptionMatch(_) default => .none.

  public mkOptionMatch(Lc,L,R) => mkMatch(Lc,mkOption(Lc,L),R).

  public isEnumSymb(A) where (Lc,.nme(_,N))?=isUnary(A,".") => .some((Lc,N)).
  isEnumSymb(_) default => .none.

  public isEnumCon(A) where (Lc,I) ?= isUnary(A,".") && (_,Op,Els) ?= isRoundTerm(I) =>
    .some((Lc,Op,Els)).
  isEnumCon(_) default => .none.

  public enum(Lc,Nm) => unary(Lc,".",.nme(Lc,Nm)).

  public mkEnumCon(Lc,Op,Args) => unary(Lc,".",roundTerm(Lc,Op,Args)).

  public mkCon(Lc,Op,Args) => unary(Lc,".",roundTerm(Lc,.nme(Lc,Op),Args)).

  public isSearch(A) where (Lc,P,G) ?= isBinary(A,"in") &&
      ~ .app(_,.nme(_,"let"),Body) .= P => .some((Lc,P,G)).
  isSearch(_) default => .none.

  public mkSearch(Lc,P,S) => binary(Lc,"in",P,S).

  getQuantifiers(T) where
      (_,Q,I) ?= isQuantified(T) => (Q,I).
  getQuantifiers(T) where
      (_,_,A) ?= isSquareTerm(T) => (A,T).
  getQuantifiers(T) default => ([],T).

  public isFieldAcc:(ast) => option[(option[locn],ast,string)].
  isFieldAcc(A) where (Lc,Rc,R) ?= isBinary(A,".") && (_,Id) ?= isNme(R) => .some((Lc,Rc,Id)).
  isFieldAcc(_) default => .none.

  public mkFieldAcc(Lc,R,F) => binary(Lc,".",R,.nme(Lc,F)).

  public isTupleAcc:(ast) => option[(option[locn],ast,integer)].
  isTupleAcc(A) where (Lc,L,R) ?= isBinary(A,".") && (_,Ix) ?= isInt(R) => .some((Lc,L,Ix)).
  isTupleAcc(_) default => .none.

  public mkTupleAcc(Lc,R,Ix) => binary(Lc,".",R,.int(Lc,Ix)).

  public isPublic:(ast) => option[(option[locn],ast)].
  isPublic(A) => isUnary(A,"public").

  public mkPublic(Lc,A) => unary(Lc,"public",A).

  public isPrivate:(ast) => option[(option[locn],ast)].
  isPrivate(A) => isUnary(A,"private").

  public mkPrivate(Lc,A) => unary(Lc,"private",A).

  public isImport:(ast)=> option[importSpec].
  isImport(A) where (Lc,I) ?= isPublic(A) =>
    (.pkgImp(_,_,Im) ?= isImport(I) ??
      .some(.pkgImp(Lc,.pUblic,Im)) ||
	.none).
  isImport(A) where (Lc,I) ?= isPrivate(A) =>
    (.pkgImp(_,_,Im) ?= isImport(I) ??
	.some(.pkgImp(Lc,.priVate,Im)) ||
	.none).
  isImport(A) where (Lc,I) ?= isUnary(A,"import") => .some(.pkgImp(Lc,.priVate,pkgeName(I))).
  isImport(_) default => .none.

  public pkgeName:(ast) => pkg.
  pkgeName(A) where (_,L,R) ?= isBinary(A,"#") => 
    .pkg(dottedName(L),.vers(dottedName(R))).
  pkgeName(A) => .pkg(dottedName(A),.defltVersion).

  dottedName:(ast) => string.
  dottedName(N) where (_,Id) ?= isName(N) => Id.
  dottedName(N) where (_,L,R) ?= isBinary(N,".") => "#(dottedName(L)).#(dottedName(R))".
  dottedName(A) default => disp(A).

  public isOpen:(ast)=> option[(option[locn],ast)].
  isOpen(A) => isUnary(A,"open").

  public mkOpen(Lc,E) => unary(Lc,"open",E).

  public isIntegrity:(ast)=> option[(option[locn],ast)].
  isIntegrity(A) => isUnary(A,"assert").

  public isShow:(ast) => option[(option[locn],ast)].
  isShow(A) => isUnary(A,"show").

  public isCoerce:(ast) => option[(option[locn],ast,ast)].
  isCoerce(A) => isBinary(A,"::").

  public mkCoercion(Lc,L,R) => binary(Lc,"::",L,R).

  public isOptCoerce:(ast) => option[(option[locn],ast,ast)].
  isOptCoerce(A) => isBinary(A,":?").

  public mkOptCoercion(Lc,L,R) => binary(Lc,":?",L,R).

  public isRef:(ast) => option[(option[locn],ast)].
  isRef(A) => isUnary(A,"ref").

  public mkRef(Lc,A) => unary(Lc,"ref",A).
  
  public isCellRef:(ast) => option[(option[locn],ast)].
  isCellRef(A) => isUnary(A,"!").

  public refCell:(option[locn],ast) => ast.
  refCell(Lc,I) => unary(Lc,"!",I).

  public isThunk:(ast) => option[(option[locn],ast)].
  isThunk(Th) => isUnary(Th,"$$").

  public mkThunk:(option[locn],ast)=>ast.
  mkThunk(Lc,E) => unary(Lc,"$$",E).

  public isThunkRef:(ast) => option[(option[locn],ast)].
  isThunkRef(Th) => isUnary(Th,"!!").

  public mkThunkRef:(option[locn],ast)=>ast.
  mkThunkRef(Lc,E) => unary(Lc,"!!",E).

  public isIndexTerm:(ast) => option[(option[locn],ast,ast)].
  isIndexTerm(A) where (Lc,Op,[Ix]) ?= isSquareTerm(A) &&
      ~ _?=isBinary(Ix,":") => .some((Lc,Op,Ix)).
  isIndexTerm(A) where (Lc,L,R) ?= isBinary(A,"!") && (_,[Ix]) ?= isSqTuple(R) =>
    .some((Lc,unary(Lc,"!",L),Ix)).
  isIndexTerm(_) default => .none.

  public mkIndexTerm(Lc,O,A) => squareTerm(Lc,O,[A]).

  public isSlice:(ast) => option[(option[locn],ast,ast,ast)].
  isSlice(A) where (Lc,Op,[Ix]) ?= isSquareTerm(A) && (_,F,T) ?= isBinary(Ix,":") =>
    .some((Lc,Op,F,T)).
  isSlice(_) default => .none.

  public isSplice:(ast) => option[(option[locn],ast,ast,ast,ast)]. -- S[F:T] := R
  isSplice(A) where (Lc,Lhs,R)?= isAssignment(A) && (_,S,F,T) ?= isSlice(Lhs) =>
    .some((Lc,S,F,T,R)).
  isSplice(_) default => .none.

  public isContractStmt:(ast) => option[(option[locn],ast,cons[ast])].
  isContractStmt(A) where
      (Lc,I) ?= isUnary(A,"contract") &&
      (_,Lhs,B) ?= isBinary(I,"::=") &&
      (_,Els) ?= isBrTuple(B) => .some((Lc,Lhs,Els)).
  isContractStmt(A) default => .none.

  public mkContractStmt(Lc,T,Els) =>
    unary(Lc,"contract",binary(Lc,"::=",T,brTuple(Lc,Els))).

  public isContractSpec:(ast) => option[(option[locn],cons[ast],
      cons[ast],string,cons[ast],cons[ast])].
  isContractSpec(A) where
      (Lc,Q,I) ?= isQuantified(A) &&
	  (_,_,C,Nm,As,Ds) ?= isContractSpec(I) => .some((Lc,Q,C,Nm,As,Ds)).
  isContractSpec(A) where
      (Lc,Lhs,Rhs) ?= isBinary(A,"|:") &&
	  (_,Q,_,Nm,As,Ds) ?= isContractSpec(Rhs) => .some((Lc,Q,deComma(Lhs),Nm,As,Ds)).
  isContractSpec(A) where
      (Lc,Nm,[E]) ?= isSquareTerm(A) &&
      (_,L,R) ?= isDepends(E) &&
      (_,Id) ?= isName(Nm) =>
    .some((Lc,[],[],Id,L,R)).
  isContractSpec(A) where
      (Lc,Nm,Els) ?= isSquareTerm(A) &&
      (_,Id) ?= isName(Nm) =>
    .some((Lc,[],[],Id,Els,[])).
  isContractSpec(_) default => .none.

  public isImplementationStmt:(ast) => option[(option[locn],cons[ast],cons[ast],ast,ast)].
  isImplementationStmt(A) where
      (Lc,I) ?= isUnary(A,"implementation") => isImplSpec(Lc,[],[],I).
  isImplementationStmt(_) default => .none.

  isImplSpec(Lc,_,Cs,T) where
      (_,Qs,In) ?= isQuantified(T) =>
    isImplSpec(Lc,Qs,Cs,In).
  isImplSpec(Lc,Qs,_,T) where
      (_,Lhs,Rhs) ?= isBinary(T,"|:") =>
    isImplSpec(Lc,Qs,deComma(Lhs),Rhs).
  isImplSpec(_,Qs,Cs,T) where
      (Lc,Cn,Exp) ?= isBinary(T,"=>") =>
    .some((Lc,Qs,Cs,Cn,Exp)).
  isImplSpec(_,_,_,_) default => .none.

  public implementedContractName:(ast) => string.
  implementedContractName(A) where
      (_,O,As) ?= isSquareApply(A) => "#(O)#(surfaceNames(As,markerString(.overMark))*)".

  surfaceNames([],_) => [].
  surfaceNames([T,.._],Sep) where (_,L,_) ?= isBinary(T,"->>") =>
    surfaceNames(deComma(L),Sep).
  surfaceNames([T,..Ts],Sep) =>
    [Sep,surfaceName(T),..surfaceNames(Ts,Sep)].

  public surfaceName(T) where (_,Id) ?= isName(T) => Id.
  surfaceName(T) where (_,Id,_) ?= isSquareApply(T) => Id.
  surfaceName(T) where (_,_,I) ?= isQuantified(T) => surfaceName(I).
  surfaceName(T) where (_,Els) ?= isTuple(T) => "()$(size(Els))".
  surfaceName(T) where _ ?= isFunctionType(T) => "=>".
  surfaceName(T) where _ ?= isContinType(T) => "=>>".
  surfaceName(T) where _ ?= isRef(T) => "ref".

  public mkImplementationStmt:(option[locn],cons[ast],cons[ast],ast,ast) => ast.
  mkImplementationStmt(Lc,Q,Cx,T,E) =>
    unary(Lc,"implementation",reUQuant(Lc,Q,reConstrain(Cx,binary(Lc,"=>",T,E)))).

  public isImplicit(A) where (Lc,L,R) ?= isBinary(A,"|=") && (_,Id)?=isName(L) => .some((Lc,Id,R)).
  isImplicit(A) where (Lc,L,R) ?= isBinary(A,":") && (_,Id)?=isName(L) => .some((Lc,Id,R)).
  isImplicit(A) default => .none.

  public mkImplicit(Lc,N,T) => binary(Lc,":",.nme(Lc,N),T).

  public typeName:(ast)=>string.
  typeName(Tp) where (_,Id) ?= isName(Tp) => Id.
  typeName(Tp) where (_,Id,_) ?= isSquareApply(Tp) => Id.
  typeName(Tp) where (_,Els) ?= isTuple(Tp) => "()$(size(Els))".

  public collectImports:(cons[ast], cons[importSpec], cons[ast]) => (cons[importSpec],cons[ast]).
  collectImports([],Imp,Oth) => (Imp,reverse(Oth)).
  collectImports([A,..Ss],Imp,Oth) => (
    Spec ?= isImport(A) ??
      collectImports(Ss,[Spec,..Imp],Oth) ||
      collectImports(Ss,Imp,[A,..Oth])).

  public ruleName:(ast) => option[(option[locn],string)].
  ruleName(A) where (_,.some(Nm),_,_,_,_) ?= isEquation(A) && (Lc,Id)?=isName(Nm) => .some((Lc,Id)).
  ruleName(_) default => .none.

  public headName:(ast) => option[string].
  headName(A) where
      (_,Nm,_) ?= isRoundTerm(A) => headName(Nm).
  headName(A) where
      (_,Id) ?= isName(A) => .some(Id).
  headName(A) where
      (_,D) ?= isDefault(A) => headName(D).
  headName(A) where (_,H,_) ?= isWhere(A) => headName(H).
  headName(A) where (_,[E]) ?= isTuple(A) => headName(E).
  headName(_) default => .none.

  public isDefn:(ast) => option[(option[locn],ast,ast)].
  isDefn(A) => isBinary(A,"=").

  public mkDefn(Lc,L,R) => binary(Lc,"=",L,R).

  public isAssignment:(ast) => option[(option[locn],ast,ast)].
  isAssignment(A) => isBinary(A,":=").

  public mkAssignment(Lc,L,R) => binary(Lc,":=",L,R).

  public isEquation:(ast) => option[(option[locn],option[ast],boolean,ast,option[ast],ast)].
  isEquation(A) where (Lc,L,R) ?= isBinary(A,"=>") &&
      (N,H,C,D) ?= splitHead(L,.none,.none,.false) => .some((Lc,N,D,H,C,R)).
  isEquation(_) default => .none.

  public mkEquation:(option[locn],option[ast],boolean,ast,option[ast],ast)=>ast.
  mkEquation(Lc,Nm,Deflt,Args,Cond,Rep) =>
    binary(Lc,"=>",mkLhs(Lc,Nm,Deflt,Args,Cond),Rep).

  mkHed(Lc,.some(Nm),Args) => .app(Lc,Nm,Args).
  mkHed(_,.none,Args) => Args.

  mkLhs(Lc,Nm,.true,Args,.none) => unary(Lc,"default",mkHed(Lc,Nm,Args)).
  mkLhs(Lc,Nm,.true,Args,.some(C)) =>
    unary(Lc,"default",binary(Lc,"where",mkHed(Lc,Nm,Args),C)).
  mkLhs(Lc,Nm,.false,Args,.none) => mkHed(Lc,Nm,Args).
  mkLhs(Lc,Nm,.false,Args,.some(C)) => binary(Lc,"where",mkHed(Lc,Nm,Args),C).

  splitHead:(ast,option[ast],option[ast],boolean) => option[(option[ast],ast,option[ast],boolean)].
  splitHead(A,N,C,_) where (_,I) ?= isDefault(A) => splitHead(I,N,C,.true).
  splitHead(A,_,C,D) where (Lc,Nm,As) ?= isRoundTerm(A) => .some((.some(Nm),rndTuple(Lc,As),C,D)).
  splitHead(A,N,C,D) where (Lc,[E]) ?= isTuple(A) => splitHead(E,N,C,D).
  splitHead(A,N,C1,D) where (Lc,L,C2) ?= isWhere(A) => splitHead(L,N,mergeCond(.some(C2),C1),D).
  splitHead(A,N,C,D) => .some((N,A,C,D)).

  splitHd:(ast,option[ast],option[ast],boolean) => option[(option[ast],ast,option[ast],boolean)].
  splitHd(A,N,C,_) where (_,I) ?= isDefault(A) => splitHd(I,N,C,.true).
  splitHd(A,_,C,D) where (Lc,Nm,As) ?= isRoundTerm(A) => .some((.some(Nm),rndTuple(Lc,As),C,D)).
  splitHd(A,N,C1,D) where (Lc,L,C2) ?= isWhere(A) => splitHd(L,N,mergeCond(.some(C2),C1),D).
  splitHd(A,N,C,D) => .some((N,A,C,D)).

  public isLambda:(ast) => option[(option[locn],boolean,ast,option[ast],ast)].
  isLambda(A) where (Lc,L,R) ?= isBinary(A,"=>") &&
      (H,C,D) ?= splitLHead(L,.none,.false) => .some((Lc,D,H,C,R)).
  isLambda(_) default => .none.

  splitLHead:(ast,option[ast],boolean) => option[(ast,option[ast],boolean)].
  splitLHead(A,C,_) where (_,I) ?= isDefault(A) => splitLHead(I,C,.true).
  splitLHead(A,C1,D) where (Lc,L,C2) ?= isWhere(A) => splitLHead(L,mergeCond(.some(C2),C1),D).
  splitLHead(A,C,D) => .some((A,C,D)).

  public mergeCond(.none,G) => G.
  mergeCond(G,.none) => G.
  mergeCond(.some(L),.some(R)) => .some(mkConjunct(locOf(L),L,R)).

  public mkLambda:(option[locn],boolean,ast,option[ast],ast) => ast.
  mkLambda(Lc,Deflt,Arg,Cond,Rep) =>
    equation(Lc,mkLambdaHead(Lc,Arg,Deflt,Cond),Rep).

  mkLambdaHead(Lc,Arg,.true,.none) => unary(Lc,"default",Arg).
  mkLambdaHead(Lc,Arg,.false,.some(C)) => binary(Lc,"where",Arg,C).
  mkLambdaHead(Lc,Arg,.false,.none) => Arg.

  public areEquations:(cons[ast]) => boolean.
  areEquations(L) => {? E in L *> _ ?= isEquation(E) ?}.

  public equation:(option[locn],ast,ast)=>ast.
  equation(Lc,Hd,Rep) => binary(Lc,"=>",Hd,Rep).

  public isCase:(ast) => option[(option[locn],ast,cons[ast])].
  isCase(A) where (Lc,L) ?= isUnary(A,"case") &&
      (_,Lhs,Rhs) ?= isBinary(L,"in") &&
	  (_,Els) ?= isBrTuple(Rhs) =>
    ([El].=Els ?? .some((Lc,Lhs,deBar(El))) || .some((Lc,Lhs,Els))).
  isCase(_) => .none.

  public mkCaseExp(Lc,E,C) =>
    unary(Lc,"case",binary(Lc,"in",E,brTuple(Lc,[reBar(C)]))).

  public deBar:(ast) => cons[ast].
  deBar(Trm) => let{.
    deC(T) where (_,Lh,Rh)?=isBinary(T,"|") => deC(Lh)++deC(Rh).
    deC(T) where (_,Rh) ?= isUnary(T,"|") => [Rh].
    deC(T) => [T].
  .} in deC(Trm).

  public reBar:(cons[ast]) => ast.
  reBar([A]) => A.
  reBar([A,..As]) =>
    binary(locOf(A),"|",A,reBar(As)).

  public isWhere:(ast) => option[(option[locn],ast,ast)].
  isWhere(A) => isBinary(A,"where").

  public mkWhere(Lc,L,R) => binary(Lc,"where",L,R).

  public mkWhereEquality:(ast) =>ast.
  mkWhereEquality(Nm) where Lc.=locOf(Nm) && V.=genName(Lc,"_W") =>
    binary(Lc,"where",V,binary(Lc,"==",Nm,V)).
    
  public mkWhereTest:(option[locn],string) =>ast.
  mkWhereTest(Lc,Op) where V.=genName(Lc,"_W") =>
    binary(Lc,"where",V,unary(Lc,Op,V)).

  public mkWherePtn:(option[locn],ast,ast) => ast.
  mkWherePtn(Lc,Ptn,Op) where V.=genName(Lc,"_P") =>
    binary(Lc,"where",V,binary(Lc,".=",mkOption(Lc,Ptn),roundTerm(Lc,Op,[V]))).

  public isOptionPtn:(ast) => option[(option[locn],ast,ast)].
  isOptionPtn(A) => isBinary(A,"^").

  public isOptVal:(ast) => option[(option[locn],ast)].
  isOptVal(A) => isUnary(A,"^").
    
  public isDefault:(ast) => option[(option[locn],ast)].
  isDefault(A) => isUnary(A,"default").

  public isFiberTerm:(ast) => option[(option[locn],ast)].
  isFiberTerm(A) where (Lc,[Args]) ?= isBrApply(A,"fiber") =>
    .some((Lc,Args)).
  isFiberTerm(_) default => .none.

  public mkFiberTerm(Lc,As) => brApply(Lc,"fiber",[As]).

  public isFiber(A) => isUnary(A,"fiber").

  public isActionSeq:(ast) => option[(option[locn],ast,ast)].
  isActionSeq(A) => isBinary(A,";").

  public actionSeq(Lc,L,R) => binary(Lc,";",L,R).

  public isSoloSeq:(ast) => option[(option[locn],ast)].
  isSoloSeq(A) => isUnary(A,";").

  public isValis:(ast) => option[(option[locn],ast)].
  isValis(A) => isUnary(A,"valis").

  public mkValis(Lc,I) => unary(Lc,"valis",I).

  public isValof:(ast) => option[(option[locn],ast)].
  isValof(A) => isUnary(A,"valof").

  public mkValof(Lc,I) => unary(Lc,"valof",I).

  public isRaise:(ast) => option[(option[locn],ast)].
  isRaise(A) => isUnary(A,"raise").

  public mkRaise(Lc,E) => unary(Lc,"raise",E).

  public isSpawn:(ast) => option[(option[locn],ast,ast)].
  isSpawn(A) where (Lc,L,R) ?= isBinary(A,"spawn") && ~_?=isBinary(R,"=>>") => .some((Lc,L,R)).
  isSpawn(_) default => .none.

  public mkSpawn(Lc,L,R) => binary(Lc,"spawn",L,R).

  public isPaused:(ast) => option[(option[locn],ast,ast,ast)].
  isPaused(A) where (Lc,L,E) ?= isBinary(A,"=>>") &&
      (_,T,F) ?= isBinary(L,"spawn") => .some((Lc,T,F,E)).
  isPaused(_) default => .none.

  public mkPaused(Lc,T,F,E) => binary(Lc,"=>>",binary(Lc,"spawn",T,F),E).

  public isInvoke:(ast) => option[(option[locn],ast,cons[ast])].
  isInvoke(A) where (Lc,Op,A) ?= isBinary(A,".") && (_,As) ?= isTuple(A) => .some((Lc,Op,As)).
  isInvoke(_) default => .none.
  
  public isSuspend(A) => isBinary(A,"suspend").

  public mkSuspend(Lc,L,R) => binary(Lc,"suspend",L,R).

  public isResume(A) => isBinary(A,"resume").

  public mkResume(Lc,L,R) => binary(Lc,"resume",L,R).

  public isRetire(A) => isBinary(A,"retire").

  public mkRetire(Lc,L,R) => binary(Lc,"retire",L,R).

  public isPerform:(ast) => option[(option[locn],ast)].
  isPerform(A) => isUnary(A,"perform").

  public mkPerform(Lc,A) => unary(Lc,"perform",A).

  public isTryCatch:(ast) => option[(option[locn],ast,ast,cons[ast])].
  isTryCatch(A) where (Lc,I) ?= isUnary(A,"try") &&
      (_,B,R) ?= isBinary(I,"catch") &&
	  (_,E,H) ?= isBinary(R,"in") &&
	      (_,Hs) ?= isBrTuple(H) =>
    ([El].=Hs ?? .some((Lc,B,E,deBar(El))) || .some((Lc,B,E,Hs))).
  isTryCatch(_) default => .none.

  public mkTryCatch(Lc,B,E,Hs) =>
    unary(Lc,"try",binary(Lc,"catch",B,binary(Lc,"in",E,brTuple(Lc,[reBar(Hs)])))).

  public isTryWith:(ast) => option[(option[locn],ast,ast,cons[ast])].
  isTryWith(A) where (Lc,I) ?= isUnary(A,"try") &&
      (_,B,R) ?= isBinary(I,"with") &&
	  (_,E,H) ?= isBinary(R,"in") &&
	      (_,Hs) ?= isBrTuple(H) =>
    ([El].=Hs ?? .some((Lc,B,E,deBar(El))) || .some((Lc,B,E,Hs))).
  isTryWith(_) default => .none.

  public mkTryWith(Lc,B,E,Hs) =>
    unary(Lc,"try",binary(Lc,"with",B,binary(Lc,"in",E,brTuple(Lc,[reBar(Hs)])))).

  public isIfThenElse:(ast) => option[(option[locn],ast,ast,ast)].
  isIfThenElse(A) where
      (Lc,Lhs,El) ?= isBinary(A,"else") &&
      (_,LL,Th) ?= isBinary(Lhs,"then") &&
      (_, Ts) ?= isUnary(LL,"if") => .some((Lc,Ts,Th,El)).
  isIfThenElse(_) default => .none.

  public mkIfThenElse(Lc,T,Th,El) =>
    binary(Lc,"else",binary(Lc,"then",unary(Lc,"if",T),Th),El).

  public isIfThen:(ast) => option[(option[locn],ast,ast)].
  isIfThen(A) where
      (Lc,LL,Th) ?= isBinary(A,"then") &&
      (_, Ts) ?= isUnary(LL,"if") => .some((Lc,Ts,Th)).
  isIfThen(_) default => .none.

  public mkIfThen(Lc,T,Th) =>
    binary(Lc,"then",unary(Lc,"if",T),Th).

  public isWhileDo:(ast) => option[(option[locn],ast,ast)].
  isWhileDo(A) where
      (Lc,LL,Bd) ?= isBinary(A,"do") &&
      (_, Ts) ?= isUnary(LL,"while") => .some((Lc,Ts,Bd)).
  isWhileDo(_) default => .none.

  public mkWhileDo(Lc,C,B) => binary(Lc,"do",unary(Lc,"while",C),B).

  public isForDo:(ast) => option[(option[locn],ast,ast,ast)].
  isForDo(A) where
      (Lc,LL,Bd) ?= isBinary(A,"do") &&
      (_, Ts) ?= isUnary(LL,"for") &&
      (_,El,It) ?= isBinary(Ts,"in") =>
    .some((Lc,El,It,Bd)).
  isForDo(_) default => .none.

  public mkForDo(Lc,El,It,B) => binary(Lc,"do",unary(Lc,"for",binary(Lc,"in",El,It)),B).

  public isCons:(ast) => option[(option[locn],ast,ast)].
  isCons(A) => isBinary(A,",..").
  
  public mkCons(Lc,L,R) => binary(Lc,",..",L,R).
  
  public isComma:(ast) => option[(option[locn],ast,ast)].
  isComma(A) => isBinary(A,",").

  public mkComma(Lc,L,R) => binary(Lc,",",L,R).

  public isPair:(ast) => option[(option[locn],ast,ast)].
  isPair(A) => isBinary(A,"->").

  public mkPair(Lc,L,R) => binary(Lc,"->",L,R).

  public isSequence:(ast) => option[(option[locn],ast,ast)].
  isSequence(A) => isBinary(A,";").

  public mkSequence(Lc,L,R) => binary(Lc,";",L,R).

  public isLbldAction:(ast)=>option[(option[locn],string,ast)].
  isLbldAction(A) where
      (Lc,L,R) ?= isBinary(A,":") && (_,Lbl) ?= isName(L) => .some((Lc,Lbl,R)).
  isLbldAction(_) default => .none.

  public mkLbldAction(Lc,Lb,R) => binary(Lc,":",.nme(Lc,Lb),R).

  public isBreak(A) where (Lc,L) ?= isUnary(A,"break") && (_,Lbl) ?= isName(L) =>
    .some((Lc,Lbl)).
  isBreak(_) default => .none.

  public mkBreak(Lc,Lb) => unary(Lc,"break",.nme(Lc,Lb)).

  public isTheta:(ast) => option[(option[locn],cons[ast])].
  isTheta(A) where (Lc,Els) ?= isBrTuple(A) && ~ _ ?= isComprehension(A) =>
    .some((Lc,Els)).
  isTheta(_) default => .none.

  public mkTheta(Lc,Els) => brTuple(Lc,Els).

  public isQTheta:(ast) => option[(option[locn],cons[ast])].
  isQTheta(A) => isQBrTuple(A).

  public mkQTheta(Lc,Els) => qbrTuple(Lc,Els).

  public isLabeledTheta:(ast) => option[(option[locn],ast,cons[ast])].
  isLabeledTheta(A) where (Lc,Op,Els) ?= isQBrTerm(A) &&
      ~_?=isKeyword(Op) => .some((Lc,Op,Els)).
  isLabeledTheta(_) default => .none.

  public mkLabeledTheta(Lc,Lb,Els) => mkQBrTerm(Lc,Lb,Els).

  public isRecordUpdate:(ast) => option[(option[locn],ast,string,ast)].
  isRecordUpdate(A) where (Lc,Lhs,Vl) ?= isBinary(A,"=") &&
      (_,Rc,Fld) ?= isFieldAcc(Lhs) => .some((Lc,Rc,Fld,Vl)).
  isRecordUpdate(_) default => .none.

  public mkRecordUpdate(Lc,Rc,Fld,Vl) =>
    binary(Lc,"=",mkFieldAcc(Lc,Rc,Fld),Vl).

  public implementation coercion[locn,ast]=>{
    _coerce(Lc where .locn(Pkg,Line,Col,Off,Ln).=Lc)=>
      .some(roundTerm(.some(Lc),.nme(.none,"locn"),[.str(.none,Pkg),
	    .int(.none,Line), .int(.none,Col), .int(.none,Off), .int(.none,Ln)])).
  }

  public isTrace:(ast) => option[(option[locn],ast)].
  isTrace(A) => isUnary(A,"trace").
}
