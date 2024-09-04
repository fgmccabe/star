star.compiler.macro{
  import star.
  import star.sort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.macro.grammar.
  import star.compiler.macro.infra.
  import star.compiler.macro.rules.
  import star.compiler.wff.

  macroAst:(ast,macroContext,(ast)=>ast) => ast.
  macroAst(A,Cxt,Examine) => 
    case applyRules(A,Cxt,.inactive) in {
    | .active(T) => valof{
	if macroTracing! then
	  showMsg("$(A) macro replaced with $(T)");
	valis macroAst(T,Cxt,Examine)
      }
    | .inactive => Examine(A)
    }.

  public macroPkg:(ast) => ast.
  macroPkg(A) => 
    macroAst(A,.package,examinePkg).

  examinePkg(A) where (Lc,O,Els) ?= isBrTerm(A) => 
    mkLabeledTheta(Lc,O,macroStmts(buildMain(Els))).

  macroStmts:(cons[ast])=>cons[ast].
  macroStmts(Ss) => reverse(flattenStmts(makeGrammar(Ss)//macroStmt,[])).

  flattenStmts([],So) => So.
  flattenStmts([A,..As],So) where (_,Els) ?= isBrTuple(A) =>
    flattenStmts(As,flattenStmts(Els,So)).
  flattenStmts([A,..As],So) => flattenStmts(As,[A,..So]).

  macroStmt:(ast) => ast.
  macroStmt(A) => macroAst(A,.statement,examineStmt).

  examineStmt:(ast) => ast.
  examineStmt(A) where (Lc,L,R) ?= isTypeAnnotation(A) => 
    mkTypeAnnotation(Lc,L,macroType(R)).
  examineStmt(A) where (Lc,R) ?= isPublic(A) => 
    mkPublic(Lc,macroStmt(R)).
  examineStmt(A) where (Lc,R) ?= isPrivate(A) =>
    mkPrivate(Lc,macroStmt(R)).
  examineStmt(A) where _ ?= isImport(A) => A.
  examineStmt(A) where (Lc,L,R) ?= isDefn(A) => 
    mkDefn(Lc,macroPtn(L),macroTerm(R)).
  examineStmt(A) where (Lc,Nm,Deflt,L,C,R) ?= isEquation(A) => 
    mkEquation(Lc,Nm,Deflt,macroPtn(L),macroOpt(C,macroCond),macroTerm(R)).
  examineStmt(A) where (Lc,Q,C,L,R) ?= isTypeFunStmt(A) => 
    mkTypeFunStmt(Lc,Q//macroType,C//macroType,macroType(L),macroType(R)).
  examineStmt(A) where (Lc,L,Els) ?= isContractStmt(A) => 
    mkContractStmt(Lc,macroType(L),macroStmts(Els)).
  examineStmt(A) where (Lc,Q,Cx,Tp,Exp) ?= isImplementationStmt(A) => 
    mkImplementationStmt(Lc,Q//macroType,Cx//macroConstraint,macroType(Tp),macroTerm(Exp)).
  examineStmt(A) where (Lc,Q,C,Tp,B) ?= isAlgebraicTypeStmt(A) =>
    mkAlgebraicTypeStmt(Lc,Q//macroTypeVar,C//macroConstraint,macroType(Tp),macroAlgebraic(B)).
  examineStmt(A) where _ ?= isAnnotation(A) => A.
  examineStmt(A) => valof{
    reportError("cannot figure out statement\n$(A)",locOf(A));
    valis A
  }

  macroAlgebraic(A) => macroAst(A,.constructor,examCons).

  examCons(A) => examineConstructor(A).

  examineConstructor(A) where (Lc,L,R)?=isBinary(A,"|") =>
    binary(Lc,"|",macroAlgebraic(L),macroAlgebraic(R)).
  examineConstructor(A) where (Lc,R)?=isUnary(A,"|") =>
    unary(Lc,"|",macroAlgebraic(R)).
  examineConstructor(A) where _ ?= isEnumSymb(A) => A.
  examineConstructor(A) where (Lc,O,Els) ?= isRoundTerm(A) =>
    roundTerm(Lc,macroTerm(O),Els//macroType).
  examineConstructor(A) where (Lc,O,Els) ?= isEnumCon(A) =>
    mkEnumCon(Lc,macroTerm(O),Els//macroType).
  examineConstructor(A) where (Lc,O,Els) ?= isBrTerm(A) => 
    braceTerm(Lc,O,Els//macroTypeDef).
  examineConstructor(A) where (Lc,Q,I) ?= isQuantified(A) =>
    reUQuant(Lc,Q//macroType,macroAlgebraic(I)).
  examineConstructor(A) where (Lc,Q,I) ?= isXQuantified(A) =>
    reXQuant(Lc,Q//macroType,macroAlgebraic(I)).
  examineConstructor(A) => valof{
    reportError("cannot figure out constructor $(A)",locOf(A));
    valis A
  }

  macroTypeDef:(ast) => ast.
  macroTypeDef(A) => macroAst(A,.statement,examineTypeStmt).

  examineTypeStmt:(ast) => ast.
  examineTypeStmt(A) where (Lc,L,R) ?= isTypeAnnotation(A) => 
    mkTypeAnnotation(Lc,L,macroType(R)).
  examineTypeStmt(A) where (Lc,Q,C,L,R) ?= isTypeFunStmt(A) => 
    mkTypeFunStmt(Lc,Q//macroType,C//macroType,macroType(L),macroType(R)).
  examineTypeStmt(A) where (Lc,L,R) ?= isTypeExists(A) => 
    mkTypeExists(Lc,macroType(L),macroType(R)).
  examineTypeStmt(A) where _ ?= isAnnotation(A) => A.
  examineTypeStmt(A) => valof{
    reportError("cannot figure out declaration\n$(A)",locOf(A));
    valis A
  }

  macroAction:(ast) => ast.
  macroAction(A) => macroAst(A,.actn,examineAction).

  examineAction(A) where (Lc,L,R) ?= isActionSeq(A) => 
    actionSeq(Lc,macroAction(L),macroAction(R)).
  examineAction(A) where (Lc,L) ?= isUnary(A,";") => 
    macroAction(L).
  examineAction(A) where (Lc,[As]) ?= isBrTuple(A) =>
    brTuple(Lc,[macroAction(As)]).
  examineAction(A) where (Lc,[]) ?= isBrTuple(A) => A.
  examineAction(A) where (Lc,L,R) ?= isLbldAction(A) => 
    mkLbldAction(Lc,L,macroAction(R)).
  examineAction(A) where _ ?= isBreak(A) => A.
  examineAction(A) where (Lc,L,R) ?= isDefn(A) => 
    mkDefn(Lc,macroPtn(L),macroTerm(R)).
  examineAction(A) where (Lc,L,R) ?= isMatch(A) => 
    mkMatch(Lc,macroPtn(L),macroTerm(R)).
  examineAction(A) where (Lc,L,R) ?= isOptionMatch(A) =>
    mkOptionMatch(Lc,macroPtn(L),macroTerm(R)).
  examineAction(A) where (Lc,L,R) ?= isAssignment(A) => 
    mkAssignment(Lc,macroTerm(L),macroTerm(R)).
  examineAction(A) where (Lc,T,L,R) ?= isIfThenElse(A) =>
    mkIfThenElse(Lc,macroCond(T),macroAction(L),macroAction(R)).
  examineAction(A) where (Lc,T,L) ?= isIfThen(A) => 
    mkIfThen(Lc,macroCond(T),macroAction(L)).
  examineAction(A) where (Lc,B,E,Hs) ?= isTryCatch(A) =>
    mkTryCatch(Lc,macroAction(B),macroType(E),Hs//macroCaseAction).
  examineAction(A) where (Lc,B,E,Hs) ?= isTryHandle(A) =>
    mkTryHandle(Lc,macroAction(B),macroType(E),Hs//macroCaseAction).
  examineAction(A) where (Lc,C,B) ?= isWhileDo(A) =>
    mkWhileDo(Lc,macroCond(C),macroAction(B)).
  examineAction(A) where (Lc,El,C,B) ?= isForIn(A) =>
    mkForIn(Lc,macroPtn(El),macroTerm(C),macroAction(B)).
  examineAction(A) where (Lc,El,C,B) ?= isForDo(A) => 
    mkForDo(Lc,macroPtn(El),macroTerm(C),macroAction(B)).
  examineAction(A) where (Lc,T) ?= isValis(A) => mkValis(Lc,macroTerm(T)).
  examineAction(A) where (Lc,T) ?= isRaise(A) => mkRaise(Lc,macroTerm(T)).
  examineAction(A) where (Lc,T) ?= isContinue(A) => mkContinue(Lc,macroTerm(T)).
  examineAction(A) where (Lc,D,B) ?= isLetDef(A) => 
    mkLetDef(Lc,macroStmts(D),macroAction(B)).
  examineAction(A) where (Lc,D,B) ?= isLetRecDef(A) => 
    mkLetRecDef(Lc,macroStmts(D),macroAction(B)).
  examineAction(A) where (Lc,G,Cs) ?= isCase(A) => 
    mkCaseExp(Lc,macroTerm(G),Cs//macroCaseAction).
  examineAction(A) where (Lc,O,Els) ?= isRoundTerm(A) => 
    roundTerm(Lc,macroTerm(O),Els//macroTerm).
  examineAction(A) default => valof{
    reportError("cannot figure out action\n$(A)",locOf(A));
    valis A
  }

  macroCaseAction:(ast) => ast.
  macroCaseAction(A) => macroAst(A,.rule,examineCaseAction).

  examineCaseAction(A) where (Lc,Dflt,L,C,R) ?= isLambda(A) =>
    mkLambda(Lc,Dflt,macroPtn(L),macroOpt(C,macroCond),macroAction(R)).
  examineCaseAction(A) => valof{
    reportError("cannot figure out case action $(A)",locOf(A));
    valis A}

  macroTerm(A) => macroAst(A,.expression,examineTerm).

  examineTerm(A) where _ ?= isName(A) => A.
  examineTerm(A) where _ ?= isEnumSymb(A) => A.
  examineTerm(A) where .int(_,_) .= A => A.
  examineTerm(A) where .big(_,_) .= A => A.
  examineTerm(A) where .chr(_,_) .= A => A.
  examineTerm(A) where .num(_,_) .= A => A.
  examineTerm(A) where .str(_,_) .= A => A.
  examineTerm(A) where (Lc,L,R) ?= isTypeAnnotation(A) => 
    mkTypeAnnotation(Lc,macroTerm(L),macroType(R)).
  examineTerm(A) where (Lc,L,R) ?= isCoerce(A) => 
    mkCoercion(Lc,macroTerm(L),macroType(R)).
  examineTerm(A) where (Lc,R) ?= isCellRef(A) =>
    refCell(Lc,macroTerm(R)).
  examineTerm(A) where (Lc,R) ?= isRef(A) => 
    mkRef(Lc,macroTerm(R)).
  examineTerm(A) where (Lc,R) ?= isThunk(A) =>
    mkThunk(Lc,macroTerm(R)).
  examineTerm(A) where (Lc,R) ?= isThunkRef(A) =>
    mkThunkRef(Lc,macroTerm(R)).
  examineTerm(A) where (Lc,R) ?= isSuppress(A) =>
    mkSuppress(Lc,macroTerm(R)).
  examineTerm(A) where (Lc,T,E) ?= isInvoke(A) =>
    mkInvoke(Lc,macroTerm(T),E//macroTerm).
  examineTerm(A) where (Lc,D,B) ?= isLetDef(A) => 
    mkLetDef(Lc,macroStmts(D),macroTerm(B)).
  examineTerm(A) where (Lc,D,B) ?= isLetRecDef(A) =>
    mkLetRecDef(Lc,macroStmts(D),macroTerm(B)).
  examineTerm(A) where (Lc,D,B) ?= isComprehension(A) =>
    mkComprehension(Lc,macroTerm(D),macroTerm(B)).
  examineTerm(A) where (Lc,D,B) ?= isIotaComprehension(A) =>
    mkIotaComprehension(Lc,macroTerm(D),macroTerm(B)).
  examineTerm(A) where (Lc,B) ?= isTestComprehension(A) => 
    mkTestComprehension(Lc,macroCond(B)).
  examineTerm(A) where (Lc,F,E,Z,B) ?= isTotalizerComprehension(A) =>
    mkTotalizerComprehension(Lc,macroTerm(F),macroTerm(E),macroTerm(Z),macroTerm(B)).
  examineTerm(A) where (Lc,Els) ?= isMapLiteral(A) =>
    mkMapLiteral(Lc,Els//macroTerm).
  examineTerm(A) where (Lc,Op,[Ix]) ?= isSquareTerm(A) =>
    squareTerm(Lc,macroTerm(Op),[macroTerm(Ix)]).
  examineTerm(A) where (Lc,Els) ?= isTuple(A) => 
    rndTuple(Lc,Els//macroTerm).
  examineTerm(A) where (Lc,Els) ?= isSqTuple(A) => 
    sqTuple(Lc,Els//macroTerm).
  examineTerm(A) where (Lc,Els) ?= isBrTuple(A) => 
    brTuple(Lc,macroStmts(Els)).
  examineTerm(A) where (Lc,L,R) ?= isCons(A) => 
    mkCons(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isComma(A) =>
    mkComma(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isPair(A) => 
    mkPair(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isSequence(A) => 
    mkSequence(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isWhere(A) =>
    mkWhere(Lc,macroTerm(L),macroCond(R)).
  examineTerm(A) where (Lc,L,R) ?= isMatch(A) =>
    mkMatch(Lc,macroPtn(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isSearch(A) =>
    mkSearch(Lc,macroPtn(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isConjunct(A) =>
    mkConjunct(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isDisjunct(A) =>
    mkDisjunct(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,R) ?= isNegation(A) =>
    negated(Lc,macroTerm(R)).
  examineTerm(A) where (Lc,T,L,R) ?= isConditional(A) =>
    mkConditional(Lc,macroCond(T),macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,R) ?= isImplies(A) =>
    mkImplies(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,D,L,C,R) ?= isLambda(A) => 
    mkLambda(Lc,D,macroPtn(L),macroOpt(C,macroCond),macroTerm(R)).
  examineTerm(A) where (Lc,S) ?= isValof(A) && (VLc,[As])?=isBrTuple(S) =>
    mkValof(Lc,brTuple(VLc,[macroAction(As)])).
  examineTerm(A) where (Lc,B,E,Hs) ?= isTryCatch(A) =>
    mkTryCatch(Lc,macroTerm(B),macroType(E),Hs//macroLambda).
  examineTerm(A) where (Lc,T) ?= isRaise(A) =>
    mkRaise(Lc,macroTerm(T)).
  examineTerm(A) where (Lc,T) ?= isContinue(A) =>
    mkContinue(Lc,macroTerm(T)).
  examineTerm(A) where (Lc,B,E,Hs) ?= isTryHandle(A) =>
    mkTryHandle(Lc,macroTerm(B),macroType(E),Hs//macroLambda).
  examineTerm(A) where (Lc,S) ?= isFiberTerm(A) => 
    mkFiberTerm(Lc,macroAction(S)).
  examineTerm(A) where (Lc,S) ?= isGenerator(A) =>
    mkGenerator(Lc,macroAction(S)).
  examineTerm(A) where (Lc,Lb,S) ?= isLabeledTheta(A) => 
    mkQBrTerm(Lc,Lb,macroStmts(S)).
  examineTerm(A) where (Lc,Lb,S) ?= isBrTerm(A) =>
    mkBrTerm(Lc,Lb,macroStmts(S)).
  examineTerm(A) where (Lc,R,F,V) ?= isRecordUpdate(A) =>
    mkRecordUpdate(Lc,macroTerm(R),F,macroTerm(V)).
  examineTerm(A) where (Lc,O,Els) ?= isEnumCon(A) => 
    mkEnumCon(Lc,macroTerm(O),Els//macroTerm).
  examineTerm(A) where (Lc,O,Els) ?= isRoundTerm(A) => 
    roundTerm(Lc,macroTerm(O),Els//macroTerm).
  examineTerm(A) where (Lc,L,R) ?= isIndexTerm(A) =>
    mkIndexTerm(Lc,macroTerm(L),macroTerm(R)).
  examineTerm(A) where (Lc,L,F,R) ?= isSlice(A) => 
    ternary(Lc,"_slice",macroTerm(L),macroTerm(F),macroTerm(R)).
  examineTerm(A) where (Lc,R,F) ?= isFieldAcc(A) =>
    mkFieldAcc(Lc,macroTerm(R),F).
  examineTerm(A) where (Lc,R,F) ?= isTupleAcc(A) =>
    mkTupleAcc(Lc,macroTerm(R),F).
  examineTerm(A) where (Lc,E,Cs) ?= isCase(A) =>
    mkCaseExp(Lc,macroTerm(E),Cs//macroLambda).
  examineTerm(A) default => valof{
    reportError("cannot figure out expression\n$(A)",locOf(A));
    valis A
  }.

  macroCond:(ast) => ast.
  macroCond(C) => macroTerm(C).

  macroOpt:(option[ast],(ast)=>ast) =>option[ast].
  macroOpt(.none,_) => .none.
  macroOpt(.some(A),E) => .some(E(A)).

  macroLambda(A) => macroAst(A,.rule,examineLambda).

  examineLambda(A) where (Lc,D,L,C,R) ?= isLambda(A) =>
    mkLambda(Lc,D,macroPtn(L),macroOpt(C,macroTerm),macroTerm(R)).
  examineLambda(A) default => valof{
    reportError("cannot figure out case rule $(A)",locOf(A));
    valis A
  }

  macroPtn(A) => macroAst(A,.pattern,examinePtn).

  examinePtn(A) where _ ?= isName(A) => A.
  examinePtn(A) where .int(_,_) .= A => A.
  examinePtn(A) where .big(_,_) .= A => A.
  examinePtn(A) where .num(_,_) .= A => A.
  examinePtn(A) where .str(_,_) .= A => A.
  examinePtn(A) where .chr(_,_) .= A => A.
  examinePtn(A) where _ ?= isEnumSymb(A) => A.
  examinePtn(A) where (Lc,L,R) ?= isTypeAnnotation(A) =>
    mkTypeAnnotation(Lc,macroPtn(L),macroType(R)).
  examinePtn(A) where (Lc,Lb,S) ?= isBrTerm(A) => 
    mkBrTerm(Lc,Lb,macroStmts(S)).
  examinePtn(A) where (Lc,O,Els) ?= isRoundTerm(A) => 
    roundTerm(Lc,macroTerm(O),Els//macroPtn).
  examinePtn(A) where (Lc,O,Els) ?= isEnumCon(A) => 
    mkEnumCon(Lc,macroTerm(O),Els//macroPtn).
  examinePtn(A) where (Lc,Els) ?= isTuple(A) =>
    rndTuple(Lc,Els//macroPtn).
  examinePtn(A) where (Lc,Els) ?= isSqTuple(A) =>
    sqTuple(Lc,Els//macroPtn).
  examinePtn(A) where (Lc,L,R) ?= isCons(A) => 
    mkCons(Lc,macroPtn(L),macroPtn(R)).
  examinePtn(A) where (Lc,L,R) ?= isComma(A) =>
    mkComma(Lc,macroPtn(L),macroPtn(R)).
  examinePtn(A) where (Lc,L,R) ?= isPair(A) =>
    mkPair(Lc,macroPtn(L),macroPtn(R)).
  examinePtn(A) where (Lc,Els) ?= isBrTuple(A) => 
    brTuple(Lc,macroStmts(Els)).
  examinePtn(A) where (Lc,L,R) ?= isWhere(A) =>
    mkWhere(Lc,macroPtn(L),macroTerm(R)).
  examinePtn(A) default => valof{
    reportError("cannot figure out pattern\n$(A)",locOf(A));
    valis A
  }

  macroType(A) => macroAst(A,.typeterm,examineType).

  examineType(A) where _ ?= isName(A) => A.
  examineType(A) where (Lc,Op,Els) ?= isSquareTerm(A) => 
    squareTerm(Lc,Op,Els//macroType).
  examineType(A) where _ ?= isTypeFunVar(A) => A.
  examineType(A) where (Lc,L,R) ?= isDepends(A) =>
    mkDepends(Lc,L//macroType,R//macroType).
  examineType(A) where (Lc,L,R) ?= isConstructorType(A) =>
    mkConstructorType(Lc,macroType(L),macroType(R)).
  examineType(A) where (Lc,L,R) ?= isFunctionType(A) =>
    mkFunctionType(Lc,macroType(L),macroType(R)).
  examineType(A) where (Lc,L,R) ?= isContinuationType(A) =>
    mkContinuationType(Lc,macroType(L),macroType(R)).
  examineType(A) where (Lc,R) ?= isRef(A) =>
    mkRef(Lc,macroType(R)).
  examineType(A) where (Lc,R) ?= isTag(A) =>
    mkTag(Lc,macroType(R)).
  examineType(A) where (Lc,L,R) ?= isTypeLambda(A) =>
    mkTypeLambda(Lc,macroType(L),macroType(R)).
  examineType(A) where (Lc,Q,T) ?= isQuantified(A) =>
    reUQuant(Lc,Q//macroTypeVar,macroType(T)).
  examineType(A) where (Lc,Q,T) ?= isXQuantified(A) =>
    reXQuant(Lc,Q//macroTypeVar,macroType(T)).
  examineType(A) where (Lc,C,T) ?= isConstrained(A) =>
    reConstrain(C//macroConstraint,macroType(T)).
  examineType(A) where (Lc,Els) ?= isTuple(A) =>
    rndTuple(Lc,Els//macroType).
  examineType(A) where (Lc,Els) ?= isBrTuple(A) =>
    brTuple(Lc,Els//macroTypeDef).
  examineType(A) where (Lc,R,F) ?= isFieldAcc(A) =>
    mkFieldAcc(Lc,macroTerm(R),F).
  examineType(A) where (Lc,I) ?= isAsync(A) =>
    mkAsync(Lc,macroType(I)).
  examineType(A) where (Lc,Y) ?= isGeneratorType(A) =>
    mkGeneratorType(Lc,macroType(Y)).
  examineType(A) default => valof{
    reportError("cannot figure out type expression\n$(A)",locOf(A));
    valis A
  }

  macroTypeVar(A) => macroAst(A,.typeterm,examineTypeVar).

  examineTypeVar(A) where _ ?= isName(A) => A.
  examineTypeVar(A) where (Lc,L,R) ?= isBinary(A,"/") && _ ?= isName(L) && _ ?= isInt(R) => A.
  examineTypeVar(A) default => valof{
    reportError("cannot figure out type variable\n$(A)",locOf(A));
    valis A
  }
  
  macroConstraint(A) => macroAst(A,.constraint,examineConstraint).

  examineConstraint(A) where (Lc,Op,Els) ?= isSquareTerm(A) =>
    squareTerm(Lc,Op,Els//macroContractArg).
  examineConstraint(A) where (Lc,L,R) ?= isTypeExists(A) =>
    mkTypeExists(Lc,macroType(L),macroType(R)).
  examineConstraint(A) where (Lc,Nm,T) ?= isImplicit(A) =>
    mkImplicit(Lc,Nm,macroType(T)).
  examineConstraint(A) where (Lc,T) ?= isRaises(A) =>
    mkRaises(Lc,macroType(T)).
  examineConstraint(A) where (_,[El]) ?= isTuple(A) =>
    examineConstraint(El).
  examineConstraint(A) default => valof{
    reportError("cannot figure out constraint $(A)",locOf(A));
    valis A
  }

  macroContractArg(A) where (Lc,L,R) ?= isDepends(A) =>
    mkDepends(Lc,L//macroType,R//macroType).
  macroContractArg(A) => macroType(A).

  visibilityOf:(ast) => (ast,visibility).
  visibilityOf(A) => visib(A,.deFault).

  visib(A,_) where (_,I) ?= isPrivate(A) => visib(I,.priVate).
  visib(A,_) where (_,I) ?= isPublic(A) => visib(I,.pUblic).
  visib(A,Vz) default => (A,Vz).

  buildMain:(cons[ast])=>cons[ast].
  buildMain(Els) where (Lc,Tp) ?= head(lookForSignature(Els,"main")) &&
      ~_?=head(lookForSignature(Els,"_main")) =>
    synthesizeMain(Lc,Tp,Els).
  buildMain(Els) default => Els.

  lookForSignature:(cons[ast],string)=>cons[(option[locn],ast)].
  lookForSignature(Els,Nm) => {(Lc,Tp) | El in Els && (Lc,Nm,Vz,Tp)?=isTypeAnnot(El)}.

  isTypeAnnot(A) where (Lc,N,Tp) ?= isBinary(A,":") && (Nm,Vz) .= visibilityOf(N) && (_,Id)?=isName(Nm) =>
    .some((Lc,Id,Vz,Tp)).
  isTypeAnnot(A) where (Lc,I) ?= isPublic(A) &&
      (_,N,Tp) ?= isBinary(I,":") &&
      (_,Id) ?= isName(N) => .some((Lc,Id,.pUblic,Tp)).
  isTypeAnnot(_) default => .none.

  /*
  _main([A1,..,An]) => valof{
    if X1?=A1:?T1 then{
      if X2?=A2:?T2 then {
        ...
        valis main(X1,..,Xn)
      } else
      showMsg("cannot coerce $(A2) to T2")
    } else{
      showMsg("Cannot coerce $(A1) to T1")
    }
  }
*/

  synthesizeMain:(option[locn],ast,cons[ast])=>cons[ast].
  synthesizeMain(Lc,Tp,Defs) where (_,Lhs,Rhs) ?= isFunctionType(Tp) && (_,ElTps)?=isTuple(Lhs) => valof{
    (Action,As) = synthCoercion(Lc,ElTps,[]);
    
    MLhs = roundTerm(Lc,.nme(Lc,"_main"),[mkConsPtn(Lc,As)]);

--    showMsg("main action $(Action)");
    Valof = mkValof(Lc,brTuple(Lc,[Action]));
    Main = equation(Lc,MLhs,Valof);
    XX = genName(Lc,"XX");
    FallBack = equation(Lc,roundTerm(Lc,.nme(Lc,"_main"),[XX]),
      mkValof(Lc,brTuple(Lc,[
	    actionSeq(Lc,unary(Lc,"_logmsg",.str(Lc,"incorrect args, should be #(Lhs::string)")),
	      mkValis(Lc,unit(Lc)))])));
    Annot = mkTypeAnnotation(Lc,.nme(Lc,"_main"),equation(Lc,rndTuple(Lc,
	  [squareTerm(Lc,.nme(Lc,"cons"),[.nme(Lc,"string")])]),rndTuple(Lc,[])));
    valis [unary(Lc,"public",Annot),Main,FallBack,..Defs].
  }

/*
  if X?=A:?T then {
  valis main(X1,..,Xn)
  } else
  showMsg("cannot coerce $(A) to T")

*/  
  synthCoercion:(option[locn],cons[ast],cons[ast])=>(ast,cons[ast]).
  synthCoercion(_,[Tp,..Ts],Xs)  => valof{
    Lc = locOf(Tp);
    X = genName(Lc,"X");
    A = genName(Lc,"A");    
    PRhs = binary(Lc,":?",A,Tp);
    Tst = binary(Lc,"?=",X,PRhs); -- .some(X).=_coerce(A):T
    Emsg = unary(Lc,"showMsg",.str(Lc,"cannot coerce \$(#(A::string)) to #(Tp::string)"));
    (Inner,As) = synthCoercion(Lc,Ts,[X,..Xs]);
    valis (mkIfThenElse(Lc,Tst,Inner,Emsg),[A,..As])
  }
  synthCoercion(Lc,[],Xs) => 
    (mkValis(Lc,roundTerm(Lc,.nme(Lc,"main"),reverse(Xs))),[]).

  mkConsPtn:(option[locn],cons[ast]) => ast.
  mkConsPtn(Lc,[]) => enum(Lc,"nil").
  mkConsPtn(Lc,[E,..Es]) => mkCon(Lc,"cons",[E,mkConsPtn(Lc,Es)]).

  synthesizeCoercions:(cons[ast],option[locn])=> (ast,cons[ast]).
  synthesizeCoercions([],Lc) => (enum(Lc,"nil"),.nil).
  synthesizeCoercions([T,..Ts],Lc) where Nm .= genName(Lc,"X") &&
      (RV,RC) .= synthesizeCoercions(Ts,Lc) =>
    (binary(Lc,"cons",Nm,RV),[binary(Lc,":",unary(Lc,"_optval",unary(Lc,"_coerce",Nm)),T),..RC]).

}
