star.compiler.macro{
  import star.
  import star.sort.

  import star.compiler.ast.
  import star.compiler.ast.display.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.macro.infra.
  import star.compiler.wff.

  macros:map[string,cons[(macroContext,macroRule)]].
  macros = [ "::=" -> [(.statement,macroAlgebraic)],
    ":=" -> [(.statement,macroAssignDef)],    
    "[]" -> [(.pattern,makeSeqPtn),
      (.expression,macroListComprehension),
      (.expression,makeSeqExp)],
    "::" -> [(.expression,macroCoercion)],
    ":?" -> [(.expression,macroCoercion)],
    "{}" -> [(.expression,macroComprehension)],
    "do" -> [(.expression,macroDo)],
    "action" -> [(.expression,actionMacro)],
    "task" -> [(.expression,taskAction)],
    "valof" -> [(.expression,performMacro)],
    "__pkg__" -> [(.expression,pkgNameMacro)],
    "-" -> [(.expression, uMinusMacro),(.pattern, uMinusMacro)],
    "^=" -> [(.expression, optionMatchMacro)],
    "^" -> [(.pattern, optionPtnMacro)]].

  applyRules:(ast,macroContext,macroState,cons[(macroContext,macroRule)],reports) =>
    either[reports,macroState].
  applyRules(A,_,St,[],_) => either(St).
  applyRules(A,Cxt,St,[(Cxt,R),..Rls],Rp) => do{
    Rslt <- R(A,Cxt,Rp);
    if .inactive.=Rslt then
      applyRules(A,Cxt,St,Rls,Rp)
    else
    valis Rslt
  }
  applyRules(A,Cxt,St,[_,..Rls],Rp) => 
    applyRules(A,Cxt,St,Rls,Rp).

  macroAst:(ast,macroContext,(ast,reports)=>either[reports,ast],reports) => either[reports,ast].
  macroAst(A,Cxt,Examine,Rp) where Rules^=macros[macroKey(A)] => do{
    Rslt <- applyRules(A,Cxt,.inactive,Rules,Rp);

    if active(T).=Rslt then
      macroAst(T,Cxt,Examine,Rp)
    else
    Examine(A,Rp)
  }
  macroAst(A,_,Examine,Rp) default => Examine(A,Rp).

  public macroPkg:(ast,reports) => either[reports,ast].
  macroPkg(A,Rp) => macroAst(A,.package,examinePkg,Rp).

  examinePkg(A,Rp) where (Lc,O,Els) ^= isBrTerm(A) => do{
    Ss <- macroStmts(Els,Rp);
    valis mkLabeledTheta(Lc,O,buildMain(Ss))
  }

  macroStmts:(cons[ast],reports)=>either[reports,cons[ast]].
  macroStmts(Ss,Rp) => do{
    SS <- seqmap((S)=>macroStmt(S,Rp),Ss);
    valis reverse(flattenStmts(SS,[]))
  }

  flattenStmts([],So) => So.
  flattenStmts([A,..As],So) where (_,Els) ^= isBrTuple(A) =>
    flattenStmts(As,flattenStmts(Els,So)).
  flattenStmts([A,..As],So) => flattenStmts(As,[A,..So]).

  disThroughGroup(A,F) where (ELc,Els) ^= isBrTuple(A) =>
    brTuple(ELc,Els//F).
  disThroughGroup(A,F) default => F(A).

  macroStmt:(ast,reports) => either[reports,ast].
  macroStmt(A,Rp) => macroAst(A,.statement,examineStmt,Rp).

  examineStmt:(ast,reports) => either[reports,ast].
  examineStmt(A,Rp) where (Lc,L,Vz,R) ^= isTypeAnnot(A) => do{
    RR <- macroType(R,Rp);
    valis reveal(typeAnnotation(Lc,nme(Lc,L),RR),Vz)
  }
  examineStmt(A,Rp) where (Lc,V,T) ^= isTypeStatement(A) => do{
    TT <- macroType(T,Rp);
    valis mkTypeStatement(Lc,V,TT)
  }
  examineStmt(A,Rp) where (Lc,R) ^= isPublic(A) => do{
    RR <- macroStmt(R,Rp);
    valis disThroughGroup(RR,(E)=>unary(Lc,"public",E))
  }
  examineStmt(A,Rp) where (Lc,R) ^= isPrivate(A) => do{
    RR <- macroStmt(R,Rp);
    valis disThroughGroup(RR,(E)=>unary(Lc,"private",E))
  }
  examineStmt(A,Rp) where _ ^= isImport(A) =>
    either(A).
  examineStmt(A,Rp) where _ ^= isOpen(A) =>
    either(A).
  examineStmt(A,Rp) where (Lc,L,R) ^= isDefn(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkDefn(Lc,LL,RR)
  }
  examineStmt(A,Rp) where (Lc,L,R) ^= isAssignment(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkAssignment(Lc,LL,RR)
  }
  examineStmt(A,Rp) where (Lc,Nm,Deflt,L,C,R) ^= isEquation(A) => do{
    LL <- macroPtn(L,Rp);
    CC <- macroOpt(C,macroCond,Rp);
    RR <- macroTerm(R,Rp);
    valis mkEquation(Lc,Nm,Deflt,LL,CC,RR)
  }
  examineStmt(A,Rp) where isConstructorStmt(A) =>
    macroType(A,Rp).
  examineStmt(A,Rp) where _ ^= isTypeExistsStmt(A) =>
    macroType(A,Rp).
  examineStmt(A,Rp) where _ ^= isTypeFunStmt(A) =>
    macroType(A,Rp).
  examineStmt(A,Rp) where (Lc,L,Els) ^= isContractStmt(A) => do{
    LT <- macroType(L,Rp);
    REls <- macroStmts(Els,Rp);
    valis mkContractStmt(Lc,LT,REls)
  }
  examineStmt(A,Rp) where (Lc,Q,Cx,Tp,Exp) ^= isImplementationStmt(A) => do{
    QQ <- seqmap((V)=>macroType(V,Rp),Q);
    CCx <- seqmap((T)=>macroType(T,Rp),Cx);
    TT <- macroType(Tp,Rp);
    EE <- macroTerm(Exp,Rp);
    valis mkImplementationStmt(Lc,QQ,CCx,TT,EE)
  }
  examineStmt(A,Rp) where _ ^= isAnnotation(A) =>
    either(A).
  examineStmt(A,Rp) where (Lc,Els) ^= isBrTuple(A) => do{
    NEls <- macroStmts(Els,Rp);
    valis brTuple(Lc,NEls)
  }
  examineStmt(A,Rp) => other(
    reportError(Rp,"cannot figure out statement $(A), key=$(macroKey(A))",locOf(A))).

  macroTerm(A,Rp) => macroAst(A,.expression,examineTerm,Rp).

  examineTerm(A,_) where _ ^= isName(A) => either(A).
  examineTerm(A,_) where _ ^= isEnum(A) => either(A).
  examineTerm(A,Rp) where (Lc,L,R) ^= isTypeAnnotation(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroType(R,Rp);
    valis typeAnnotation(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isCoerce(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroType(R,Rp);
    valis mkCoercion(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,R) ^= isCellRef(A) => do{
    RR <- macroTerm(R,Rp);
    valis refCell(Lc,RR)
  }
  examineTerm(A,Rp) where (Lc,R) ^= isRef(A) => do{
    RR <- macroTerm(R,Rp);
    valis mkRef(Lc,RR)
  }
  examineTerm(A,Rp) where (Lc,D,B) ^= isLetDef(A) => do{
    DD <- macroStmts(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkLetDef(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,D,B) ^= isQLetDef(A) => do{
    DD <- macroStmts(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkQLetDef(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,S) ^= isTheta(A) => do{
    SS <- macroStmts(S,Rp);
    valis mkTheta(Lc,SS)
  }
  examineTerm(A,Rp) where (Lc,S) ^= isQTheta(A) => do{
    SS <- macroStmts(S,Rp);
    valis mkQTheta(Lc,SS)
  }
  examineTerm(A,Rp) where (Lc,Lb,S) ^= isLabeledTheta(A) => do{
    SS <- macroStmts(S,Rp);
    valis mkBrTerm(Lc,Lb,SS)
  }
  examineTerm(A,Rp) where (Lc,Lb,S) ^= isLabeledRecord(A) => do{
    SS <- macroStmts(S,Rp);
    valis mkQBrTerm(Lc,Lb,SS)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isRecordUpdate(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkRecordUpdate(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,O,Els) ^= isRoundTerm(A) => do{
    OO <- macroTerm(O,Rp);
    EE <- seqmap((E)=>macroTerm(E,Rp),Els);
    valis roundTerm(Lc,OO,EE)
  }
  examineTerm(A,Rp) where (Lc,Els) ^= isTuple(A) => do{
    EE <- seqmap((E)=>macroTerm(E,Rp),Els);
    valis rndTuple(Lc,EE)
  }
  examineTerm(A,Rp) where (Lc,Els) ^= isSqTuple(A) => do{
    EE <- seqmap((E)=>macroTerm(E,Rp),Els);
    valis sqTuple(Lc,EE)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isCons(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkCons(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isComma(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkComma(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isIndex(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    if (_,K,V) ^= isBinary(RR,"->") then
      valis ternary(Lc,"_put",LL,K,V)
    else if (_,K) ^= isNegation(RR) then
      valis binary(Lc,"_remove",LL,K)
    else
    valis binary(Lc,"_index",LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,F,R) ^= isSlice(A) => do{
    LL <- macroTerm(L,Rp);
    FF <- macroTerm(F,Rp);
    RR <- macroTerm(R,Rp);
    valis ternary(Lc,"_slice",LL,FF,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isMatch(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkMatch(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isOptionMatch(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkMatch(Lc,unary(Lc,"some",LL),RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isSearch(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkSearch(Lc,LL,RR)
  }
  examineTerm(A,Rp) where _ ^= isConjunct(A) =>
    macroCond(A,Rp).
  examineTerm(A,Rp) where _ ^= isDisjunct(A) =>
    macroCond(A,Rp).
  examineTerm(A,Rp) where _ ^= isImplies(A) => 
    macroCond(A,Rp).
  examineTerm(A,Rp) where _ ^= isNegation(A) => 
    macroCond(A,Rp).
  examineTerm(A,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    TT <- macroCond(T,Rp);
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkConditional(Lc,TT,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isWhere(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroCond(R,Rp);
    valis mkWhere(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,D,L,C,R) ^= isLambda(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    CC <- macroOpt(C,macroCond,Rp);
    valis mkLambda(Lc,D,LL,CC,RR)
  } 
  examineTerm(A,Rp) where (Lc,R) ^= isPromotion(A) => do{
    RR <- macroTerm(R,Rp);
    valis mkPromotion(Lc,RR)
  }
  examineTerm(A,Rp) where (Lc,R) ^= isValof(A) => do{
    RR <- macroTerm(R,Rp);
    valis unary(Lc,"_perform",RR)
  }
  examineTerm(A,Rp) where (Lc,R,F) ^= isFieldAcc(A) => do{
    RR <- macroTerm(R,Rp);
    valis mkFieldAcc(Lc,RR,F)
  }
  examineTerm(A,Rp) where (Lc,E,Cs) ^= isCase(A) => do{
    EE <- macroTerm(E,Rp);
    CC <- seqmap((C)=>macroLambda(C,Rp),Cs);
    valis mkCaseExp(Lc,EE,CC)
  }
  examineTerm(A,Rp) where int(_,_) .= A => either(A).
  examineTerm(A,Rp) where num(_,_) .= A => either(A).
  examineTerm(A,Rp) where str(_,_) .= A => either(A).
  examineTerm(A,Rp) default =>
    other(reportError(Rp,"cannot figure out expression $(A), key=$(macroKey(A))",locOf(A))).

  macroIterate:(ast,(ast,reports)=>either[reports,ast],reports) => either[reports,ast].
  macroIterate(A,E,Rp) => do{
    AA <- E(A,Rp);

    if isIterable(AA) then{
      Itr <- makeIterableGoal(AA,Rp);
      valis Itr
    } else
    valis AA
  }

  hideCond:(ast) => ast.
  hideCond(A) where (Lc,L,R) ^= isSearch(A) =>
    binary(Lc,"@in",L,R).
  hideCond(A) where (Lc,L,R) ^= isConjunct(A) =>
    binary(Lc,"@&&",hideCond(L),hideCond(R)).
  hideCond(A) where (Lc,L,R) ^= isDisjunct(A) =>
    binary(Lc,"@||",hideCond(L),hideCond(R)).
  hideCond(A) where (Lc,L,R) ^= isImplies(A) =>
    binary(Lc,"@*>",hideCond(L),hideCond(R)).
  hideCond(A) where (Lc,R) ^= isNegation(A) =>
    unary(Lc,"@!",hideCond(R)).
  hideCond(A) where (Lc,[E]) ^= isTuple(A) => hideCond(E).
  hideCond(A) default => A.

  revealCond:(ast) => ast.
  revealCond(A) where (Lc,L,R) ^= isBinary(A,"@in") =>
    mkSearch(Lc,L,R).
  revealCond(A) where (Lc,L,R) ^= isBinary(A,"@&&") =>
    mkConjunct(Lc,revealCond(L),revealCond(R)).
  revealCond(A) where (Lc,L,R) ^= isBinary(A,"@||") =>
    mkDisjunct(Lc,revealCond(L),revealCond(R)).
  revealCond(A) where (Lc,L,R) ^= isBinary(A,"@*>") =>
    mkImplies(Lc,revealCond(L),revealCond(R)).
  revealCond(A) where (Lc,T,L,R) ^= isTernary(A,"@?") =>
    mkConditional(Lc,revealCond(T),revealCond(L),revealCond(R)).
  revealCond(A) where (Lc,R) ^= isUnary(A,"@!") =>
    negated(Lc,revealCond(R)).
  revealCond(A) default => A.

  macroCond:(ast,reports) => either[reports,ast].
  macroCond(C,Rp) => do{
    HC .= hideCond(C);
    C1 <- macroTerm(HC,Rp);
    CC .= revealCond(C1);

    if isIterable(C) then {
      Itr <- makeIterableGoal(CC,Rp);
      valis Itr
    } else{
      valis CC
    }
  }

  macroOpt:(option[ast],(ast,reports)=>either[reports,ast],reports) =>
    either[reports,option[ast]].
  macroOpt(.none,_,_) => either(.none).
  macroOpt(some(A),E,Rp) => do{
    MA <- E(A,Rp);
    valis some(MA)
  }

  macroLambda(A,Rp) where (Lc,D,L,C,R) ^= isLambda(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    CC <- macroOpt(C,macroTerm,Rp);
    valis mkLambda(Lc,D,LL,CC,RR)
  }

  macroPtn(A,Rp) => macroAst(A,.pattern,examinePtn,Rp).

  examinePtn(A,_) where _ ^= isName(A) => either(A).
  examinePtn(A,Rp) where int(_,_) .= A => either(A).
  examinePtn(A,Rp) where num(_,_) .= A => either(A).
  examinePtn(A,Rp) where str(_,_) .= A => either(A).
  examinePtn(A,_) where _ ^= isName(A) => either(A).
  examinePtn(A,_) where _ ^= isEnum(A) => either(A).
  examinePtn(A,Rp) where (Lc,L,R) ^= isTypeAnnotation(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroType(R,Rp);
    valis typeAnnotation(Lc,LL,RR)
  }
  examinePtn(A,Rp) where (Lc,Lb,S) ^= isLabeledRecord(A) => do{
    SS <- macroStmts(S,Rp);
    valis mkQBrTerm(Lc,Lb,SS)
  }
  examinePtn(A,Rp) where (Lc,O,Els) ^= isRoundTerm(A) => do{
    OO <- macroTerm(O,Rp);
    EE <- seqmap((E)=>macroPtn(E,Rp),Els);
    valis roundTerm(Lc,OO,EE)
  }
  examinePtn(A,Rp) where (Lc,Els) ^= isTuple(A) => do{
    EE <- seqmap((E)=>macroPtn(E,Rp),Els);
    valis rndTuple(Lc,EE)
  }
  examinePtn(A,Rp) where (Lc,Els) ^= isSqTuple(A) => do{
    EE <- seqmap((E)=>macroPtn(E,Rp),Els);
    valis sqTuple(Lc,EE)
  }
  examinePtn(A,Rp) where (Lc,L,R) ^= isCons(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroPtn(R,Rp);
    valis mkCons(Lc,LL,RR)
  }
  examinePtn(A,Rp) where (Lc,L,R) ^= isComma(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroPtn(R,Rp);
    valis mkComma(Lc,LL,RR)
  }
  examinePtn(A,Rp) where (Lc,L,R) ^= isWhere(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkWhere(Lc,LL,RR)
  }
  examinePtn(A,Rp) where (Lc,R) ^= isPromotion(A) => do{
    RR <- macroPtn(R,Rp);
    valis mkPromotion(Lc,RR)
  }
  examinePtn(A,Rp) default =>
    other(reportError(Rp,"cannot figure out pattern $(A), key=$(macroKey(A))",locOf(A))).

  macroType(A,Rp) => macroAst(A,.typeterm,examineType,Rp).

  examineType(A,_) where _ ^= isName(A) => either(A).
  examineType(A,Rp) where (Lc,Op,Els) ^= isSquareTerm(A) => do{
    NEls <- seqmap((E)=>macroType(E,Rp),Els);
    valis squareTerm(Lc,Op,NEls)
  }
  examineType(A,Rp) where (Lc,L,R) ^= isConstructorType(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkConstructorType(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ^= isFunctionType(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkFunctionType(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ^= isBinary(A,"->>") => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis binary(Lc,"->>",NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ^= isComma(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis binary(Lc,",",NL,NR)
  }
  examineType(A,Rp) where (Lc,R) ^= isRef(A) => do{
    NR <- macroType(R,Rp);
    valis mkRef(Lc,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ^= isTypeLambda(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkTypeLambda(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,L,R) ^= isTypeExists(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkTypeExists(Lc,NL,NR)
  }
  examineType(A,Rp) where (Lc,Q,T) ^= isQuantified(A) => do{
    NT <- macroType(T,Rp);
    valis reUQuant(Lc,Q,NT)
  }
  examineType(A,Rp) where (Lc,Q,T) ^= isXQuantified(A) => do{
    NT <- macroType(T,Rp);
    valis reXQuant(Lc,Q,NT)
  }
  examineType(A,Rp) where (Lc,C,T) ^= isConstrained(A) => do{
    NC <- seqmap((X)=>macroConstraint(X,Rp),C);
    NT <- macroType(T,Rp);
    valis reConstrain(NC,NT)
  }
  examineType(A,Rp) where (Lc,Els) ^= isTuple(A) => do{
    NC <- seqmap((X)=>macroType(X,Rp),Els);
    valis rndTuple(Lc,NC)
  }
  examineType(A,Rp) where (Lc,Els) ^= isBrTuple(A) => do{
    NC <- macroStmts(Els,Rp);
    valis brTuple(Lc,NC)
  }
  examineType(A,Rp) where (Lc,R,F) ^= isFieldAcc(A) => do{
    RR <- macroTerm(R,Rp);
    valis mkFieldAcc(Lc,RR,F)
  }

  examineType(A,Rp) default =>
    other(reportError(Rp,"cannot figure out type expression $(A), key=$(macroKey(A))",locOf(A))).

  macroConstraint(A,Rp) => macroAst(A,.constraint,examineConstraint,Rp).

  examineConstraint(A,Rp) where (Lc,Op,Els) ^= isSquareTerm(A) => do{
    NEls <- seqmap((E)=>macroType(E,Rp),Els);
    valis squareTerm(Lc,Op,NEls)
  }
  examineConstraint(A,Rp) where (Lc,L,R) ^= isTypeExists(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkTypeExists(Lc,NL,NR)
  }

  macroAlgebraic(St,.statement,Rp) where (Lc,Vz,Q,Cx,H,R) ^= isAlgebraicTypeStmt(St) => do{
    Rslt <- makeAlgebraic(Lc,Vz,Q,Cx,H,R,Rp);
    valis active(brTuple(Lc,Rslt))
  }
  macroAlgebraic(_,_,_) default => either(.inactive).

  /*
  * We generate auto implementations of fields declared for an algebraic type
  * declaration
  *
  * RT ::= rc{F:FT..}
  *
  * becomes
  *
  * RT <~ {}
  * auto_contract '$F'[RT->>FT] => {.
  *  '$F'(rc(_,..,F,...,_)) => F
  *.}
  *
  * where auto_contract is an implementation statement that automatically 
  * induces a contract -- with the empty prefix
  */

  makeAlgebraic:(locn,visibility,cons[ast],cons[ast],ast,ast,reports) =>
    either[reports,cons[ast]].
  makeAlgebraic(Lc,Vz,Q,Cx,H,R,Rp) => do{
    Nm .= typeName(H);
    (Qs,Xs,Face) <- algebraicFace(R,Q,[],Rp);
    TpExSt .= reveal(reUQuant(Lc,Qs,reConstrain(Cx,binary(Lc,"<~",H,reXQuant(Lc,Xs,brTuple(Lc,sort(Face,compEls)))))),Vz);
    Cons <- buildConstructors(R,Q,Cx,H,Vz,Rp);
    Ixx .= buildConIndices(R,[]);
--    logMsg("constructor indices $(Ixx)");
    valis [TpExSt,..Cons]
  }

  algebraicFace:(ast,cons[ast],cons[ast],reports) =>
    either[reports,(cons[ast],cons[ast],cons[ast])].
  algebraicFace(A,Qs,Xs,Rp) where (_,L,R) ^= isBinary(A,"|") => do{
    (Q1,X1,Lhs) <- algebraicFace(L,Qs,Xs,Rp);
    (Qx,Xx,Rhs) <- algebraicFace(R,Q1,X1,Rp);
    Fs <- combineFaces(Lhs,Rhs,Rp);
    valis (Qx,Xx,Fs)
  }
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_,_) ^= isRoundTerm(A) =>
    either((Qs,Xs,[])).
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_) ^= isEnum(A) => either((Qs,Xs,[])).
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_,Els) ^= isBrTerm(A) =>
    either((Qs,Xs,Els)).
  algebraicFace(A,Qs,Xs,Rp) where (_,I) ^= isPrivate(A) =>
    algebraicFace(I,Qs,Xs,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,I) ^= isPublic(A) =>
    algebraicFace(I,Qs,Xs,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,X0,I) ^= isXQuantified(A) =>
    algebraicFace(I,Qs,Xs\/X0,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,A0,I) ^= isQuantified(A) =>
    algebraicFace(I,Qs\/A0,Xs,Rp).
  algebraicFace(A,_,_,Rp) default =>
    other(reportError(Rp,"invalid case in algebraic type",locOf(A))).

  combineFaces([],F2,Rp) => either(F2).
  combineFaces(F1,[],Rp) => either(F1).
  combineFaces([F,..Fs],Gs,Rp) where (Lc,Id,_,Tp) ^= isTypeAnnot(F) => do{
    G1 <- mergeField(Lc,Id,Tp,Gs,Rp);
    Fs1 <- combineFaces(Fs,G1,Rp);
    valis [F,..Fs1]
  }

  mergeField(_,_,_,[],_) => either([]).
  mergeField(Lc,Id,Tp,[A,..As],Rp) => do{
    if (Lc2,Id,_,Tp2) ^= isTypeAnnot(A) then {
      if Tp==Tp2 then
	valis As
      else
      throw reportError(Rp,"type associated with $(Id) at $(Lc) incompatible with $(Tp2)",Lc2)
    } else{
      A1 <- mergeField(Lc,Id,Tp,As,Rp);
      valis [A,..A1]
    }
  }

  buildConIndices:(ast,map[string,map[string,integer]]) => map[string,map[string,integer]].
  buildConIndices(A,Ixx) where (Lc,L,R) ^= isBinary(A,"|") =>
    buildConIndices(L,buildConIndices(R,Ixx)).
  buildConIndices(A,Ixx) where (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) =>
    Ixx[Nm->fst(foldLeft((El,(Mp,Ix)) => buildConIx(El,Mp,Ix),([],0),
	  sort(Els,compEls)))].
  buildConIndices(_,Ixx) default => Ixx.

  buildConIx:(ast,map[string,integer],integer)=>(map[string,integer],integer).
  buildConIx(El,Mp,Ix) where (_,Id,_,_) ^= isTypeAnnot(El) => (Mp[Id->Ix],Ix+1).
  buildConIx(_,Mp,Ix) default => (Mp,Ix).

  buildConstructors:(ast,cons[ast],cons[ast],ast,visibility,reports)=>either[reports,cons[ast]].
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,L,R) ^= isBinary(A,"|") => do{
	Dfs1 <- buildConstructors(L,Qs,Cx,Tp,Vz,Rp);
	Dfs2 <- buildConstructors(R,Qs,Cx,Tp,Vz,Rp);
	valis Dfs1++Dfs2
      }.
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",reXQuant(Lc,XQs,
		  reConstrain(XCx,brTuple(Lc,sort(Els,compEls)))),Tp)))).
      } in either([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isRoundCon(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,Els),Tp)))).
      } in either([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm) ^= isEnumSymb(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,[]),Tp)))).
      } in either([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,_,Rp) where
      (_,I) ^= isPrivate(A) => 
    buildConstructors(I,Qs,Cx,Tp,.priVate,Rp).
  buildConstructors(A,Qs,Cx,Tp,_,Rp) where
      (_,I) ^= isPublic(A) => 
    buildConstructors(I,Qs,Cx,Tp,.pUblic,Rp).
  buildConstructors(A,_,_,_,_,Rp) =>
    other(reportError(Rp,"cannot fathom constructor $(A)",locOf(A))).

  compEls:(ast,ast)=>boolean.
  compEls(A,B) where
      (_,N1,_,_) ^= isTypeAnnot(A) &&
      (_,N2,_,_) ^= isTypeAnnot(B) => N1<N2.
  compEls(_,_) default => .false.

  reveal(A,.priVate) => unary(locOf(A),"private",A).
  reveal(A,.pUblic) => unary(locOf(A),"public",A).
  reveal(A,_) default => A.

  visibilityOf:(ast) => (ast,visibility).
  visibilityOf(A) => visib(A,.deFault).

  visib(A,_) where (_,I) ^= isPrivate(A) => visib(I,.priVate).
  visib(A,_) where (_,I) ^= isPublic(A) => visib(I,.pUblic).
  visib(A,Vz) default => (A,Vz).

  isBraceCon:(ast) => option[(locn,string,cons[ast],cons[ast],cons[ast])].
  isBraceCon(A) => isCon(A,isBrTerm).

  isRoundCon:(ast) => option[(locn,string,cons[ast],cons[ast],cons[ast])].
  isRoundCon(A) => isCon(A,isRoundTerm).

  isCon:(ast,(ast)=>option[(locn,ast,cons[ast])]) => option[(locn,string,cons[ast],cons[ast],cons[ast])].
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
  isCon(_,_) default => .none.
    
  makeSeqPtn(A,.pattern,Rp) where (Lc,Els) ^= isSqTuple(A) =>
    either(active(macroSquarePtn(Lc,Els))).
  makeSeqPtn(_,_,_) => either(.inactive).

  makeSeqExp(A,.expression,Rp) where (Lc,Els) ^= isSqTuple(A) && ~_^=isListComprehension(A)=>
    either(active(macroSquareExp(Lc,Els))).
  makeSeqExp(_,_,_) => either(.inactive).

  macroListComprehension(A,.expression,Rp) where (Lc,B,C) ^= isListComprehension(A) => do{
    Q <- makeAbstraction(Lc,B,C,Rp);
    valis active(binary(Lc,":",Q,squareTerm(Lc,nme(Lc,"cons"),[anon(Lc)])))
  }
  macroListComprehension(_,_,_) => either(.inactive).

  macroComprehension(A,.expression,Rp) where (Lc,B,C) ^= isComprehension(A) => do{
    Q <- makeAbstraction(Lc,B,C,Rp);
    valis active(Q)
  }
  macroComprehension(_,_,_) => either(.inactive).
  
  public macroSquarePtn:(locn,cons[ast]) => ast.
  macroSquarePtn(Lc,Els) =>
    macroListEntries(Lc,Els,(Lx)=>mkWhereTest(Lx,"_eof"),
      (Lx,H,T) => mkWherePtn(Lx,tpl(Lx,"()",[H,T]),nme(Lx,"_hdtl"))).

  public macroSquareExp:(locn,cons[ast]) => ast.
  macroSquareExp(Lc,Els) =>
    macroListEntries(Lc,Els,(Lx)=>nme(Lx,"_nil"),
      (Lx,H,T) => binary(Lx,"_cons",H,T)).

  macroListEntries:(locn,cons[ast],(locn)=>ast,(locn,ast,ast)=>ast) => ast.
  macroListEntries(Lc,[],End,_) => End(Lc).
  macroListEntries(_,[Cns],_,Hed) where (Lc,H,T) ^= isCons(Cns) =>
    Hed(Lc,H,T).
  macroListEntries(Lc,[El,..Rest],Eof,Hed) =>
    Hed(Lc,El,macroListEntries(Lc,Rest,Eof,Hed)).

  macroCoercion(A,.expression,Rp) where (Lc,L,R) ^= isCoerce(A) =>
    either(active(typeAnnotation(Lc,unary(Lc,"_optval",unary(Lc,"_coerce",L)),R))).
  macroCoercion(A,.expression,Rp) where (Lc,L,R) ^= isOptCoerce(A) =>
    either(active(typeAnnotation(Lc,unary(Lc,"_coerce",L),sqUnary(Lc,"option",R)))).
  macroCoercion(_,_,_) => either(.inactive).
  
  public reconstructDisp:(ast)=>ast.
  reconstructDisp(C) where (Lc,Ex,Tp) ^= isCoerce(C) && (_,"string") ^= isName(Tp) => let{
    flat:(ast,cons[string])=>cons[string].
    flat(SS,So) where (_,Tx) ^= isUnary(SS,"ss") && (_,Txt) ^= isStr(Tx) => [Txt,..So].
    flat(SS,So) where (_,Tx) ^= isUnary(SS,"ss") => ["$(Tx)",..So].
    flat(E,So) where (_,Sq) ^= isUnary(E,"ssSeq") && (_,L) ^= isSqTuple(Sq) => fltList(L,So).
    flat(E,So) where (_,D) ^= isUnary(E,"disp") => ["\$",D::string,..So].
    flat(E,So) default => [disp(E)::string,..So].

    fltList(L,So) => foldRight((E,X)=>flat(E,X),So,L).
    
  } in str(Lc,_str_multicat(flat(Ex,[]))).
  reconstructDisp(A) where Lc.=locOf(A) => str(Lc,A::string).

  public buildMain:(cons[ast])=>cons[ast].
  buildMain(Els) where (Lc,Tp) ^= head(lookForSignature(Els,"main")) &&
      ~_^=head(lookForSignature(Els,"_main")) =>
    synthesizeMain(Lc,Tp,Els).
  buildMain(Els) default => Els.

  lookForSignature:(cons[ast],string)=>cons[(locn,ast)].
  lookForSignature(Els,Nm) => [(Lc,Tp) | El in Els && (Lc,Nm,Vz,Tp)^=isTypeAnnot(El)].

  isTypeAnnot(A) where (Lc,N,Tp) ^= isBinary(A,":") && (Nm,Vz) .= visibilityOf(N) && (_,Id)^=isName(Nm) =>
    some((Lc,Id,Vz,Tp)).
  isTypeAnnot(A) where (Lc,I) ^= isPublic(A) &&
      (_,N,Tp) ^= isBinary(I,":") &&
      (_,Id) ^= isName(N) => some((Lc,Id,.pUblic,Tp)).
  isTypeAnnot(_) default => .none.

  synthesizeMain:(locn,ast,cons[ast])=>cons[ast].
  synthesizeMain(Lc,Tp,Defs) where (_,Lhs,Rhs) ^= isFunctionType(Tp) && (_,ElTps)^=isTuple(Lhs) => valof action{
    (Vs,Cs) .= synthesizeCoercions(ElTps,Lc);
    MLhs .= roundTerm(Lc,nme(Lc,"_main"),[Vs]);
    MRhs .= roundTerm(Lc,nme(Lc,"main"),Cs);
    Main .= equation(Lc,MLhs,unary(Lc,"_perform",MRhs));
    Annot .= binary(Lc,":",nme(Lc,"_main"),equation(Lc,rndTuple(Lc,[squareTerm(Lc,nme(Lc,"cons"),[nme(Lc,"string")])]),rndTuple(Lc,[])));
    valis [unary(Lc,"public",Annot),Main,..Defs].
  }

  synthesizeCoercions:(cons[ast],locn)=> (ast,cons[ast]).
  synthesizeCoercions([],Lc) => (enum(Lc,"nil"),.nil).
  synthesizeCoercions([T,..Ts],Lc) where Nm .= genName(Lc,"X") &&
      (RV,RC) .= synthesizeCoercions(Ts,Lc) =>
    (binary(Lc,"cons",Nm,RV),[binary(Lc,":",unary(Lc,"_optval",unary(Lc,"_coerce",Nm)),T),..RC]).

  -- Temporary
  public isSimpleAction:(ast)=>boolean.
  isSimpleAction(A) where (_,L,R) ^= isActionSeq(A) =>
    isSimpleAction(L) && isSimpleAction(R).
  isSimpleAction(A) where _ ^= isValis(A) => .true.
  isSimpleAction(A) where _ ^= isThrow(A) => .true.
  isSimpleAction(A) where (_,I) ^= isPerform(A) => isSimpleAction(I).
  isSimpleAction(A) where (_,[St]) ^= isBrTuple(A) => isSimpleAction(St).
  isSimpleAction(A) where (Lc,L,R) ^= isBind(A) => .true.
  isSimpleAction(A) where (Lc,L,R) ^= isMatch(A) => .true.
  isSimpleAction(A) where (Lc,L,R) ^= isOptionMatch(A) => .true.
  isSimpleAction(A) where (_,B,H) ^= isTryCatch(A) =>
    isSimpleAction(B) && ((_,[St])^=isBrTuple(H) ? isSimpleAction(St) || .true).
  isSimpleAction(A) where (_,G,Cs) ^= isCase(A)  =>
    (C in Cs *> ((_,_,_,_,R) ^= isLambda(C) && isSimpleAction(R))).
  isSimpleAction(A) where _ ^= isIntegrity(A) => .true.
  isSimpleAction(A) where _ ^= isShow(A) => .true.
  isSimpleAction(A) where (_,T,L,R) ^= isIfThenElse(A) =>
    isSimpleAction(L) && isSimpleAction(R).
  isSimpleAction(A) where (_,T,L) ^= isIfThen(A) =>
    isSimpleAction(L).
  isSimpleAction(A) where (_,_,B) ^= isWhileDo(A) =>
    isSimpleAction(B).
  isSimpleAction(A) where (_,_,B) ^= isForDo(A) =>
    isSimpleAction(B).
  isSimpleAction(A) where _ ^= isRoundTerm(A) => .true.
  isSimpleAction(_) default => .false.

  public isIterable:(ast) => boolean.
  isIterable(A) where _ ^= isSearch(A) => .true.
  isIterable(A) where (_,L,R) ^= isConjunct(A) =>
    isIterable(L) || isIterable(R).
--  isIterable(A) where (_,T,L,R) ^= isConditional(A) =>
--    isIterable(L) || isIterable(R).
  isIterable(A) where (_,L,R) ^= isDisjunct(A) =>
    isIterable(L) || isIterable(R).
  isIterable(A) where (_,L,R) ^= isImplies(A) =>
    isIterable(L) || isIterable(R).
  isIterable(A) where (_,R) ^= isNegation(A) =>
    isIterable(R).
  isIterable(A) where (_,[I]) ^= isTuple(A) => isIterable(I).
  isIterable(_) default => .false.

  actionMacro(A,.expression,Rp) where (Lc,Act) ^= isActionTerm(A) => do{
    AA <- delayedAction(Act,Rp);
    valis active(typeAnnotation(Lc,AA,sqBinary(Lc,"action",anon(Lc),anon(Lc))))
  }

  taskAction(A,.expression,Rp) where (Lc,Act) ^= isTaskTerm(A) => do{
    AA <- delayedAction(Act,Rp);
    valis active(typeAnnotation(Lc,AA,sqBinary(Lc,"task",anon(Lc),anon(Lc))))
  }

  macroDo(A,.expression,Rp) where (Lc,Act) ^= isDoTerm(A) => do{
    AA <- delayedAction(Act,Rp);
    valis active(AA)
  }

  delayedAction(A,Rp)  => do{
    AA <- makeAction(A,.none,Rp);
    Lc .= locOf(A);

    Unit .= tpl(Lc,"()",[]);
    Start .= makeReturn(Lc,Unit);
    valis combine(Start,some((Lc,AA)))
  }

  performMacro(A,.expression,Rp) where (Lc,E) ^= isValof(A) =>
    either(active(unary(Lc,"_perform",E))).
  peformMacro(A,_,_) default => either(.inactive).
  
  public makeAction:(ast,option[(locn,ast)],reports) => either[reports,ast].
  makeAction(A,Cont,Rp) where (_,[St]) ^= isBrTuple(A) =>
    makeAction(St,Cont,Rp).
  makeAction(A,Cont,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    RR <- makeAction(R,Cont,Rp);
    makeAction(L,some((Lc,RR)),Rp)
  }
  makeAction(A,.none,Rp) where (Lc,R) ^= isValis(A) =>
    either(makeReturn(Lc,R)).
  makeAction(A,some((_,CC)),Rp) where (Lc,R) ^= isValis(A) =>
    other(reportError(Rp,"$(A) must be the last action\n$(CC) follows valis",Lc)).
  makeAction(A,_,Rp) where (Lc,R) ^= isThrow(A) =>
    makeThrow(Lc,R,Rp).
  makeAction(A,Cont,Rp) where (Lc,I) ^= isPerform(A) => do{
    Sub <- makeAction(I,.none,Rp);
    valis combine(makePerform(Lc,Sub),Cont)
  }
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isBind(A) =>
    other(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isBind(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[L]),Cont);
    valis binary(CLc,"_sequence",R,Lam)
  }
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isMatch(A) =>
    other(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isMatch(A) => do{
    Lam .= equation(Lc,L,Cont);
    valis mkCaseExp(Lc,R,[Lam]) -- roundTerm(CLc,Lam,[R])
  }
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isOptionMatch(A) =>
    other(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isOptionMatch(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[unary(Lc,"some",L)]),Cont);
    valis roundTerm(CLc,Lam,[R])
  }
  makeAction(A,Cont,Rp) where (Lc,L,R) ^= isAssignment(A) &&
      (LLc,LL,LR) ^= isIndex(L) =>
    makeAction(binary(Lc,":=",LL,ternary(LLc,"_put",unary(LLc,"!",LL),LR,R)),
      Cont,Rp).
  makeAction(A,Cont,Rp) where (Lc,B,H) ^= isTryCatch(A) => do{
    NB <- makeAction(B,.none,Rp);
    NH <- makeHandler(H,Rp);
    
    valis combine(binary(Lc,"_handle",NB,NH),Cont)
  }
  /*
    assert C 
  becomes
  try{
  assrt(()=>C,"failed: C",Loc)
  } catch(Err) => action{
    logMsg(Err)
    throw ()
  }
*/
  makeAction(A,Cont,Rp) where (Lc,C) ^= isIntegrity(A) => do{
    Unit .= tpl(Lc,"()",[]);
    Lam .= equation(Lc,Unit,C);
    Assert .= ternary(Lc,"assrt",Lam,str(Lc,C::string),Lc::ast);
    B .= brTuple(Lc,[Assert]);
    Err .= genName(Lc,"E");
    Thrw <- makeThrow(Lc,Unit,Rp);
    EH .= combine(unary(Lc,"logMsg",Err),some((Lc,Thrw)));
    ELam .= equation(Lc,rndTuple(Lc,[Err]),EH);
      
    valis combine(binary(Lc,"_handle",Assert,ELam),Cont)
  }
  /*
  show E becomes shwMsg(()=>E,"E",Lc)
*/
   makeAction(A,Cont,Rp) where (Lc,E) ^= isShow(A) => do{
    Lam .= equation(Lc,rndTuple(Lc,[]),E);

    valis combine(ternary(Lc,"shwMsg",Lam,reconstructDisp(E),Lc::ast),Cont)
  }
  makeAction(A,Cont,Rp) where (Lc,T,Th,El) ^= isIfThenElse(A) => do{
    Then <- makeAction(Th,.none,Rp);
    Else <- makeAction(El,.none,Rp);
    if isIterable(T) then {
      Itr <- makeIterableGoal(T,Rp);
      valis combine(mkConditional(Lc,Itr,Then,Else),Cont)
    } else{
      valis combine(mkConditional(Lc,T,Then,Else),Cont)
    }
  }
  makeAction(A,Cont,Rp) where (Lc,T,Th) ^= isIfThen(A) => do{
    Then <- makeAction(Th,.none,Rp);
    Unit .= rndTuple(Lc,[]);
    if isIterable(T) then {
      Itr <- makeIterableGoal(T,Rp);
      valis combine(mkConditional(Lc,Itr,Then,makeReturn(Lc,Unit)),Cont)
    } else{
      valis combine(mkConditional(Lc,T,Then,makeReturn(Lc,Unit)),Cont)
    }
  }
  makeAction(A,Cont,Rp) where (Lc,"nothing") ^= isEnumSymb(A) => do{
    if (_,CC) ^= Cont then
      valis CC
    else
    valis makeReturn(Lc, rndTuple(Lc,[]))
  }
  makeAction(A,Cont,Rp) where (Lc,[]) ^= isBrTuple(A) => do{
    if (_,CC) ^= Cont then
      valis CC
    else
    valis makeReturn(Lc, rndTuple(Lc,[]))
  }
  /* Construct a local iterator function:
   let{
  loop() => do{ if C then { B; loop() }}
   } in loop()
  */
  makeAction(A,Cont,Rp) where (Lc,Tst,Body) ^= isWhileDo(A) => do{
    Unit .= rndTuple(Lc,[]);
    FnCall .= zeroary(Lc,genSym("loop"));

    Then <- makeAction(mkIfThen(Lc,Tst,actionSeq(Lc,Body,FnCall)),.none,Rp);
    Lam .= equation(Lc,FnCall,Then);
    valis combine(mkLetDef(Lc,[Lam],FnCall),Cont)
  }
    /*
   for C do {A}
  becomes:
  
  <iterator>( do{return ()}, (Lcls,St) => do {A; return ()})
*/
  makeAction(A,Cont,Rp) where (Lc,C,B) ^= isForDo(A) => do{
    Unit .= rndTuple(Lc,[]);
    Zed .= makeReturn(Lc,Unit);
    IterBody <- makeAction(B,some((Lc,Zed)),Rp);
    Loop <- makeCondition(C,makeRtn,
      (St)=>IterBody,
      lyfted(Zed),Rp);
    valis combine(Loop,Cont)
  }
  makeAction(A,Cont,Rp) where (Lc,G,Cs) ^= isCase(A) => do{
    Unit .= rndTuple(Lc,[]);
    Zed .= makeReturn(Lc,Unit);
    Cases <- seqmap((C) => do{
	if (CLc,CD,CL,CC,CR) ^= isLambda(C) then{
	  AR <- makeAction(CR,some((Lc,Zed)),Rp);
	  valis mkLambda(CLc,CD,CL,CC,AR)
	}
	else
	throw reportError(Rp,"invalid action case $(C)",locOf(C))
      },Cs);
    valis combine(mkCaseExp(Lc,G,Cases),Cont)
  }

  makeAction(A,Cont,_) where _ ^= isRoundTerm(A) =>
    either(combine(A,Cont)).

  makeAction(A,_,Rp) =>
    other(reportError(Rp,"cannot figure out action $(A)",locOf(A))).

  /*
  * An 'iterable' conditions become a match on the result of a search
  *
  * becomes:
  *
  * either(some(PtnV)) .= <genCondition>(C,either(.none),...)
  *
  * This will need more optimization ...
  */

  public makeIterableGoal:(ast,reports) => either[reports,ast].
  makeIterableGoal(A,Rp) => do{
    Lc .= locOf(A);
    Vrs .= (goalVars(A) // (Nm)=>nme(Lc,Nm));
    VTpl .= rndTuple(Lc,Vrs);
    Unit .= unary(Lc,"either",enum(Lc,"none"));
    Zed .= unary(Lc,"_valis",Unit);
    Ptn .= unary(Lc,"either",unary(Lc,"some",VTpl));
    Seq <- makeCondition(A,makeRtn,
      (_)=>unary(Lc,"_valis",Ptn),
      lyfted(Zed),Rp);
    Tpd .= typeAnnotation(Lc,Seq,sqBinary(Lc,"action",anon(Lc),anon(Lc)));
    valis mkMatch(Lc,Ptn,unary(Lc,"_perform",Tpd))
  }

  makeHandler(H,Rp) where (Lc,[St]) ^= isBrTuple(H) => do{
    NH <- makeAction(St,.none,Rp);
    valis equation(Lc,rndTuple(Lc,[nme(Lc,"_")]),NH)
  }
  makeHandler(H,_) => either(H). 

  combine(A,.none) => A.
  combine(A,some((Lc,Cont))) => 
    binary(Lc,"_sequence",A,equation(Lc,rndTuple(Lc,[anon(Lc)]),Cont)).

  makeRtn(lyfted(Exp)) => Exp.
  makeRtn(grounded(Exp)) => unary(locOf(Exp),"_valis",Exp).

  makeReturn(Lc,A) => unary(Lc,"_valis",A).

  makeThrow(Lc,A,Rp) => either(unary(Lc,"_raise",A)).

  lyfted[a] ::= lyfted(a) | grounded(a).

  public makeAbstraction:(locn,ast,ast,reports) => either[reports,ast].
  makeAbstraction(Lc,Bnd,Cond,Rp) => do{
    Zed .= makeReturn(Lc,nme(Lc,"_nil"));
    Loop <- makeCondition(Cond,makeRtn,
      makeEl(Lc,Bnd),
      lyfted(Zed),Rp);
    valis makePerform(Lc,typeAnnotation(Lc,Loop,
	sqBinary(Lc,"action",rndTuple(Lc,[]),anon(Lc))))
  }

  makePerform(Lc,Exp) => unary(Lc,"_perform",Exp).

  makeSequence(Lc,St,X,Rs) => binary(Lc,"_sequence",makeRtn(X),
    equation(Lc,rndTuple(Lc,[St]),Rs)).
    
  makeEl(Lc,Bnd) => let{
    f(grounded(Strm)) =>  makeReturn(Lc,binary(Lc,"_cons",Bnd,Strm)).
    f(lyfted(Strm)) => Strm.
  } in f.

  /*
  * Ptn in Src
  * becomes
  * let{.
  *  sF(Ptn,St) => AddEl(X,St).
  *  sF(_,St) default => do { return St}.
  * .} in _iter(Src,Zed,sF)
  *
  * where AddEl, Zez are parameters to the conversion
  */
  makeCondition:(ast,(lyfted[ast])=>ast,
    (lyfted[ast])=>ast,
    lyfted[ast],reports) => either[reports,ast].
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,Ptn,Src) ^= isSearch(A) => do{
    sF .= nme(Lc,"sF");
    St .= genName(Lc,"St");

    Eq1 .= equation(Lc,roundTerm(Lc,sF,[Ptn,St]),Succ(grounded(St)));
    Eq2 .= equation(Lc,roundTerm(Lc,sF,[anon(Lc),St]),Lift(grounded(St)));
    
    FF .= mkQLetDef(Lc,[Eq1,Eq2],sF);
    valis ternary(Lc,"_iter",Src,Lift(Zed),FF)
  }
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,L,R) ^= isConjunct(A) =>
    makeCondition(L,Lift,(Lf) => valof makeCondition(R,Lift,Succ,Lf,Rp),Zed,Rp).
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,L,R) ^= isDisjunct(A) => do{
    E1<-makeCondition(L,Lift,Succ,Zed,Rp);
    makeCondition(R,Lift,Succ,lyfted(E1),Rp)
  }
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,R) ^= isNegation(A) => do{
    Negated <- makeCondition(R,Lift,(_)=>Lift(grounded(enum(Lc,"true"))),
      grounded(enum(Lc,"false")),Rp);
    St .= genName(Lc,"St");
    SuccCase .= Succ(Zed);
    FalseCase .= Lift(Zed);
    valis makeSequence(Lc,St,lyfted(Negated),mkConditional(Lc,St,FalseCase,SuccCase))
  }
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,L,R) ^= isImplies(A) =>
    makeCondition(negated(Lc,mkConjunct(Lc,L,negated(Lc,R))),Lift,Succ,Zed,Rp).
  makeCondition(Other,Lift,Succ,Zed,Rp) => do{
    Lc .= locOf(Other);
    AddToSucc .= Succ(Zed);
    St .= anon(Lc);
    Init .= Lift(Zed);
    if Fun ^= isBoolLift(AddToSucc) then -- special case
      valis Fun(Other)
    else
    valis makeSequence(Lc,St,Zed,mkConditional(Lc,Other,AddToSucc,Init))
  }

  isBoolLift(V) where (Lc,I) ^= isUnary(V,"_valis") =>
    ((_,"true") ^= isEnumSymb(I) ?
	some((X)=>unary(Lc,"_valis",X)) ||
	.none).
  isBoolLift(_) default => .none.

  goalVars:(ast)=>cons[string].
  goalVars(Cond) => glVars(Cond,["_"],[])::cons[string].

  glVars:(ast,set[string],set[string]) => set[string].
  glVars(A,Excl,Vrs) where (_,L,R) ^= isWhere(A) =>
    glVars(L,Excl,glVars(R,Excl,Vrs)).
  glVars(A,Excl,Vrs) where (_,L,R) ^= isConjunct(A) =>
    glVars(L,Excl,glVars(R,Excl,Vrs)).
  glVars(A,Excl,Vrs) where (_,L,R) ^= isDisjunct(A) =>
    glVars(L,Excl,Vrs)/\glVars(R,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,T,L,R) ^= isConditional(A) =>
    glVars(L,Excl,glVars(T,Excl,Vrs))/\glVars(R,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,Els) ^= isTuple(A) =>
    foldRight((E,F)=>glVars(E,Excl,F),Vrs,Els).
  glVars(A,Excl,Vrs) where (_,P,C) ^= isSearch(A) => ptnVars(P,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,P,C) ^= isMatch(A) => ptnVars(P,Excl,Vrs).
  glVars(A,Excl,Vrs) where (_,P,C) ^= isOptionMatch(A) => ptnVars(P,Excl,Vrs).
  glVars(_,_,Vrs) default => Vrs.

  ptnVars:(ast,set[string],set[string]) => set[string].
  ptnVars(A,Excl,Vrs) where (_,Nm) ^= isName(A) =>
    (Nm in Excl ? Vrs || Vrs\+Nm).
  ptnVars(A,Excl,Vrs) where _ ^= isInt(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isFlt(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isStr(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isStr(A) => Vrs.
  ptnVars(A,Excl,Vrs) where _ ^= isEnum(A) => Vrs.
  ptnVars(A,Excl,Vrs) where (_,L,R) ^= isWhere(A) =>
    glVars(R,Excl,ptnVars(L,Excl,Vrs)).
  ptnVars(A,Excl,Vrs) where (_,_,Els) ^= isRoundTerm(A) =>
    foldRight((E,F)=>ptnVars(E,Excl,F),Vrs,Els).
  ptnVars(A,Excl,Vrs) where (_,Els) ^= isTuple(A) =>
    foldRight((E,F)=>ptnVars(E,Excl,F),Vrs,Els).
  ptnVars(A,Excl,Vrs) where (_,Els) ^= isSqTuple(A) =>
    foldRight((E,F)=>ptnVars(E,Excl,F),Vrs,Els).
  ptnVars(_,_,Vrs) default => Vrs.

  pkgNameMacro(A,.expression,Rp) where
      (Lc,"__pkg__") ^= isName(A) && locn(Pkg,_,_,_,_).=Lc =>
    either(active(str(Lc,Pkg))).

  uMinusMacro(A,_,Rp) where (Lc,R) ^= isUnary(A,"-") =>
    (int(_,Ix) .= R ?
	either(active(int(Lc,-Ix))) ||
	num(_,Dx) .= R ?
	  either(active(num(Lc,-Dx))) ||
	  either(active(unary(Lc,"__minus",R)))).
  uMinusMacro(_,_,_) => either(.inactive).

  optionMatchMacro(A,.expression,Rp) where (Lc,L,R) ^= isOptionMatch(A) =>
    either(active(mkMatch(Lc,unary(Lc,"some",L),R))).
  optionMatchMacro(_,_,_) default => either(.inactive).

  optionPtnMacro(A,.pattern,Rp) where (Lc,L,R) ^= isOptionPtn(A) => do{
    valis active(mkWherePtn(Lc,R,L))
  }.

  macroAssignDef(A,.statement,Rp) where (Lc,L,R) ^= isAssignment(A) =>
    either(active(binary(Lc,"=",L,unary(Lc,"_cell",R)))).
  macroAssignDef(_,_,_) default => either(.inactive).
}
