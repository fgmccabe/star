star.compiler.macro{
  import star.
  import star.sort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.macro.infra.
  import star.compiler.macro.rules.
  import star.compiler.wff.

  macroAst:(ast,macroContext,(ast,reports)=>result[reports,ast],reports) => result[reports,ast].
  macroAst(A,Cxt,Examine,Rp) => do{
    Rslt <- applyRules(A,Cxt,.inactive,Rp);

    if active(T).=Rslt then
      macroAst(T,Cxt,Examine,Rp)
    else
      Examine(A,Rp)
  }
  macroAst(A,_,Examine,Rp) default => Examine(A,Rp).

  public macroPkg:(ast,reports) => result[reports,ast].
  macroPkg(A,Rp) => macroAst(A,.package,examinePkg,Rp).

  examinePkg(A,Rp) where (Lc,O,Els) ^= isBrTerm(A) => do{
    Ss <- macroStmts(buildMain(Els),Rp);
    valis mkLabeledTheta(Lc,O,Ss)
  }

  macroStmts:(cons[ast],reports)=>result[reports,cons[ast]].
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

  macroStmt:(ast,reports) => result[reports,ast].
  macroStmt(A,Rp) => macroAst(A,.statement,examineStmt,Rp).

  examineStmt:(ast,reports) => result[reports,ast].
  examineStmt(A,Rp) where (Lc,L,R) ^= isTypeAnnotation(A) => do{
    RR <- macroType(R,Rp);
    valis typeAnnotation(Lc,L,RR)
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
  examineStmt(A,Rp) where _ ^= isImport(A) => do { valis A}.
  examineStmt(A,Rp) where (Lc,E) ^= isOpen(A) => do{
    EE <- macroTerm(E,Rp);
    valis mkOpen(Lc,EE)
  }.
  examineStmt(A,Rp) where (Lc,L,R) ^= isDefn(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkDefn(Lc,LL,RR)
  }
  examineStmt(A,Rp) where (Lc,Nm,Deflt,L,C,R) ^= isEquation(A) => do{
    LL <- macroPtn(L,Rp);
    CC <- macroOpt(C,macroCond,Rp);
    RR <- macroTerm(R,Rp);
    valis mkEquation(Lc,Nm,Deflt,LL,CC,RR)
  }
  examineStmt(A,Rp) where (Lc,Q,C,L,R) ^= isTypeExistsStmt(A) => do{
    Lx <- macroType(L,Rp);
    Rx <- macroType(R,Rp);
    Qx <- seqmap((V)=>macroType(V,Rp),Q);
    Cx <- seqmap((V)=>macroType(V,Rp),C);
    valis mkTypeExistsStmt(Lc,Qx,Cx,Lx,Rx)
  }
  examineStmt(A,Rp) where (Lc,Q,C,L,R) ^= isTypeFunStmt(A) => do{
    Lx <- macroType(L,Rp);
    Rx <- macroType(R,Rp);
    Qx <- seqmap((V)=>macroType(V,Rp),Q);
    Cx <- seqmap((V)=>macroType(V,Rp),C);
    valis mkTypeFunStmt(Lc,Qx,Cx,Lx,Rx)
  }
  examineStmt(A,Rp) where (Lc,L,Els) ^= isContractStmt(A) => do{
    Lx <- macroType(L,Rp);
    Elx <- macroStmts(Els,Rp);
    valis mkContractStmt(Lc,Lx,Elx)
  }
  examineStmt(A,Rp) where (Lc,Q,Cx,Tp,Exp) ^= isImplementationStmt(A) => do{
    QQ <- seqmap((V)=>macroType(V,Rp),Q);
    CCx <- seqmap((T)=>macroType(T,Rp),Cx);
    TT <- macroType(Tp,Rp);
    EE <- macroTerm(Exp,Rp);
    valis mkImplementationStmt(Lc,QQ,CCx,TT,EE)
  }
  examineStmt(A,Rp) where (Lc,_,Q,C,Tp,B) ^= isAlgebraicTypeStmt(A) => do{
    Qx <- seqmap((Qv)=>macroType(Qv,Rp),Q);
    Cx <- seqmap((T)=>macroType(T,Rp),C);
    Tx <- macroType(Tp,Rp);
    Bx <- macroConstructor(B,Rp);
    valis mkAlgebraicTypeStmt(Lc,Qx,Cx,Tx,Bx)
  }
  examineStmt(A,Rp) where isConstructorStmt(A) =>
    macroType(A,Rp).
  examineStmt(A,Rp) where _ ^= isAnnotation(A) => ok(A).
  examineStmt(A,Rp) where (Lc,Els) ^= isBrTuple(A) => do{
    Elx <- macroStmts(Els,Rp);
    valis brTuple(Lc,Elx)
  }
  examineStmt(A,Rp) =>
    err(reportError(Rp,"cannot figure out statement $(A)",locOf(A))).

  macroConstructor(A,Rp) where (Lc,L,R)^=isBinary(A,"|") => do{
    Lx <- macroConstructor(L,Rp);
    Rx <- macroConstructor(R,Rp);
    valis binary(Lc,"|",Lx,Rx)
  }
  macroConstructor(A,Rp) where _ ^= isEnum(A) => do{ valis A}
  macroConstructor(A,Rp) where _ ^= isName(A) => do{ valis A}
  macroConstructor(A,Rp) where (Lc,O,Els) ^= isRoundTerm(A) => do{
    Ox <- macroTerm(O,Rp);
    Ex <- seqmap((E)=>macroType(E,Rp),Els);
    valis roundTerm(Lc,Ox,Ex)
  }
  macroConstructor(A,Rp) where (Lc,O,Q,C,Els) ^= isBraceCon(A) => do{
    Qx <- seqmap((V)=>macroType(V,Rp),Q);
    Cx <- seqmap((T)=>macroType(T,Rp),C);
    Ex <- macroStmts(Els,Rp);
    valis reUQuant(Lc,Qx,reConstrain(Cx,braceTerm(Lc,nme(Lc,O),Ex)))
  }
  macroConstructor(A,Rp) where (Lc,I) ^= isPrivate(A) => do{
    Ix <- macroConstructor(I,Rp);
    valis unary(Lc,"private",Ix)
  }
  macroConstructor(A,Rp) where (Lc,Q,I) ^= isXQuantified(A) => do{
    Qx <- seqmap((V)=>macroType(V,Rp),Q);
    Ix <- macroConstructor(I,Rp);
    valis reXQuant(Lc,Qx,Ix)
  }


  macroAction:(ast,reports) => result[reports,ast].
  macroAction(A,Rp) => macroAst(A,.actn,examineAction,Rp).

  examineAction(A,_) where _ ^= isName(A) => ok(A).
  examineAction(A,Rp) where (Lc,L,R) ^= isActionSeq(A) => do{
    Lx <- macroAction(L,Rp);
    Rx <- macroAction(R,Rp);
    valis actionSeq(Lc,Lx,Rx)
  }
  examineAction(A,Rp) where (Lc,L) ^= isUnary(A,";") => 
    macroAction(L,Rp).
  examineAction(A,Rp) where (Lc,[As]) ^= isBrTuple(A) => do{
    Ax <- macroAction(As,Rp);
    valis brTuple(Lc,[Ax]);
  }
  examineAction(A,Rp) where (Lc,[]) ^= isBrTuple(A) => ok(A).
  examineAction(A,Rp) where (Lc,L,R) ^= isMatch(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkMatch(Lc,LL,RR)
  }
  examineAction(A,Rp) where (Lc,L,R) ^= isOptionMatch(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkOptionMatch(Lc,LL,RR)
  }
  examineAction(A,Rp) where (Lc,L,R) ^= isAssignment(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkAssignment(Lc,LL,RR)
  }
  examineAction(A,Rp) where (Lc,L,R) ^= isBind(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkBind(Lc,LL,RR)
  }
  examineAction(A,Rp) where (Lc,T,L,R) ^= isIfThenElse(A) => do{
    Tx <- macroCond(T,Rp);
    Lx <- macroAction(L,Rp);
    Rx <- macroAction(R,Rp);
    valis mkIfThenElse(Lc,Tx,Lx,Rx)
  }
  examineAction(A,Rp) where (Lc,T,L) ^= isIfThen(A) => do{
    Tx <- macroCond(T,Rp);
    Lx <- macroAction(L,Rp);
    valis mkIfThen(Lc,Tx,Lx)
  }
  examineAction(A,Rp) where (Lc,B,H) ^= isTryCatch(A) => do{
    Bx <- macroAction(B,Rp);
    Hx <- macroTerm(H,Rp);
    valis mkTryCatch(Lc,Bx,Hx)
  }
  examineAction(A,Rp) where (Lc,B,H) ^= isTryHandle(A) => do{
    Bx <- macroAction(B,Rp);
    Hx <- macroTerm(H,Rp);
    valis mkTryHandle(Lc,Bx,Hx)
  }
  examineAction(A,Rp) where (Lc,C,B) ^= isWhileDo(A) => do{
    Cx <- macroCond(C,Rp);
    Bx <- macroAction(B,Rp);
    valis mkWhileDo(Lc,Cx,Bx)
  }
  examineAction(A,Rp) where (Lc,B,C) ^= isUntilDo(A) => do{
    Cx <- macroCond(C,Rp);
    Bx <- macroAction(B,Rp);
    valis mkUntilDo(Lc,Cx,Bx)
  }
  examineAction(A,Rp) where (Lc,C,B) ^= isForDo(A) => do{
    Cx <- macroTerm(C,Rp);
    Bx <- macroAction(B,Rp);
    valis mkForDo(Lc,Cx,Bx)
  }
  examineAction(A,Rp) where (Lc,T) ^= isValis(A) => do{
    Tx <- macroTerm(T,Rp);
    valis mkValis(Lc,Tx)
  }
  examineAction(A,Rp) where (Lc,T) ^= isIgnore(A) => do{
    Tx <- macroTerm(T,Rp);
    valis mkIgnore(Lc,Tx)
  }
  examineAction(A,Rp) where (Lc,T) ^= isRaise(A) => do{
    Tx <- macroTerm(T,Rp);
    valis mkRaise(Lc,Tx)
  }
  examineAction(A,Rp) where (Lc,T) ^= isThrow(A) => do{
    Tx <- macroTerm(T,Rp);
    valis mkThrow(Lc,Tx)
  }
  examineAction(A,Rp) where (Lc,T) ^= isPerform(A) => do{
    Tx <- macroTerm(T,Rp);
    valis mkPerform(Lc,Tx)
  }
  examineAction(A,Rp) where (Lc,L,B) ^= isPrompt(A) => do{
    Lx <- macroTerm(L,Rp);
    Bx <- macroAction(B,Rp);
    valis mkPrompt(Lc,Lx,Bx)
  }
  examineAction(A,Rp) where (Lc,Lb,L,R) ^= isCut(A) => do{
    Lbx <- macroTerm(Lb,Rp);
    Lx <- macroTerm(L,Rp);
    Rx <- macroAction(R,Rp);
    valis mkCut(Lc,Lbx,Lx,Rx)
  }
  examineAction(A,Rp) where (Lc,K,As) ^= isResume(A) => do{
    Kx <- macroTerm(K,Rp);
    Asx <- macroTerm(As,Rp);
    valis mkResume(Lc,Kx,Asx)
  }
  examineAction(A,Rp) where (Lc,D,B) ^= isLetDef(A) => do{
    DD <- macroStmts(D,Rp);
    BB <- macroAction(B,Rp);
    valis mkLetDef(Lc,DD,BB)
  }
  examineAction(A,Rp) where (Lc,D,B) ^= isLetRecDef(A) => do{
    DD <- macroStmts(D,Rp);
    BB <- macroAction(B,Rp);
    valis mkLetRecDef(Lc,DD,BB)
  }
  examineAction(A,Rp) where (Lc,G,Cs) ^= isCase(A) => do{
    Gx <- macroTerm(G,Rp);
    CC <- seqmap((C)=>macroLambda(C,Rp),Cs);
    valis mkCaseExp(Lc,Gx,CC)
  }
  examineAction(A,Rp) where (Lc,O,Els) ^= isRoundTerm(A) => do{
    OO <- macroTerm(O,Rp);
    EE <- seqmap((E)=>macroTerm(E,Rp),Els);
    valis roundTerm(Lc,OO,EE)
  }
  examineAction(A,Rp) default =>
    err(reportError(Rp,"cannot figure out action $(A)",locOf(A))).

  macroTerm(A,Rp) => macroAst(A,.expression,examineTerm,Rp).

  examineTerm(A,_) where _ ^= isName(A) => ok(A).
  examineTerm(A,_) where _ ^= isEnum(A) => ok(A).
  examineTerm(A,Rp) where int(_,_) .= A => ok(A).
  examineTerm(A,Rp) where big(_,_) .= A => ok(A).
  examineTerm(A,Rp) where chr(_,_) .= A => ok(A).
  examineTerm(A,Rp) where num(_,_) .= A => ok(A).
  examineTerm(A,Rp) where str(_,_) .= A => ok(A).
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
  examineTerm(A,Rp) where (Lc,D,B) ^= isLetDef(A) => do{
    DD <- macroStmts(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkLetDef(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,D,B) ^= isLetRecDef(A) => do{
    DD <- macroStmts(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkLetRecDef(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,R) ^= isCellRef(A) => do{
    RR <- macroTerm(R,Rp);
    valis refCell(Lc,RR)
  }
  examineTerm(A,Rp) where (Lc,R) ^= isRef(A) => do{
    RR <- macroTerm(R,Rp);
    valis mkRef(Lc,RR)
  }
  examineTerm(A,Rp) where  _^= isTag(A) => ok(A).
  examineTerm(A,Rp) where (Lc,D,B) ^= isComprehension(A) => do{
    DD <- macroTerm(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkComprehension(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,D,B) ^= isListComprehension(A) => do{
    DD <- macroTerm(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkListComprehension(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,D,B) ^= isIotaComprehension(A) => do{
    DD <- macroTerm(D,Rp);
    BB <- macroTerm(B,Rp);
    valis mkIotaComprehension(Lc,DD,BB)
  }
  examineTerm(A,Rp) where (Lc,B) ^= isTestComprehension(A) => do{
    BB <- macroCond(B,Rp);
    valis mkTestComprehension(Lc,BB)
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
  examineTerm(A,Rp) where (Lc,Els) ^= isMapLiteral(A) => do{
    Ex <- seqmap((E)=>macroTerm(E,Rp),Els);
    valis mkMapLiteral(Lc,Ex)
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
  examineTerm(A,Rp) where (Lc,L,R) ^= isPair(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkComma(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isSequence(A) => do{
    LL <- macroTerm(L,Rp);
    RR <- macroTerm(R,Rp);
    valis mkSequence(Lc,LL,RR)
  }
  examineTerm(A,Rp) where (Lc,S) ^= isDoTerm(A) => do{
    Sx <- macroAction(S,Rp);
    valis mkDoTerm(Lc,Sx)
  }
  examineTerm(A,Rp) where (Lc,S) ^= isActionTerm(A) => do{
    Sx <- macroAction(S,Rp);
    valis mkActionTerm(Lc,Sx)
  }
  examineTerm(A,Rp) where (Lc,S) ^= isTaskTerm(A) => do{
    Sx <- macroAction(S,Rp);
    valis mkTaskTerm(Lc,Sx)
  }
  examineTerm(A,Rp) where _ ^= isTag(A) => do{ valis A}
  examineTerm(A,Rp) where (Lc,L,R) ^= isPrompt(A) => do{
    Lx <- macroTerm(L,Rp);
    Rx <- macroTerm(R,Rp);
    valis mkPrompt(Lc,Lx,Rx)
  }
  examineTerm(A,Rp) where (Lc,Lb,L,R) ^= isCut(A) => do{
    Lbx <- macroTerm(Lb,Rp);
    Lx <- macroTerm(L,Rp);
    Rx <- macroTerm(R,Rp);
    valis mkCut(Lc,Lbx,Lx,Rx)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isResume(A) => do{
    Lx <- macroTerm(L,Rp);
    Rx <- macroTerm(R,Rp);
    valis mkResume(Lc,Lx,Rx)
  }
  examineTerm(A,Rp) where (Lc,L,R) ^= isIndexTerm(A) => do{
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
  examineTerm(A,Rp) default =>
    err(reportError(Rp,"cannot figure out expression $(A), key=$(macroKey(A))",locOf(A))).

  macroCond:(ast,reports) => result[reports,ast].
  macroCond(C,Rp) => macroTerm(C,Rp).

  macroOpt:(option[ast],(ast,reports)=>result[reports,ast],reports) =>
    result[reports,option[ast]].
  macroOpt(.none,_,_) => ok(.none).
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

  examinePtn(A,_) where _ ^= isName(A) => ok(A).
  examinePtn(A,Rp) where int(_,_) .= A => ok(A).
  examinePtn(A,Rp) where big(_,_) .= A => ok(A).
  examinePtn(A,Rp) where num(_,_) .= A => ok(A).
  examinePtn(A,Rp) where str(_,_) .= A => ok(A).
  examinePtn(A,Rp) where chr(_,_) .= A => ok(A).
  examinePtn(A,_) where _ ^= isEnum(A) => ok(A).
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
  examinePtn(A,Rp) where (Lc,L,R) ^= isPair(A) => do{
    LL <- macroPtn(L,Rp);
    RR <- macroPtn(R,Rp);
    valis mkPair(Lc,LL,RR)
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
    err(reportError(Rp,"cannot figure out pattern $(A), key=$(macroKey(A))",locOf(A))).

  macroType(A,Rp) => macroAst(A,.typeterm,examineType,Rp).

  examineType(A,_) where _ ^= isName(A) => ok(A).
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
  examineType(A,Rp) where (Lc,L,R) ^= isContType(A) => do{
    NL <- macroType(L,Rp);
    NR <- macroType(R,Rp);
    valis mkContType(Lc,NL,NR)
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
    err(reportError(Rp,"cannot figure out type expression $(A), key=$(macroKey(A))",locOf(A))).

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

  /*
  _main([A1,..,An]) => valof do{
    if X1^=A1:?T1 then{
      if X2^=A2:?T2 then {
        ...
        valis main(X1,..,Xn)
      } else
      logMsg("cannot coerce $(A2) to T2")
    } else{
      logMsg("Cannot coerce $(A1) to T1")
    }
  }
*/

  synthesizeMain:(locn,ast,cons[ast])=>cons[ast].
  synthesizeMain(Lc,Tp,Defs) where (_,Lhs,Rhs) ^= isFunctionType(Tp) && (_,ElTps)^=isTuple(Lhs) => valof action{
    (Action,As) <- synthCoercion(Lc,ElTps,[]);
    
--    (Vs,Cs) .= synthesizeCoercions(ElTps,Lc);
    MLhs .= roundTerm(Lc,nme(Lc,"_main"),[mkConsPtn(Lc,As)]);
    Valof .= unary(Lc,"valof",
      unary(Lc,"do",brTuple(Lc,[Action])));
    Main .= equation(Lc,MLhs,Valof);
    Annot .= binary(Lc,":",nme(Lc,"_main"),equation(Lc,rndTuple(Lc,[squareTerm(Lc,nme(Lc,"cons"),[nme(Lc,"string")])]),rndTuple(Lc,[])));
    valis [unary(Lc,"public",Annot),Main,..Defs].
  }

/*
  if X^=A:?T then {
  valis main(X1,..,Xn)
  } else
  logMsg("cannot coerce $(A) to T")

*/  
  synthCoercion:(locn,cons[ast],cons[ast])=>result[(),(ast,cons[ast])].
  synthCoercion(_,[Tp,..Ts],Xs)  => do{
    Lc .= locOf(Tp);
    X .= genName(Lc,"X");
    A .= genName(Lc,"A");    
    PRhs .= binary(Lc,":?",A,Tp);
    Tst .= binary(Lc,"^=",X,PRhs); -- some(X).=_coerce(A):T
    Emsg .= unary(Lc,"logMsg",str(Lc,"cannot coerce \$(#(A::string)) to #(Tp::string)"));
    (Inner,As) <- synthCoercion(Lc,Ts,[X,..Xs]);
    valis (mkIfThenElse(Lc,Tst,Inner,Emsg),[A,..As])
  }
  synthCoercion(Lc,[],Xs) => do{
    valis (roundTerm(Lc,nme(Lc,"main"),reverse(Xs)),[])
  }

  mkConsPtn:(locn,cons[ast]) => ast.
  mkConsPtn(Lc,[]) => enum(Lc,"nil").
  mkConsPtn(Lc,[E,..Es]) => binary(Lc,"cons",E,mkConsPtn(Lc,Es)).

  synthesizeCoercions:(cons[ast],locn)=> (ast,cons[ast]).
  synthesizeCoercions([],Lc) => (enum(Lc,"nil"),.nil).
  synthesizeCoercions([T,..Ts],Lc) where Nm .= genName(Lc,"X") &&
      (RV,RC) .= synthesizeCoercions(Ts,Lc) =>
    (binary(Lc,"cons",Nm,RV),[binary(Lc,":",unary(Lc,"_optval",unary(Lc,"_coerce",Nm)),T),..RC]).

}
