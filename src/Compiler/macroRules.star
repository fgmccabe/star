star.compiler.macro.rules{
  import star.
  import star.sort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.macro.infra.
  import star.compiler.wff.

  public applyRules:(ast,macroContext,macroState,reports) => result[reports,macroState].
  applyRules(A,Cxt,St,Rp) where Rules^=macros[macroKey(A)] =>
    applyRls(A,Cxt,St,Rules,Rp).
  applyRules(A,_,_,_) default => do{ valis .inactive }.

  applyRls:(ast,macroContext,macroState,cons[(macroContext,macroRule)],reports) =>
    result[reports,macroState].
  applyRls(A,_,St,[],_) => do{ valis St}.
  applyRls(A,Cxt,St,[(Cxt,R),..Rls],Rp) => do{
    Rslt <- R(A,Cxt,Rp);
    if .inactive.=Rslt then
      applyRls(A,Cxt,St,Rls,Rp)
    else
    valis Rslt
  }
  applyRls(A,Cxt,St,[_,..Rls],Rp) => 
    applyRls(A,Cxt,St,Rls,Rp).
  
  macros:map[string,cons[(macroContext,macroRule)]].
  macros = { "::=" -> [(.statement,algebraicMacro)],
    ":=" -> [(.actn,spliceAssignMacro),
      (.actn,indexAssignMacro)],
    "[]" -> [(.pattern,squarePtnMacro),
      (.expression,squareSequenceMacro)],
    "$[]" -> [(.expression,indexMacro),(.expression,sliceMacro)],
    "<||>" -> [(.expression,quoteMacro)],
    "::" -> [(.expression,coercionMacro)],
    ":?" -> [(.expression,coercionMacro)],
    "*" -> [(.expression,multicatMacro)],
    "${}" -> [(.expression,comprehensionMacro),(.expression,mapLiteralMacro)],
    "{!!}" -> [(.expression,iotaComprehensionMacro)],
    "{??}" -> [(.expression,testComprehensionMacro)],
    "do" -> [(.actn,forLoopMacro)],
    "valof" -> [(.expression,valofMacro)],
    "assert" -> [(.actn,assertMacro)],
    "show" -> [(.actn,showMacro)],
    "try" -> [(.actn,tryCatchMacro)],
    "__pkg__" -> [(.expression,pkgNameMacro)],
    "__loc__" -> [(.expression,locationMacro)],
    "-" -> [(.expression, uMinusMacro),(.pattern, uMinusMacro)],
    "^=" -> [(.expression, optionMatchMacro)],
    "^" -> [(.expression,unwrapMacro)],
    "!" -> [(.expression,binRefMacro)],
    "implementation" -> [(.statement,implementationMacro)]
  }.

  -- Convert assert C to assrt(()=>C,"failed C",Loc)
  assertMacro(A,.actn,Rp) where (Lc,C) ^= isIntegrity(A) => result{
    Lam .= equation(Lc,tpl(Lc,"()",[]),C);
    Assert .= ternary(Lc,"assrt",Lam,str(Lc,C::string),Lc::ast);
    valis active(Assert)
  }

  -- Convert show E to shwMsg(()=>E,"E",Lc)
  showMacro(A,.actn,Rp) where (Lc,E) ^= isShow(A) => result{
    Lam .= equation(Lc,tpl(Lc,"()",[]),E);
    Shw .= ternary(Lc,"shwMsg",Lam,str(Lc,A::string),Lc::ast);
    valis active(Shw)
  }

  -- Convert A![X] to (A!)[X]
  binRefMacro(A,.expression,Rp) where
      (Lc,Lhs,Rhs) ^= isBinary(A,"!") &&
      (LLc,[E]) ^= isSqTuple(Rhs) =>
    ok(active(squareTerm(LLc,refCell(Lc,Lhs),[E]))).
  binRefMacro(A,.expression,Rp)  where
      (Lc,_,_) ^= isBinary(A,"!") =>
    bad(reportError(Rp,"illegal use of !",Lc)).
  binRefMacro(A,_,_) =>
    ok(.inactive).

  -- Convert P^=E to some(P).=E
  optionMatchMacro(A,.expression,Rp) where
      (Lc,L,R) ^= isOptionMatch(A) =>
    ok(active(mkMatch(Lc,unary(Lc,"some",L),R))).
  optionMatchMacro(_,_,_) default => ok(.inactive).

  mkLoc(Lc) where locn(P,Line,Col,Off,Ln).=Lc =>
    roundTerm(Lc,nme(Lc,"locn"),
      [str(Lc,P),int(Lc,Line),int(Lc,Col),int(Lc,Off),int(Lc,Ln)]).

  -- Handle __loc__ macro symbol
  locationMacro(A,.expression,Rp) where
      (Lc,"__loc__") ^= isName(A) =>
    ok(active(mkLoc(Lc))).
  
  -- Handle occurrences of __pkg__
  pkgNameMacro(A,.expression,Rp) where
      (Lc,"__pkg__") ^= isName(A) && locn(Pkg,_,_,_,_).=Lc =>
    ok(active(str(Lc,Pkg))).

  -- Convert expression P^E to X where E ^= P(X)
  unwrapMacroA(A,.expression,Rp) where
      (Lc,Lhs,Rhs) ^= isOptionPtn(A) && (LLc,Nm) ^= isName(Lhs) => result{
	X .= genName(LLc,"X");
   	valis active(mkWhere(Lc,X,mkMatch(LLc,Rhs,unary(LLc,Nm,X))))
      }.
  unwrapMacro(_,_,_) default => ok(.inactive).
	
  -- Convert unary minus to a call to __minus
  uMinusMacro(A,_,Rp) where (Lc,R) ^= isUnary(A,"-") =>
    (int(_,Ix) .= R ?
	ok(active(int(Lc,-Ix))) ||
	num(_,Dx) .= R ?
	  ok(active(num(Lc,-Dx))) ||
	  ok(active(unary(Lc,"__minus",R)))).
  uMinusMacro(_,_,_) => ok(.inactive).

  -- Convert [P,...P] to _hdtl etc.
  squarePtnMacro(A,.pattern,Rp) where (Lc,Els) ^= isSqTuple(A) =>
    ok(active(macroListEntries(Lc,Els,genEofTest,genHedTest))).
  squarePtnMacro(_,_,_) => ok(.inactive).

  macroListEntries:(locn,cons[ast],(locn)=>ast,(locn,ast,ast)=>ast) => ast.
  macroListEntries(Lc,[],End,_) => End(Lc).
  macroListEntries(_,[Cns],_,Hed) where (Lc,H,T) ^= isCons(Cns) =>
    Hed(Lc,H,T).
  macroListEntries(Lc,[El,..Rest],Eof,Hed) =>
    Hed(Lc,El,macroListEntries(Lc,Rest,Eof,Hed)).

  genEofTest(Lc) => mkWhereTest(Lc,"_eof").
  genHedTest(Lc,H,T) => mkWherePtn(Lc,tpl(Lc,"()",[H,T]),nme(Lc,"_hdtl")).

  squareSequenceMacro(A,.expression,Rp) where
      (Lc,Els) ^= isSqTuple(A) =>
    ok(active(macroListEntries(Lc,Els,genNil,genCons))).
  squareSequenceMacro(_,_,_) => ok(.inactive).

  genNil(Lc) => nme(Lc,"_nil").
  genCons(Lc,H,T) => binary(Lc,"_cons",H,T).

  mapLiteralMacro(A,.expression,Rp) where
      (Lc,Els) ^= isMapLiteral(A) => do{
	valis active(macroListEntries(Lc,Els,genEmpty,genPut))
      }.
  mapLiteralMacro(_,_,_) default => ok(.inactive).

  genEmpty(Lc) => nme(Lc,"_empty").
  genPut(Lc,H,T) where (_,L,R) ^= isPair(H) => ternary(Lc,"_put",T,L,R).

  comprehensionMacro(A,.expression,Rp) where (Lc,B,C) ^= isComprehension(A) => do{
    Q <- makeComprehension(Lc,B,C,Rp);
    valis active(Q)
  }
  comprehensionMacro(_,_,_) => ok(.inactive).

  -- Convert {! Bnd | Body !}
  iotaComprehensionMacro(A,.expression,Rp) where
      (Lc,Bnd,Body) ^= isIotaComprehension(A) => do{
	CC <- makeCondition(Body,passThru,
	  genResult(unary(Lc,"some",Bnd)),
	  lyfted(enum(Lc,"none")),Rp);
	valis active(CC)
      }.
  iotaComprehensionMacro(_,_,_) => ok(.inactive).

  -- Convert {? Cond ?}
  testComprehensionMacro(A,.expression,Rp) where
      (Lc,Cond) ^= isTestComprehension(A) => do{
	CC <- makeCondition(Cond,passThru,
	  genResult(enum(Lc,"true")),
	  lyfted(enum(Lc,"false")),Rp);
	valis active(CC)
      }
  testComprehensionMacro(_,_,_) => ok(.inactive).
	
  makeComprehension:(locn,ast,ast,reports) => result[reports,ast].
  makeComprehension(Lc,Bnd,Cond,Rp) => do{
    makeCondition(Cond,passThru,(St)=>consResult(Lc,Bnd,St),
      grounded(nme(Lc,"_nil")),Rp)
  }

  -- Condition processing

  lyfted[a] ::= lyfted(a) | grounded(a).

  passThru(grounded(X)) => X.
  passThru(lyfted(X)) => X.

  consResult(Lc,Bnd,grounded(St)) => binary(Lc,"_cons",Bnd,St).
  consResult(_,_,lyfted(St)) => St.

  genResult(T) => let{
    chk(grounded(_))=>T.
    chk(lyfted(St)) => St.
  } in chk.	

  /*
  * Ptn in Src
  * becomes
  * let{
  *  sF(Ptn,St) => Succ(X,St).
  *  sF(_,St) default => Lift(St).
  * } in _iter(Src,Zed,sF)
  *
  * where Succ, Lift & Zed are parameters to the conversion
  */
  makeCondition:(ast,(lyfted[ast])=>ast,
    (lyfted[ast])=>ast,
    lyfted[ast],reports) => result[reports,ast].
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,Ptn,Src) ^= isSearch(A) => do{
    sF .= genName(Lc,"sF");
    St .= genName(Lc,"St");

    Eq1 .= equation(Lc,roundTerm(Lc,sF,[Ptn,St]),Succ(grounded(St)));
    Eq2 .= equation(Lc,roundTerm(Lc,sF,[anon(Lc),St]),Lift(grounded(St)));
    
    FF .= mkLetDef(Lc,[Eq1,Eq2],sF);
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
    valis mkConditional(Lc,Negated,FalseCase,SuccCase)
  }
  makeCondition(A,Lift,Succ,Zed,Rp) where (Lc,L,R) ^= isImplies(A) =>
    makeCondition(negated(Lc,mkConjunct(Lc,L,negated(Lc,R))),Lift,Succ,Zed,Rp).
  makeCondition(Other,Lift,Succ,Zed,Rp) => do{
    valis mkConditional(locOf(Other),Other,Succ(Zed),Lift(Zed))
  }

  -- Convert E::T to _optval(_coerce(E)):T
  -- Convert E:?T to _coerce(E):option[T]

  coercionMacro(A,.expression,Rp) where (Lc,L,R) ^= isCoerce(A) =>
    ok(active(typeAnnotation(Lc,unary(Lc,"_optval",unary(Lc,"_coerce",L)),R))).
  coercionMacro(A,.expression,Rp) where (Lc,L,R) ^= isOptCoerce(A) =>
    ok(active(typeAnnotation(Lc,unary(Lc,"_coerce",L),sqUnary(Lc,"option",R)))).
  coercionMacro(_,_,_) => ok(.inactive).

  indexMacro(A,.expression,Rp) where (Lc,L,R) ^= isIndexTerm(A) =>
    ((_,Ky,Vl) ^= isBinary(R,"->") ?
	ok(active(ternary(Lc,"_put",L,Ky,Vl))) ||
	(_,Ky) ^= isNegation(R) ?
	  ok(active(binary(Lc,"_remove",L,Ky))) ||
	  ok(active(binary(Lc,"_index",L,R)))).
  indexMacro(_,_,_) default => ok(.inactive).

  -- C[I]:=R becomes C:=_put(C!,I,R)

  indexAssignMacro(A,.actn,Rp) where
      (Lc,L,R) ^= isAssignment(A) &&
      (LLc,S,I) ^= isIndexTerm(L) =>
    ok(active(mkAssignment(Lc,S,
	  ternary(Lc,"_put",refCell(Lc,S),I,R)))).
  indexAssignMacro(_,_,_) default => ok(.inactive).
  

  sliceMacro(A,.expression,Rp) where (Lc,Op,F,T) ^= isSlice(A) =>
    ok(active(ternary(Lc,"_slice",Op,F,T))).
  sliceMacro(_,_,_) default => ok(.inactive).

  spliceAssignMacro(A,.expression,Rp) where
      (Lc,S,F,T,R) ^= isSplice(A) =>
    ok(active(mkAssignment(Lc,S,
	  roundTerm(Lc,nme(Lc,"_splice"),
	    [refCell(Lc,S),F,T,R])))).
  spliceAssignMacro(_,_,_) default => ok(.inactive).
  
  multicatMacro(A,.expression,Rp) where (Lc,E) ^= isUnary(A,"*") =>
    ok(active(unary(Lc,"_multicat",E))).
  multicatMacro(_,.expression,_) => ok(.inactive).


  -- Convert <| E |> to a quote expression
  -- which relies on the quote contract
  quoteMacro(A,.expression,Rp) where (Lc,Trm) ^= isQuoted(A) =>
    ok(active(quoteAst(A))).
  quoteMacro(_,_,_) => ok(.inactive).

  -- quoteAst creates an ast that checks to a quoted form of the ast itself
  quoteAst(E) where (_,X) ^= isUnary(E,"?") => X.
  quoteAst(nme(Lc,Nm)) => unary(Lc,"_name",str(Lc,Nm)).
  quoteAst(qnm(Lc,Nm)) => unary(Lc,"_qnme",str(Lc,Nm)).
  quoteAst(int(Lc,Nx)) => unary(Lc,"_integer",int(Lc,Nx)).
  quoteAst(num(Lc,Nx)) => unary(Lc,"_float",num(Lc,Nx)).
  quoteAst(chr(Lc,Cx)) => unary(Lc,"_char",chr(Lc,Cx)).
  quoteAst(str(Lc,Sx)) => unary(Lc,"_string",str(Lc,Sx)).
  quoteAst(tpl(Lc,Nm,Els)) => binary(Lc,"_tuple",str(Lc,Nm),
    macroListEntries(Lc,Els,genNil,genCons)).
  quoteAst(app(Lc,Op,Arg)) => binary(Lc,"_apply",quoteAst(Op),quoteAst(Arg)).
  
  -- Handle valof
  valofMacro(A,.actn,Rp) where (Lc,E) ^= isValof(A) =>
    (LLc,[As]) ^= isBrTuple(E) ?
    ok(active(unary(Lc,"_valof",
	  typeAnnotation(Lc,mkDoTerm(LLc,As),
	    squareTerm(LLc,nme(LLc,"result"),[anon(LLc),anon(LLc)]))))) ||
    ok(active(unary(Lc,"_valof",E))).
  valofMacro(_,_,_) default =>
    ok(.inactive).

  tryCatchMacro(A,.actn,Rp) where
      (Lc,B,H) ^= isTryCatch(A) &&
      (LLc,[As]) ^= isBrTuple(H) => do{
	Q .= equation(Lc,rndTuple(LLc,[anon(LLc)]),mkDoTerm(LLc,As));
	valis active(mkTryCatch(Lc,B,Q))
      }.
  tryCatchMacro(_,_,_) default => ok(.inactive).

  /*
  for X in L do Act
  becomes
  ignore _iter(L,ok(()),let{
    lP(_,bad(E)) => bad(E).
    lP(X,_) => do{ Act}
  } in lP)
*/

  forLoopMacro(A,.actn,Rp) where (Lc,T,B) ^= isForDo(A) &&
      (LLc,X,L) ^= isSearch(T) => do{
	E .= genName(Lc,"E");
	Lp .= genName(Lc,"Lp");
	Err .= unary(Lc,"err",E);
	Eq1 .= equation(Lc,roundTerm(Lc,Lp,[anon(Lc),Err]),Err);
	Hd2 .= roundTerm(Lc,Lp,[X,anon(Lc)]);
	V .= mkDoTerm(Lc,actionSeq(Lc,B,mkValis(Lc,unit(Lc))));
	Eq2 .= equation(Lc,Hd2,V);
	LLp .= mkLetDef(Lc,[Eq1,Eq2],Lp);
	valis active(ternary(Lc,"_iter",L,unary(Lc,"ok",unit(Lc)),LLp))
      }.
  forLoopMacro(_,_,_) => ok(.inactive).

  implementationMacro(A,.statement,Rp) where
      (Lc,Q,C,H,E) ^= isImplementationStmt(A) &&
      (_,Nm,_) ^= isSquareTerm(H) &&
      Ex ^= labelImplExp(E,Nm) => do{
	valis active(mkImplementationStmt(Lc,Q,C,H,Ex))
      }.
  implementationMacro(_,_,_) default => ok(.inactive).

  labelImplExp(T,Nm) where (Lc,Els) ^= isBrTuple(T) =>
    some(braceTerm(Lc,Nm,Els)).
  labelImplExp(T,Nm) where (Lc,Els) ^= isQBrTuple(T) =>
    some(qbraceTerm(Lc,Nm,Els)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ^= isLetDef(T) &&
    EE ^= labelImplExp(Exp,Nm) =>
    some(mkLetDef(Lc,Els,EE)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ^= isLetRecDef(T) &&
      EE ^= labelImplExp(Exp,Nm) =>
    some(mkLetRecDef(Lc,Els,EE)).
  labelImplExp(T,_) default => .none.

  -- Handle algebraic type definitions

  algebraicMacro(St,.statement,Rp) where (Lc,Vz,Q,Cx,H,R) ^= isAlgebraicTypeStmt(St) => do{
    Rslt <- makeAlgebraic(Lc,Vz,Q,Cx,H,R,Rp);
    valis active(brTuple(Lc,Rslt))
  }
  algebraicMacro(_,_,_) default => do{ valis .inactive}.

  /*
  * We generate auto implementations of fields declared for an algebraic type
  * declaration
  *
  * RT ::= rc{F:FT.}
  *
  * becomes
  *
  * RT <~ {F:FT .. }
  * auto_contract '.F'[RT->>FT] => {
  *  '$F'(rc(_,..,F,...,_)) => F
  *}
  *
  * where auto_contract is an implementation statement that automatically 
  * induces a contract -- with the empty prefix
  */

  makeAlgebraic:(locn,visibility,cons[ast],cons[ast],ast,ast,reports) =>
    result[reports,cons[ast]].
  makeAlgebraic(Lc,Vz,Q,Cx,H,Rhs,Rp) => do{
    (Qs,Xs,Face) <- algebraicFace(Rhs,Q,[],Rp);
--    Fields .= sort(Face,compEls);
    TpExSt .= reveal(reUQuant(Lc,Qs,reConstrain(Cx,binary(Lc,"<~",H,reXQuant(Lc,Xs,brTuple(Lc,Face))))),Vz);
--    logMsg("Type rule is $(TpExSt)");
    Cons <- buildConstructors(Rhs,Q,Cx,H,Vz,Rp);
--    logMsg("Constructors are $(Cons)");
    Accs <- buildAccessors(Rhs,Q,Cx,H,typeName(H),Face,Vz,Rp);
--    logMsg("Accessors are $(Accs)");
    Ups <- buildUpdaters(Rhs,Q,Cx,H,typeName(H),Face,Vz,Rp);
--    logMsg("Updaters are $(Ups)");
    valis [TpExSt,..Cons++Accs++Ups]
  }

  algebraicFace:(ast,cons[ast],cons[ast],reports) =>
    result[reports,(cons[ast],cons[ast],cons[ast])].
  algebraicFace(A,Qs,Xs,Rp) where (_,L,R) ^= isBinary(A,"|") => do{
    (Q1,X1,Lhs) <- algebraicFace(L,Qs,Xs,Rp);
    (Qx,Xx,Rhs) <- algebraicFace(R,Q1,X1,Rp);
    Fs <- combineFaces(Lhs,Rhs,Rp);
    valis (Qx,Xx,Fs)
  }
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_,_) ^= isRoundTerm(A) =>
    ok((Qs,Xs,[])).
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_) ^= isEnum(A) => ok((Qs,Xs,[])).
  algebraicFace(A,Qs,Xs,Rp) where (Lc,_,Els) ^= isBrTerm(A) =>
    ok((Qs,Xs,Els)).
  algebraicFace(A,Qs,Xs,Rp) where (_,I) ^= isPrivate(A) =>
    algebraicFace(I,Qs,Xs,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,I) ^= isPublic(A) =>
    algebraicFace(I,Qs,Xs,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,X0,I) ^= isXQuantified(A) =>
    algebraicFace(I,Qs,Xs\/X0,Rp).
  algebraicFace(A,Qs,Xs,Rp) where (_,A0,I) ^= isQuantified(A) =>
    algebraicFace(I,Qs\/A0,Xs,Rp).
  algebraicFace(A,_,_,Rp) default =>
    bad(reportError(Rp,"invalid case in algebraic type",locOf(A))).

  combineFaces([],F2,Rp) => ok(F2).
  combineFaces(F1,[],Rp) => ok(F1).
  combineFaces([F,..Fs],Gs,Rp) where (Lc,Id,Tp)^=isTypeAnnotation(F) => do{
    G1 <- mergeField(Lc,Id,Tp,Gs,Rp);
    Fs1 <- combineFaces(Fs,G1,Rp);
    valis [F,..Fs1]
  }

  mergeField(_,_,_,[],_) => ok([]).
  mergeField(Lc,Id,Tp,[A,..As],Rp) => do{
    if (Lc2,Id,Tp2) ^= isTypeAnnotation(A) then {
      if Tp==Tp2 then
	valis As
      else
      raise reportError(Rp,"type associated with $(Id) at $(Lc) incompatible with $(Tp2)",Lc2)
    } else{
      A1 <- mergeField(Lc,Id,Tp,As,Rp);
      valis [A,..A1]
    }
  }

  conArities:(ast,map[string,integer]) => map[string,integer].
  conArities(A,Axx) where (Lc,L,R) ^= isBinary(A,"|") =>
    conArities(L,conArities(R,Axx)).
  conArities(A,Axx) where (_,Nm,_,_,Els) ^= isBraceCon(A) =>
    Axx[Nm->size(Els)].
  conArities(_,Axx) default => Axx.

  buildConIndices:(ast,map[string,map[string,integer]]) => map[string,map[string,integer]].
  buildConIndices(A,Ixx) where (Lc,L,R) ^= isBinary(A,"|") =>
    buildConIndices(L,buildConIndices(R,Ixx)).
  buildConIndices(A,Ixx) where (Lc,Nm,XQs,XCx,Els) ^= isBraceCon(A) =>
    buildConIx(Els,Nm,Ixx).
  buildConIndices(_,Ixx) default => Ixx.

  buildConIx:(cons[ast],string,map[string,map[string,integer]])=>map[string,map[string,integer]].
  buildConIx(Els,Nm,Ixx) => let{
    pickCon:(ast,(map[string,map[string,integer]],integer))=>
      (map[string,map[string,integer]],integer).
    pickCon(El,(Mp,Ix)) where
	(_,F,_) ^= isTypeAnnotation(El) && (_,Fld)^=isName(F) =>
      (Mp[Fld->recordIx(Mp[Fld],Nm,Ix)],Ix+1).
    pickCon(_,U) default => U.
  } in fst(foldLeft(pickCon,(Ixx,0),sort(Els,compEls))).

  recordIx:(option[map[string,integer]],string,integer) => map[string,integer].
  recordIx(some(NIx),Rc,Off) => NIx[Rc->Off].
  recordIx(.none,Rc,Off) => [Rc->Off].

  buildConstructors:(ast,cons[ast],cons[ast],ast,visibility,reports)=>result[reports,cons[ast]].
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
      } in ok([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm,XQs,XCx,Els) ^= isRoundCon(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,Els),Tp)))).
      } in ok([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,Vz,Rp) where
      (Lc,Nm) ^= isEnumSymb(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,[]),Tp)))).
      } in ok([reveal(Con,Vz)]).
  buildConstructors(A,Qs,Cx,Tp,_,Rp) where
      (_,I) ^= isPrivate(A) => 
    buildConstructors(I,Qs,Cx,Tp,.priVate,Rp).
  buildConstructors(A,Qs,Cx,Tp,_,Rp) where
      (_,I) ^= isPublic(A) => 
    buildConstructors(I,Qs,Cx,Tp,.pUblic,Rp).
  buildConstructors(A,_,_,_,_,Rp) =>
    bad(reportError(Rp,"cannot fathom constructor $(A)",locOf(A))).

  compEls:(ast,ast)=>boolean.
  compEls(A,B) where
      (_,N1,_) ^= isTypeAnnotation(A) && (_,Nm1) ^= isName(N1) &&
      (_,N2,_) ^= isTypeAnnotation(B) && (_,Nm2) ^= isName(N2) => Nm1<Nm2.
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


  buildAccessors:(ast,cons[ast],cons[ast],ast,string,cons[ast],
    visibility,reports)=>result[reports,cons[ast]].
  buildAccessors(Rhs,Q,Cx,H,TpNm,Fields,Vz,Rp) =>
    seqmap((F)=>makeAccessor(F,TpNm,Rhs,Q,Cx,H,Vz,Rp),Fields).

  buildUpdaters:(ast,cons[ast],cons[ast],ast,string,cons[ast],
    visibility,reports)=>result[reports,cons[ast]].
  buildUpdaters(Rhs,Q,Cx,H,TpNm,Fields,Vz,Rp) =>
    seqmap((F)=>makeUpdater(F,TpNm,Rhs,Q,Cx,H,Vz,Rp),Fields).
  
  makeAccessor:(ast,string,ast,cons[ast],cons[ast],ast,
    visibility,reports) => result[reports,ast].
  makeAccessor(Annot,TpNm,Cns,Q,Cx,H,Vz,Rp) where (Lc,Fld,FldTp)^=isTypeAnnotation(Annot) => do{
    AcEqs <- accessorEqns(Cns,Fld,[],Rp);
    AccNm .= nme(Lc,".$(Fld)");
    AccessHead .= squareTerm(Lc,AccNm,[mkDepends(Lc,[H],[FldTp])]);
    valis mkAccessorStmt(Lc,Q,Cx,AccessHead,mkLetDef(Lc,AcEqs,AccNm))
  }

  accessorEqns:(ast,ast,cons[ast],reports)=>result[reports,cons[ast]].
  accessorEqns(Cns,Fld,SoFar,Rp) where (Lc,L,R)^=isBinary(Cns,"|") => do{
    E1 <- accessorEqns(L,Fld,SoFar,Rp);
    accessorEqns(R,Fld,E1,Rp)
  }
  accessorEqns(Cns,Fld,SoFar,Rp) where (Lc,CnNm,Els)^=isBrTerm(Cns) => do{
    Sorted .= sort(Els,compEls);
    ConArgs .= projectArgTypes(Sorted,Fld);
    Eqn .=mkEquation(Lc,some(nme(Lc,".$(Fld)")),.false,
      rndTuple(Lc,[roundTerm(Lc,CnNm,ConArgs)]),.none,nme(Lc,"X"));
    valis [Eqn,..SoFar]
  }
  accessorEqns(C,Fld,Eqns,Rp) where (Lc,I) ^= isPrivate(C) =>
    accessorEqns(I,Fld,Eqns,Rp).
  accessorEqns(C,Fld,Eqns,_) default => do{
    valis Eqns
  }

  projectArgTypes([],_) => [].
  projectArgTypes([A,..As],F) where
      (Lc,V,T) ^= isTypeAnnotation(A) && F== V =>
    [nme(Lc,"X"),..projectArgTypes(As,F)].
  projectArgTypes([A,..As],F) where (Lc,V,T) ^= isTypeAnnotation(A) =>
    [anon(Lc),..projectArgTypes(As,F)].
  projectArgTypes([_,..As],F) => projectArgTypes(As,F).

  makeUpdater:(ast,string,ast,cons[ast],cons[ast],ast,
    visibility,reports) => result[reports,ast].
  makeUpdater(Annot,TpNm,Cns,Q,Cx,H,Vz,Rp) where (Lc,Fld,FldTp)^=isTypeAnnotation(Annot) => do{
    UpEqs <- updaterEqns(Cns,Fld,[],Rp);
    AccNm .= nme(Lc,":$(Fld)");
    AccessHead .= squareTerm(Lc,AccNm,[mkDepends(Lc,[H],[FldTp])]);
    valis mkUpdaterStmt(Lc,Q,Cx,AccessHead,mkLetDef(Lc,UpEqs,AccNm))
  }

  updaterEqns:(ast,ast,cons[ast],reports)=>result[reports,cons[ast]].
  updaterEqns(Cns,Fld,SoFar,Rp) where (Lc,L,R)^=isBinary(Cns,"|") => do{
    E1 <- updaterEqns(L,Fld,SoFar,Rp);
    updaterEqns(R,Fld,E1,Rp)
  }
  updaterEqns(Cns,Fld,SoFar,Rp) where (Lc,CnNm,Els)^=isBrTerm(Cns) => do{
    Sorted .= sort(Els,compEls);
    ConArgs .= projectArgTypes(Sorted,Fld);
    UEqn .= mkEquation(Lc,some(nme(Lc,":$(Fld)")),.false,
      rndTuple(Lc,[roundTerm(Lc,CnNm,allArgs(Sorted,Fld,0,anon(Lc))),nme(Lc,"XX")]),.none,
      roundTerm(Lc,CnNm,allArgs(Sorted,Fld,0,nme(Lc,"XX"))));
    valis [UEqn,..SoFar]
  }
  updaterEqns(C,Fld,Eqns,Rp) where (Lc,I) ^= isPrivate(C) =>
    updaterEqns(I,Fld,Eqns,Rp).
  updaterEqns(C,Fld,Eqns,_) default => do{
    valis Eqns
  }
  

  
    
  allArgs([],_,_,_) => [].
  allArgs([A,..As],F,Ix,Rp) where
      (Lc,V,T) ^= isTypeAnnotation(A) && F == V =>
    [Rp,..allArgs(As,F,Ix+1,Rp)].
  allArgs([A,..As],F,Ix,Rp) where (Lc,V,T) ^= isTypeAnnotation(A) =>
    [nme(Lc,"X$(Ix)"),..allArgs(As,F,Ix+1,Rp)].
  allArgs([_,..As],F,Ix,Rp) => allArgs(As,F,Ix,Rp).

  makeReturn(Lc,E) => unary(Lc,"_valis",E).

  makeRaise(Lc,E) => unary(Lc,"_raise",E).

  makeSequence(Lc,P,E,Cont) =>
    binary(Lc,"_sequence",
      E, mkEquation(Lc,.none,.false,rndTuple(Lc,[P]),.none,Cont)).

  combine(A,.none) => A.
  combine(A,some((Lc,Cont))) =>
    makeSequence(Lc,anon(Lc),A,Cont).

  public makeAction:(ast,option[(locn,ast)],reports)=>result[reports,ast].
  makeAction(A,Cont,Rp) where (_,[E]) ^= isBrTuple(A) =>
    makeAction(E,Cont,Rp).
  makeAction(A,some((_,Cont)),_) where (_,"nothing") ^= isName(A) => ok(Cont).
  makeAction(A,.none,_) where (Lc,"nothing") ^= isName(A) =>
    ok(makeReturn(Lc,unit(Lc))).
  makeAction(A,some((_,Cont)),_) where (_,[]) ^= isBrTuple(A) => ok(Cont).
  makeAction(A,.none,_) where (Lc,[]) ^= isBrTuple(A) =>
    ok(makeReturn(Lc,unit(Lc))).
  makeAction(A,Cont,Rp) where  (Lc,L,R) ^= isSequence(A) => do{
    Rr <- makeAction(R,Cont,Rp);
    makeAction(L,some((Lc,Rr)),Rp)
  }
  makeAction(A,Cont,Rp) where  (Lc,L) ^= isUnary(A,";") =>
    makeAction(L,Cont,Rp).
  makeAction(A,.none,_) where (Lc,E) ^= isValis(A) =>
    ok(makeReturn(Lc,E)).
  makeAction(A,some((OLc,_)),Rp) where (Lc,_) ^= isValis(A) =>
    bad(reportError(Rp,"$(A) must be the last action, previous action at $(OLc)",Lc)).
  makeAction(A,_,Rp) where (Lc,E) ^= isThrow(A) =>
    bad(reportError(Rp,"throe $(E) not supported",Lc)).
  makeAction(A,_,_) where (Lc,E) ^= isRaise(A) =>
    ok(makeRaise(Lc,E)).
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isBind(A) =>
    bad(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isBind(A) =>
    ok(makeSequence(Lc,L,R,Cont)).
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isMatch(A) =>
    bad(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isMatch(A) =>
    ok(roundTerm(Lc,mkEquation(Lc,.none,.false,rndTuple(Lc,[L]),.none,Cont),
	[R])).
  makeAction(A,.none,Rp) where (Lc,L,R) ^= isOptionMatch(A) =>
    bad(reportError(Rp,"$(A) may not be the last action",Lc)).
  makeAction(A,some((CLc,Cont)),Rp) where (Lc,L,R) ^= isOptionMatch(A) =>
    ok(roundTerm(Lc,mkEquation(Lc,.none,.false,
	  rndTuple(Lc,[unary(Lc,"some",L)]),.none,Cont),[R])).
  makeAction(A,Cont,Rp) where (Lc,S,F,T,R) ^= isSplice(A) =>
    ok(combine(mkAssignment(Lc,S,
	  roundTerm(Lc,nme(Lc,"_splice"),[refCell(Lc,S),F,T,R])),Cont)).
  makeAction(A,Cont,Rp) where (Lc,B,H) ^= isTryCatch(A) => do{
    Bx <- makeAction(B,.none,Rp);
    Hx <- makeHandler(H,Rp);
    valis combine(binary(Lc,"_handle",Bx,Hx),Cont)
  }
  makeAction(A,Cont,Rp) where (Lc,T,L,R) ^= isIfThenElse(A) => do{
    Lx <- makeAction(L,.none,Rp);
    Rx <- makeAction(R,.none,Rp);
    valis combine(mkConditional(Lc,T,Lx,Rx),Cont)
  }
  makeAction(A,Cont,Rp) where (Lc,T,L) ^= isIfThen(A) => do{
    Lx <- makeAction(L,.none,Rp);
    valis combine(mkConditional(Lc,T,Lx,makeReturn(Lc,unit(Lc))),Cont)
  }
  /* Construct a local loop function for while:
  let{.
    loop() => do{ if T then { B; loop() }}
  .} in loop()
  */
  makeAction(A,Cont,Rp) where (Lc,T,B) ^= isWhileDo(A) => do{
    LpNm .= genName(Lc,"loop");
    Lp .= roundTerm(Lc,LpNm,[]);
    Bx <- makeAction(B,.none,Rp);
    Body .= makeSequence(Lc,anon(Lc),Bx,Lp);
    LoopEqn .= mkEquation(Lc,some(LpNm),.true,unit(Lc),.none,
      mkConditional(Lc,T,Body,makeReturn(Lc,unit(Lc))));
    valis mkLetRecDef(Lc,[LoopEqn],Lp)
  }
  /* Construct a local loop function for until:
    let{.
      loop() => do{ B; if T then loop()}
    .} in loop()
  */
  makeAction(A,Cont,Rp) where (Lc,B,T) ^= isUntilDo(A) => do{
    LpNm .= genName(Lc,"loop");
    Lp .= roundTerm(Lc,LpNm,[]);
    Bx <- makeAction(B,.none,Rp);
    Body .= makeSequence(Lc,anon(Lc),Bx,
      mkConditional(Lc,T,Lp,makeReturn(Lc,unit(Lc))));
    LoopEqn .= mkEquation(Lc,some(LpNm),.true,unit(Lc),.none,Body);
    valis mkLetRecDef(Lc,[LoopEqn],Lp)
  }
  /*
    for X in S do Act
    becomes
  _iter(S,_valis(()),let{
        lP(X,St) => _sequence(St,(X)=>_sequence(Act,(_)=>_valis(())))
        lP(_,St) => St
       } in lP)
  */
  makeAction(A,Cont,Rp) where (Lc,Q,Act) ^= isForDo(A) && (_,X,S) ^= isSearch(Q) => do{
    LpNm .= genName(Lc,"lp");
    St .= genName(Lc,"st");
    Ax <- makeAction(Act,.none,Rp);
    UV .= makeReturn(Lc,unit(Lc));
    Bx .= makeSequence(Lc,X,St,makeSequence(Lc,anon(Lc),Act,UV));
    Eq1 .= mkEquation(Lc,some(LpNm),.false,rndTuple(Lc,[X,St]),.none,Bx);
    Eq2 .= mkEquation(Lc,some(LpNm),.true,rndTuple(Lc,[anon(Lc),St]),.none,St);
    LpDef .= mkLetRecDef(Lc,[Eq1,Eq2],LpNm);
    valis ternary(Lc,"_iter",S,UV,LpDef)
  }
  makeAction(A,Cont,Rp) where (Lc,I) ^= isPerform(A) => do{
    valis combine(I,Cont)
  }
  makeAction(A,Cont,Rp) where (Lc,Defs,B) ^= isLetDef(A) => do{
    Bx <- makeAction(B,.none,Rp);
    valis mkLetDef(Lc,Defs,Bx)
  }
  makeAction(A,Cont,Rp) where (Lc,Defs,B) ^= isLetRecDef(A) => do{
    Bx <- makeAction(B,.none,Rp);
    valis mkLetRecDef(Lc,Defs,Bx)
  }
  makeAction(A,Cont,Rp) where (Lc,G,Cases) ^= isCase(A) => do{
    CC <- seqmap((C)=>makeCase(C,Rp),Cases);
    valis mkCaseExp(Lc,G,CC)
  }
  makeAction(A,Cont,Rp) where (Lc,_,_) ^= isRoundTerm(A) => do{
    valis combine(A,Cont)
  }
  makeAction(A,_,Rp) default =>
    bad(reportError(Rp,"$(A) is not a valid action",locOf(A))).
  
  makeHandler(H,Rp) where (Lc,[S]) ^= isBrTuple(H) => do{
    Sx <- makeAction(S,.none,Rp);
    valis mkEquation(Lc,.none,.false,rndTuple(Lc,[anon(Lc)]),.none,Sx)
  }
  makeHandler(H,_Rp) => ok(H).

  makeCase(A,Rp) where (Lc,D,L,C,R) ^= isLambda(A) => do{
    RR <- makeAction(R,.none,Rp);
    valis mkLambda(Lc,D,L,C,RR)
  }
  


  
    
}
