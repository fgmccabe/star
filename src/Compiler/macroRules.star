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
  macros = { -- "::=" -> [(.statement,algebraicMacro)],
    ":=" -> [(.actn,spliceAssignMacro),
      (.actn,indexAssignMacro)],
    "[]" -> [(.pattern,squarePtnMacro),
      (.expression,listComprehensionMacro),
      (.expression,squareSequenceMacro)],
    "$[]" -> [(.expression,indexMacro),(.expression,sliceMacro)],
    "<||>" -> [(.expression,quoteMacro)],
    "::" -> [(.expression,coercionMacro)],
    ":?" -> [(.expression,coercionMacro)],
    "*" -> [(.expression,multicatMacro)],
    "{}" -> [(.expression,comprehensionMacro),(.expression,mapLiteralMacro)],
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
  assertMacro(A,.actn,Rp) where (Lc,C) ^= isIntegrity(A) => do{
    Lam .= equation(Lc,tpl(Lc,"()",[]),C);
    Assert .= ternary(Lc,"assrt",Lam,str(Lc,C::string),Lc::ast);
    valis active(Assert)
  }

  -- Convert show E to shwMsg(()=>E,"E",Lc)
  showMacro(A,.actn,Rp) where (Lc,E) ^= isShow(A) => do{
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
    err(reportError(Rp,"illegal use of !",Lc)).
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
      (Lc,Lhs,Rhs) ^= isOptionPtn(A) && (LLc,Nm) ^= isName(Lhs) => do{
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
      (Lc,Els) ^= isSqTuple(A) &&
      ~_^=isListComprehension(A)=>
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

  listComprehensionMacro(A,.expression,Rp) where (Lc,B,C) ^= isListComprehension(A) => do{
    Q <- makeComprehension(Lc,B,C,Rp);
    valis active(binary(Lc,":",Q,squareTerm(Lc,nme(Lc,"cons"),[anon(Lc)])))
  }
  listComprehensionMacro(_,_,_) => ok(.inactive).

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
    sF .= nme(Lc,"sF");
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
    ok(active(unary(Lc,"_valof",E))).
  valofMacro(_,_,_) default =>
    ok(.inactive).

  tryCatchMacro(A,.actn,Rp) where
      (Lc,B,H) ^= isTryCatch(A) &&
      (LLc,[As]) ^= isBrTuple(H) => do{
	A .= rndTuple(LLc,[anon(LLc)]);
	Q .= equation(Lc,A,mkDoTerm(LLc,As));
	valis active(mkTryCatch(Lc,B,Q))
      }.
  tryCatchMacro(_,_,_) default => ok(.inactive).

  /*
  for X in L do Act
  becomes
  ignore _iter(L,ok(()),let{
    lP(_,err(E)) => err(E).
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
      (_,Nm,_) ^= isSquareTerm(H) => do{
	Ex .= labelImplExp(E,Nm);
	valis active(mkImplementationStmt(Lc,Q,C,H,Ex))
      }.
  implementationMacro(_,_,_) default => ok(.inactive).

  labelImplExp(T,Nm) where (Lc,Els) ^= isBrTuple(T) =>
    braceTerm(Lc,Nm,Els).
  labelImplExp(T,Nm) where (Lc,Els) ^= isQBrTuple(T) =>
    qbraceTerm(Lc,Nm,Els).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ^= isLetDef(T) =>
    mkLetDef(Lc,Els,labelImplExp(Exp,Nm)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ^= isLetRecDef(T) =>
    mkLetRecDef(Lc,Els,labelImplExp(Exp,Nm)).
  labelImplExp(T,_) default => T.
    
}
