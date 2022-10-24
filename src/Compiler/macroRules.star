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

  public macroRule ~> (ast,macroContext) => macroState.

  public macroState ::= .inactive | active(ast) .

  public implementation display[macroState] => {
    disp(.inactive) => "inactive".
    disp(.active(A)) => "active $(A)".
  }

  public applyRules:(ast,macroContext,macroState) => macroState.
  applyRules(A,Cxt,St) where Rules?=macros[macroKey(A)] =>
    applyRls(A,Cxt,St,Rules).
  applyRules(A,_,_) default => .inactive.

  applyRls:(ast,macroContext,macroState,cons[(macroContext,macroRule)]) => macroState.
  applyRls(A,_,St,[]) => St.
  applyRls(A,Cxt,St,[(Cxt,R),..Rls]) => valof{
    Rslt = R(A,Cxt);
    if .inactive.=Rslt then
      valis applyRls(A,Cxt,St,Rls)
    else
    valis Rslt
  }
  applyRls(A,Cxt,St,[_,..Rls]) => 
    applyRls(A,Cxt,St,Rls).
  
  macros:map[string,cons[(macroContext,macroRule)]].
  macros = { "::=" -> [(.statement,algebraicMacro)],
    ":=" -> [(.actn,spliceAssignMacro), (.actn,indexAssignMacro)],
    "[]" -> [(.pattern,squarePtnMacro), (.expression,squareSequenceMacro)],
    "$[]" -> [(.expression,indexMacro),(.expression,sliceMacro)],
    "<||>" -> [(.expression,quoteMacro)],
    "::" -> [(.expression,coercionMacro)],
    ":?" -> [(.expression,coercionMacro)],
    "*" -> [(.expression,multicatMacro)],
    "{}" -> [(.expression,comprehensionMacro),(.expression,mapLiteralMacro)],
    "{!!}" -> [(.expression,iotaComprehensionMacro)],
    "{??}" -> [(.expression,testComprehensionMacro)],
    "__pkg__" -> [(.expression,pkgNameMacro)],
    "__loc__" -> [(.expression,locationMacro)],
    "-" -> [(.expression, uMinusMacro),(.pattern, uMinusMacro)],
    "?=" -> [(.expression, optionMatchMacro)],
    "?" -> [(.expression, optionMacro), (.pattern, optionMacro)],
    "^" -> [(.expression,unwrapMacro),(.expression,optvalMacro)],
    "!" -> [(.expression,binRefMacro)],
    "do" -> [(.actn,forLoopMacro)],
    "contract" -> [(.statement,contractMacro)],
    "implementation" -> [(.statement,implementationMacro)],
    "assert" -> [(.actn,assertMacro)],
    "show" -> [(.actn,showMacro)],
    "trace" -> [(.expression,traceMacro)],
    "\${}" -> [(.expression,taskMacro)],
    "generator" -> [(.expression,generatorMacro)],
    "task" -> [(.expression,taskMacro)],
    "yield" -> [(.actn,yieldMacro)],
    "->" -> [(.expression,arrowMacro),(.pattern,arrowMacro)]
  }.

  -- Convert assert C to assrt(C,"failed C",Loc)
  assertMacro(A,.actn) where (Lc,C) ?= isIntegrity(A) => valof{
    Assert = ternary(Lc,"assrt",C,.str(Lc,C::string),mkLoc(Lc));
    valis .active(Assert)
  }
  assertMacro(_,_) default => .inactive.

  -- Convert show E to shwMsg(E,"E",Lc)
  showMacro(A,.actn) where (Lc,E) ?= isShow(A) => valof{
    Shw = ternary(Lc,"shwMsg",E,.str(Lc,E::string),mkLoc(Lc));
    valis .active(Shw)
  }
  showMacro(_,.actn) default => .inactive.

  -- Convert trace E to traceCall("value of "E" is ",E)
  traceMacro(T,.expression) where (Lc,Exp) ?= isTrace(T) => valof{
    Tr = binary(Lc,"traceCall",.str(Lc,"$(Lc)\: #(Exp::string)"),Exp);
    valis .active(Tr)
  }
  traceMacro(_,_) default => .inactive.

  -- Convert A![X] to (A!)[X]
  binRefMacro(A,.expression) where (Lc,Lhs,Rhs) ?= isBinary(A,"!") &&
      (LLc,[E]) ?= isSqTuple(Rhs) =>
    .active(squareTerm(LLc,refCell(Lc,Lhs),[E])).
  binRefMacro(A,.expression)  where (Lc,_,_) ?= isBinary(A,"!") => valof{
    reportError("illegal use of !",Lc);
    valis .inactive
  }
  binRefMacro(A,_) default => .inactive.

  -- Convert ?E to .some(E)
  optionMacro(A,.pattern) where (Lc,R) ?= isOption(A) =>
    .active(mkOption(Lc,R)).
  optionMacro(A,.expression) where (Lc,R) ?= isOption(A) =>
    .active(mkOption(Lc,R)).
  optionMacro(_,_) default => .inactive.

  -- Convert P?=E to some(P).=E
  optionMatchMacro(A,.expression) where (Lc,L,R) ?= isOptionMatch(A) =>
    .active(mkMatch(Lc,unary(Lc,"some",L),R)).
  optionMatchMacro(_,_) default => .inactive.

  mkLoc(Lc) where .locn(P,Line,Col,Off,Ln)?=Lc =>
    roundTerm(Lc,.nme(Lc,"locn"),
      [.str(Lc,P),.int(Lc,Line),.int(Lc,Col),.int(Lc,Off),.int(Lc,Ln)]).

  -- Handle __loc__ macro symbol
  locationMacro(A,.expression) where
      (Lc,"__loc__") ?= isName(A) =>
    .active(mkLoc(Lc)).
  
  -- Handle occurrences of __pkg__
  pkgNameMacro(A,.expression) where
      (Lc,"__pkg__") ?= isName(A) && .locn(Pkg,_,_,_,_)?=Lc =>
    .active(str(Lc,Pkg)).

  -- Convert expression P^E to X where E ?= P(X)
  unwrapMacro(A,.expression) where
      (Lc,Lhs,Rhs) ?= isOptionPtn(A) && (LLc,Nm) ?= isName(Lhs) => valof{
	X = genName(LLc,"X");
   	valis .active(mkWhere(Lc,X,mkMatch(LLc,Rhs,unary(LLc,Nm,X))))
      }.
  unwrapMacro(_,_) default => .inactive.

  -- Convert expression ^E to _optval(E)
  optvalMacro(A,.expression) where (Lc,Rhs) ?= isOptVal(A) =>
    .active(unary(Lc,"_optval",Rhs)).
  optvalMacro(_,_) default => .inactive.
  
  -- Convert unary minus to a call to __minus
  uMinusMacro(A,_) where (Lc,R) ?= isUnary(A,"-") =>
    (.int(_,Ix) .= R ?
	.active(int(Lc,-Ix)) ||
	.num(_,Dx) .= R ?
	  .active(.num(Lc,-Dx)) ||
	  .active(unary(Lc,"__minus",R))).
  uMinusMacro(_,_) => .inactive.

  -- Convert [P,...P] to _hdtl etc.
  squarePtnMacro(A,.pattern) where (Lc,Els) ?= isSqTuple(A) =>
    .active(macroListEntries(Lc,Els,genEofTest,genHedTest)).
  squarePtnMacro(_,_) => .inactive.

  macroListEntries:(option[locn],cons[ast],(option[locn])=>ast,(option[locn],ast,ast)=>ast) => ast.
  macroListEntries(Lc,[],End,_) => End(Lc).
  macroListEntries(_,[Cns],_,Hed) where (Lc,H,T) ?= isCons(Cns) =>
    Hed(Lc,H,T).
  macroListEntries(Lc,[El,..Rest],Eof,Hed) =>
    Hed(Lc,El,macroListEntries(Lc,Rest,Eof,Hed)).

  genEofTest(Lc) => mkWhereTest(Lc,"_eof").
  genHedTest(Lc,H,T) => mkWherePtn(Lc,.tpl(Lc,"()",[H,T]),nme(Lc,"_hdtl")).

  squareSequenceMacro(A,.expression) where
      (Lc,Els) ?= isSqTuple(A) =>
    .active(macroListEntries(Lc,Els,genNil,genCons)).
  squareSequenceMacro(_,_) => .inactive.

  genNil(Lc) => nme(Lc,"_nil").
  genCons(Lc,H,T) => binary(Lc,"_cons",H,T).

  mapLiteralMacro(A,.expression) where
      (Lc,Els) ?= isMapLiteral(A) => .active(macroListEntries(Lc,Els,genEmpty,genPut)).
  mapLiteralMacro(_,_) default => .inactive.

  genEmpty(Lc) => nme(Lc,"_empty").
  genPut(Lc,H,T) where (_,L,R) ?= isPair(H) => ternary(Lc,"_put",T,L,R).

  comprehensionMacro(A,.expression) where (Lc,B,C) ?= isComprehension(A) => valof{
    Q = makeComprehension(Lc,B,C);
    valis .active(Q)
  }
  comprehensionMacro(_,_) => .inactive.

  -- Convert {! Bnd | Body !}
  iotaComprehensionMacro(A,.expression) where
      (Lc,Bnd,Body) ?= isIotaComprehension(A) => valof{
	CC = makeCondition(Body,passThru,
	  genResult(unary(Lc,"some",Bnd)),
	  lyfted(enum(Lc,"none")));
	valis .active(CC)
      }.
  iotaComprehensionMacro(_,_) default => .inactive.

  -- Convert {? Cond ?}
  testComprehensionMacro(A,.expression) where
      (Lc,Cond) ?= isTestComprehension(A) => valof{
	CC = makeCondition(Cond,passThru,
	  genResult(enum(Lc,"true")),
	  .lyfted(enum(Lc,"false")));
	valis .active(CC)
      }
  testComprehensionMacro(_,_) => .inactive.
	
  makeComprehension:(option[locn],ast,ast) => ast.
  makeComprehension(Lc,Bnd,Cond) => 
    makeCondition(Cond,passThru,(St)=>consResult(Lc,Bnd,St),
      .grounded(.nme(Lc,"_nil"))).

  -- Condition processing

  lyfted[a] ::= .lyfted(a) | .grounded(a).

  passThru(.grounded(X)) => X.
  passThru(.lyfted(X)) => X.

  consResult(Lc,Bnd,.grounded(St)) => binary(Lc,"_cons",Bnd,St).
  consResult(_,_,.lyfted(St)) => St.

  genResult(T) => let{
    chk(.grounded(_))=>T.
    chk(.lyfted(St)) => St.
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
  makeCondition:(ast,(lyfted[ast])=>ast,(lyfted[ast])=>ast,lyfted[ast]) => ast.
  makeCondition(A,Lift,Succ,Zed) where (Lc,Ptn,Src) ?= isSearch(A) => valof{
    sF = genName(Lc,"sF");
    St = genName(Lc,"St");

    Eq1 = equation(Lc,roundTerm(Lc,sF,[Ptn,St]),Succ(.grounded(St)));
    Eq2 = equation(Lc,roundTerm(Lc,sF,[mkAnon(Lc),St]),Lift(.grounded(St)));
    
    FF = mkLetDef(Lc,[Eq1,Eq2],sF);
    valis ternary(Lc,"_iter",Src,Lift(Zed),FF)
  }
  makeCondition(A,Lift,Succ,Zed) where (Lc,L,R) ?= isConjunct(A) =>
    makeCondition(L,Lift,(Lf) => makeCondition(R,Lift,Succ,Lf),Zed).
  makeCondition(A,Lift,Succ,Zed) where (Lc,L,R) ?= isDisjunct(A) => valof{
    E1 =makeCondition(L,Lift,Succ,Zed);
    valis makeCondition(R,Lift,Succ,.lyfted(E1))
  }
  makeCondition(A,Lift,Succ,Zed) where (Lc,R) ?= isNegation(A) => valof{
    Negated = makeCondition(R,Lift,(_)=>Lift(.grounded(enum(Lc,"true"))),
      .grounded(enum(Lc,"false")));
    St = genName(Lc,"St");
    SuccCase = Succ(Zed);
    FalseCase = Lift(Zed);
    valis mkConditional(Lc,Negated,FalseCase,SuccCase)
  }
  makeCondition(A,Lift,Succ,Zed) where (Lc,L,R) ?= isImplies(A) =>
    makeCondition(negated(Lc,mkConjunct(Lc,L,negated(Lc,R))),Lift,Succ,Zed).
  makeCondition(Other,Lift,Succ,Zed) => 
    mkConditional(locOf(Other),Other,Succ(Zed),Lift(Zed)).

  -- Convert E::T to _optval(_coerce(E)):T
  -- Convert E:?T to _coerce(E):option[T]

  coercionMacro(A,.expression) where (Lc,L,R) ?= isCoerce(A) =>
    .active(typeAnnotation(Lc,unary(Lc,"_optval",unary(Lc,"_coerce",L)),R)).
  coercionMacro(A,.expression) where (Lc,L,R) ?= isOptCoerce(A) =>
    active(typeAnnotation(Lc,unary(Lc,"_coerce",L),sqUnary(Lc,"option",R))).
  coercionMacro(_,_) => .inactive.

  indexMacro(A,.expression) where (Lc,L,R) ?= isIndexTerm(A) =>
    ((_,Ky,Vl) ?= isBinary(R,"->") ?
	active(ternary(Lc,"_put",L,Ky,Vl)) ||
	(_,Ky) ?= isNegation(R) ?
	  active(binary(Lc,"_remove",L,Ky)) ||
	  active(binary(Lc,"_index",L,R))).
  indexMacro(_,_) default => .inactive.

  -- C[I]:=R becomes C:=_put(C!,I,R)

  indexAssignMacro(A,.actn) where
      (Lc,L,R) ?= isAssignment(A) &&
      (LLc,S,I) ?= isIndexTerm(L) =>
    active(mkAssignment(Lc,S,
	  ternary(Lc,"_put",refCell(Lc,S),I,R))).
  indexAssignMacro(_,_) default => .inactive.
  
  sliceMacro(A,.expression) where (Lc,Op,F,T) ?= isSlice(A) =>
    active(ternary(Lc,"_slice",Op,F,T)).
  sliceMacro(_,_) default => .inactive.

  spliceAssignMacro(A,.expression) where
      (Lc,S,F,T,R) ?= isSplice(A) =>
    active(mkAssignment(Lc,S,
	  roundTerm(Lc,nme(Lc,"_splice"),
	    [refCell(Lc,S),F,T,R]))).
  spliceAssignMacro(_,_) default => .inactive.
  
  multicatMacro(A,.expression) where (Lc,E) ?= isUnary(A,"*") =>
    active(unary(Lc,"_multicat",E)).
  multicatMacro(_,.expression) => .inactive.

  -- Convert <| E |> to a quote expression
  -- which relies on the quote contract
  quoteMacro(A,.expression) where (Lc,Trm) ?= isQuoted(A) =>
    active(quoteAst(A)).
  quoteMacro(_,_) => .inactive.

  -- quoteAst creates an ast that checks to a quoted form of the ast itself
  quoteAst(E) where (_,X) ?= isUnary(E,"?") => X.
  quoteAst(.nme(Lc,Nm)) => unary(Lc,"_name",.str(Lc,Nm)).
  quoteAst(.qnm(Lc,Nm)) => unary(Lc,"_qnme",.str(Lc,Nm)).
  quoteAst(.int(Lc,Nx)) => unary(Lc,"_integer",.int(Lc,Nx)).
  quoteAst(.num(Lc,Nx)) => unary(Lc,"_float",.num(Lc,Nx)).
  quoteAst(.chr(Lc,Cx)) => unary(Lc,"_char",.chr(Lc,Cx)).
  quoteAst(.str(Lc,Sx)) => unary(Lc,"_string",.str(Lc,Sx)).
  quoteAst(.tpl(Lc,Nm,Els)) => binary(Lc,"_tuple",.str(Lc,Nm),
    macroListEntries(Lc,Els,genNil,genCons)).
  quoteAst(.app(Lc,Op,Arg)) => binary(Lc,"_apply",quoteAst(Op),quoteAst(Arg)).
  
  /*
  for P in C do B
  becomes
  {
    I .= _generate(C);
    lb:while .true do{
      I resume ._next in {
        _yld(P) => B.
        _yld(_) default => {}.
        ._all => break lb
      }
    }
  }
  */

  forLoopMacro(A,.actn) where (Lc,P,C,B) ?= isForDo(A) => valof{
    I = genName(Lc,"I");
    Lb = genId("Lb");

    /* Build ._all => break Lb */
    End = mkLambda(Lc,.false,enum(Lc,"_all"),.none,mkBreak(Lc,Lb));

    /* build _yld(P) => B */
    Yld = mkLambda(Lc,.false,unary(Lc,"_yld",P),.none,B);

    /* build _yld(_) default => {} */
    Deflt = mkEquation(Lc,.none,.true,unary(Lc,"_yld",mkAnon(Lc)),.none,brTuple(Lc,[]));

    /* build I resume ._next in .. */
    Resume = mkResume(Lc,I,enum(Lc,"_next"),[Yld,Deflt,End]);

    /* Build while .true loop */
    Loop = mkWhileDo(Lc,enum(Lc,"true"),brTuple(Lc,[Resume]));

   /* Build Lb:while .true do .. */
    Lbld = mkLbldAction(Lc,Lb,Loop);

    /* Build call to _generate */
    IT = roundTerm(Lc,nme(Lc,"_generate"),[C]);
    S1 = mkDefn(Lc,I,IT);

    valis active(mkSequence(Lc,S1,Lbld))
  }
  forLoopMacro(_,.actn) default => .inactive.

  /* task{A} becomes task((this,first)=>A) */
  taskMacro(E,.expression) where (Lc,A) ?= isTaskTerm(E) =>
    active(unary(Lc,"task",
	mkLambda(Lc,.false,rndTuple(Lc,[nme(Lc,"this"),nme(Lc,"first")]),
	  .none,mkValof(Lc,brTuple(Lc,[A]))))).
  taskMacro(_,.expression) => .inactive.
    
  /* generator{A}
  becomes
  task{
    try{A*} catch {._cancel => {}};
    valis .all
  }
  */
   
  generatorMacro(E,.expression) where
    (Lc,A) ?= isUnary(E,"generator") &&
    (_,[B]) ?= isBrTuple(A) => valof{
	/* Build try ... catch ... */
  
	/* Build .cancel => {} */
	Catcher = mkLambda(Lc,.false,enum(Lc,"_cancel"),.none,brTuple(Lc,[]));

	Try = mkTryCatch(Lc,A,[Catcher]);

	All = mkValis(Lc,enum(Lc,"_all"));
	valis active(mkTaskTerm(Lc,mkSequence(Lc,Try,All)))
      }.
  generatorMacro(_,_) default => .inactive.

  /* yield E
   becomes
   suspend _yld(E) in {
     ._next => {}.
     ._cancel => throw ._cancel
   }
  */
  yieldMacro(A,.actn) where (Lc,E) ?= isUnary(A,"yield") => valof{
    /* build ._next => {} */
    Nxt = mkLambda(Lc,.false,enum(Lc,"_next"),.none,brTuple(Lc,[]));

    /* build ._cancel => throw ._cancel */
    Cancel = mkLambda(Lc,.false,enum(Lc,"_cancel"),.none,mkThrow(Lc,enum(Lc,"_cancel")));

    /* Build suspend */
    valis active(mkSuspend(Lc,nme(Lc,"this"),unary(Lc,"_yld",E),[Nxt,Cancel]))
  }

  contractMacro(A,.statement) where
      (Lc,Lhs,Els) ?= isContractStmt(A) && (_,Nm,Q,C,T) ?= isContractSpec(Lhs) &&
      (SLc,Op,As) ?= isSquareTerm(T) => valof{
	DlNm = hashName(Op);
	CTp =contractType(SLc,DlNm,As);
	ConTp = mkAlgebraicTypeStmt(Lc,Q,C,CTp,braceTerm(SLc,DlNm,Els));
	valis active(brTuple(Lc,[ConTp,mkCntrctStmt(Lc,Q,C,T,Els)]))
      }.
  contractMacro(A,_) default => .inactive.

  contractType(Lc,Op,[A]) where (_,L,R) ?= isDepends(A) =>
    squareTerm(Lc,Op,L++R).
  contractType(Lc,Op,A) =>
    squareTerm(Lc,Op,A).

  implementationMacro(A,.statement) where
      (Lc,Q,C,H,E) ?= isImplementationStmt(A) &&
      (_,Nm,_) ?= isSquareTerm(H) &&
      Ex ?= labelImplExp(E,Nm) => 
    active(mkImplementationStmt(Lc,Q,C,H,Ex)).
  implementationMacro(_,_) default => .inactive.

  labelImplExp(T,Nm) where (Lc,Els) ?= isBrTuple(T) =>
    some(braceTerm(Lc,hashName(Nm),Els)).
  labelImplExp(T,Nm) where (Lc,Els) ?= isQBrTuple(T) =>
    some(qbraceTerm(Lc,hashName(Nm),Els)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ?= isLetDef(T) &&
    EE ?= labelImplExp(Exp,Nm) =>
    some(mkLetDef(Lc,Els,EE)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ?= isLetRecDef(T) &&
      EE ?= labelImplExp(Exp,Nm) =>
    some(mkLetRecDef(Lc,Els,EE)).
  labelImplExp(T,_) default => .none.

  arrowMacro(E,_) where (Lc,Lhs,Rhs) ?= isBinary(E,"->") =>
    .active(mkEnumCon(Lc,.nme(Lc,"kv"),[Lhs,Rhs])).
  arrowMacro(_,_) default => .inactive.

  -- Handle algebraic type definitions

  algebraicMacro(St,.statement) where (Lc,Vz,Q,Cx,H,R) ?= isAlgebraicTypeStmt(St) => 
    active(brTuple(Lc,makeAlgebraic(Lc,Vz,Q,Cx,H,R))).
  algebraicMacro(_,_) default => .inactive.

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

  makeAlgebraic:(option[locn],visibility,cons[ast],cons[ast],ast,ast) => cons[ast].
  makeAlgebraic(Lc,Vz,Q,Cx,H,Rhs) => valof{
    (Qs,Xs,Face) = algebraicFace(Rhs,Q,[]);
    Fields = sort(Face,compEls);
    logMsg("algebraic fields $(Fields)");
    TpExSt = reveal(reUQuant(Lc,Qs,reConstrain(Cx,binary(Lc,"<~",H,reXQuant(Lc,Xs,brTuple(Lc,Fields))))),Vz);
--    logMsg("Type rule is $(TpExSt)");
    Cons = buildConstructors(Rhs,Q,Cx,H,Vz);
--    logMsg("Constructors are $(Cons)");
    Accs = buildAccessors(Rhs,Q,Cx,H,typeName(H),Fields,Vz);
--    logMsg("Accessors are $(Accs)");
    Ups = buildUpdaters(Rhs,Q,Cx,H,typeName(H),Fields,Vz);
--    logMsg("Updaters are $(Ups)");
    valis [TpExSt,..Cons++Accs++Ups]
  }

  algebraicFace:(ast,cons[ast],cons[ast]) => (cons[ast],cons[ast],cons[ast]).
  algebraicFace(A,Qs,Xs) where (_,L,R) ?= isBinary(A,"|") => valof{
    (Q1,X1,Lhs) = algebraicFace(L,Qs,Xs);
    (Qx,Xx,Rhs) = algebraicFace(R,Q1,X1);
    Fs = combineFaces(Lhs,Rhs);
    valis (Qx,Xx,Fs)
  }
  algebraicFace(A,Qs,Xs) where (Lc,_,_) ?= isRoundTerm(A) => (Qs,Xs,[]).
  algebraicFace(A,Qs,Xs) where (Lc,_) ?= isEnumSymb(A) => (Qs,Xs,[]).
  algebraicFace(A,Qs,Xs) where (Lc,_,_) ?= isEnumCon(A) => (Qs,Xs,[]).
  algebraicFace(A,Qs,Xs) where (Lc,_,Els) ?= isBrTerm(A) => (Qs,Xs,Els^/((E)=>_?=isTypeAnnotation(E))).
  algebraicFace(A,Qs,Xs) where (_,I) ?= isPrivate(A) =>
    algebraicFace(I,Qs,Xs).
  algebraicFace(A,Qs,Xs) where (_,I) ?= isPublic(A) =>
    algebraicFace(I,Qs,Xs).
  algebraicFace(A,Qs,Xs) where (_,X0,I) ?= isXQuantified(A) =>
    algebraicFace(I,Qs,Xs\/X0).
  algebraicFace(A,Qs,Xs) where (_,A0,I) ?= isQuantified(A) =>
    algebraicFace(I,Qs\/A0,Xs).
  algebraicFace(A,Qs,Xs) default => valof{
    reportError("invalid case in algebraic type",locOf(A));
    valis (Qs,Xs,[])
  }

  combineFaces([],F2) => F2.
  combineFaces(F1,[]) => F1.
  combineFaces([F,..Fs],Gs) where (Lc,Id,Tp)?=isTypeAnnotation(F) => valof{
    G1 = mergeField(Lc,Id,Tp,Gs);
    Fs1 = combineFaces(Fs,G1);
    valis [F,..Fs1]
  }

  mergeField(_,_,_,[]) => [].
  mergeField(Lc,Id,Tp,[A,..As]) => valof{
    if (Lc2,Id,Tp2) ?= isTypeAnnotation(A) then {
      if Tp==Tp2 then
	valis As
      else{
	reportError("type associated with $(Id) at $(Lc) incompatible with $(Tp2)",Lc2);
	valis As
      }
    } else{
      A1 = mergeField(Lc,Id,Tp,As);
      valis [A,..A1]
    }
  }

  conArities:(ast,map[string,integer]) => map[string,integer].
  conArities(A,Axx) where (Lc,L,R) ?= isBinary(A,"|") =>
    conArities(L,conArities(R,Axx)).
  conArities(A,Axx) where
      (_,Nm,_,_,Els) ?= isBraceCon(A) && (_,Id) ?= isName(Nm)=>
    Axx[Id->size(Els)].
  conArities(_,Axx) default => Axx.

  buildConIndices:(ast,map[string,map[string,integer]]) => map[string,map[string,integer]].
  buildConIndices(A,Ixx) where (Lc,L,R) ?= isBinary(A,"|") =>
    buildConIndices(L,buildConIndices(R,Ixx)).
  buildConIndices(A,Ixx) where
      (Lc,Nm,XQs,XCx,Els) ?= isBraceCon(A) &&
      (_,Id) ?= isName(Nm) =>
    buildConIx(Els,Id,Ixx).
  buildConIndices(_,Ixx) default => Ixx.

  buildConIx:(cons[ast],string,map[string,map[string,integer]])=>map[string,map[string,integer]].
  buildConIx(Els,Nm,Ixx) => let{
    pickCon:(ast,(map[string,map[string,integer]],integer))=>
      (map[string,map[string,integer]],integer).
    pickCon(El,(Mp,Ix)) where
	(_,F,_) ?= isTypeAnnotation(El) && (_,Fld)?=isName(F) =>
      (Mp[Fld->recordIx(Mp[Fld],Nm,Ix)],Ix+1).
    pickCon(_,U) default => U.
  } in fst(foldLeft(pickCon,(Ixx,0),sort(Els,compEls))).

  recordIx:(option[map[string,integer]],string,integer) => map[string,integer].
  recordIx(.some(NIx),Rc,Off) => NIx[Rc->Off].
  recordIx(.none,Rc,Off) => [Rc->Off].

  buildConstructors:(ast,cons[ast],cons[ast],ast,visibility)=>cons[ast].
  buildConstructors(A,Qs,Cx,Tp,Vz) where
      (Lc,L,R) ?= isBinary(A,"|") => valof{
	Dfs1 = buildConstructors(L,Qs,Cx,Tp,Vz);
	Dfs2 = buildConstructors(R,Qs,Cx,Tp,Vz);
	valis Dfs1++Dfs2
      }.
  buildConstructors(A,Qs,Cx,Tp,Vz) where
      (Lc,Nm,XQs,XCx,Es) ?= isBraceCon(A) => valof{
	Els = sort(Es^/((E)=>_?=isTypeAnnotation(E)),compEls);
	logMsg("constructor els $(Els)");
	BCon = typeAnnotation(Lc,Nm,reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",reXQuant(Lc,XQs,
		  reConstrain(XCx,brTuple(Lc,Els))),Tp))));
	DCon = typeAnnotation(Lc,dollarName(Nm),reUQuant(Lc,Qs,
	    reConstrain(Cx,
	      binary(Lc,"<=>",reXQuant(Lc,XQs,
		  reConstrain(XCx,rndTuple(Lc,projectTps(Els)))),Tp))));
	valis [reveal(BCon,Vz),reveal(DCon,Vz)]
      }
  buildConstructors(A,Qs,Cx,Tp,Vz) where
      (Lc,Nm,XQs,XCx,Els) ?= isRoundCon(A) => let{
	Con = typeAnnotation(Lc,Nm,reUQuant(Lc,Qs,
	  reConstrain(Cx,
	    binary(Lc,"<=>",rndTuple(Lc,Els),Tp)))).
      } in [reveal(Con,Vz)].
  buildConstructors(A,Qs,Cx,Tp,Vz) where
      (Lc,Nm) ?= isEnumSymb(A) => let{
	Con = typeAnnotation(Lc,nme(Lc,Nm),reUQuant(Lc,Qs,
	  reConstrain(Cx,
	      binary(Lc,"<=>",rndTuple(Lc,[]),Tp)))).
      } in [reveal(Con,Vz)].
  buildConstructors(A,Qs,Cx,Tp,_) where
      (_,I) ?= isPrivate(A) => 
    buildConstructors(I,Qs,Cx,Tp,.priVate).
  buildConstructors(A,Qs,Cx,Tp,_) where
      (_,I) ?= isPublic(A) => 
    buildConstructors(I,Qs,Cx,Tp,.pUblic).
  buildConstructors(A,_,_,_,_) => valof{
    reportError("cannot fathom constructor $(A)",locOf(A));
    valis []
  }

  projectTps([])=>[].
  projectTps([E,..Es]) where (_,_,T)?=isTypeAnnotation(E) => [T,..projectTps(Es)].
  projectTps([_,..Es]) => projectTps(Es).

  compEls:(ast,ast)=>boolean.
  compEls(A,B) where
      (_,N1,_) ?= isTypeAnnotation(A) && (_,Nm1) ?= isName(N1) &&
      (_,N2,_) ?= isTypeAnnotation(B) && (_,Nm2) ?= isName(N2) => Nm1<Nm2.
  compEls(_,_) default => .false.

  reveal(A,.priVate) => unary(locOf(A),"private",A).
  reveal(A,.pUblic) => unary(locOf(A),"public",A).
  reveal(A,_) default => A.

  visibilityOf:(ast) => (ast,visibility).
  visibilityOf(A) => visib(A,.deFault).

  visib(A,_) where (_,I) ?= isPrivate(A) => visib(I,.priVate).
  visib(A,_) where (_,I) ?= isPublic(A) => visib(I,.pUblic).
  visib(A,Vz) default => (A,Vz).

  public isBraceCon:(ast) => option[(option[locn],ast,cons[ast],cons[ast],cons[ast])].
  isBraceCon(A) => isCon(A,isBrTerm).

  public isRoundCon:(ast) => option[(option[locn],ast,cons[ast],cons[ast],cons[ast])].
  isRoundCon(A) => isCon(A,isEnumCon).

  isCon:(ast,(ast)=>option[(option[locn],ast,cons[ast])]) => option[(option[locn],ast,cons[ast],cons[ast],cons[ast])].
  isCon(A,P) where
      (Lc,Nm,Els) ?= P(A) && _ ?= isName(Nm) => some((Lc,Nm,[],[],Els)).
  isCon(A,P) where
      (Lc,Q,I) ?= isXQuantified(A) &&
      (_,Nm,_,Cx,Els) ?= isCon(I,P) =>
    some((Lc,Nm,Q,Cx,Els)).
  isCon(A,P) where
      (Lc,Cx,I) ?= isConstrained(A) &&
      (_,Nm,Q,_,Els) ?= isCon(I,P) =>
    some((Lc,Nm,Q,Cx,Els)).
  isCon(_,_) default => .none.


  buildAccessors:(ast,cons[ast],cons[ast],ast,string,cons[ast],visibility)=>cons[ast].
  buildAccessors(Rhs,Q,Cx,H,TpNm,Fields,Vz) =>
    (Fields//((F)=>makeAccessor(F,TpNm,Rhs,Q,Cx,H,Vz))).

  buildUpdaters:(ast,cons[ast],cons[ast],ast,string,cons[ast],
    visibility)=>cons[ast].
  buildUpdaters(Rhs,Q,Cx,H,TpNm,Fields,Vz) =>
    (Fields//((F)=>makeUpdater(F,TpNm,Rhs,Q,Cx,H,Vz))).
  
  makeAccessor:(ast,string,ast,cons[ast],cons[ast],ast,visibility) => ast.
  makeAccessor(Annot,TpNm,Cns,Q,Cx,H,Vz) where (Lc,Fld,FldTp)?=isTypeAnnotation(Annot) =>
    valof{
      AccNm = nme(Lc,"\$$(Fld)");
      AcEqs = accessorEqns(Cns,Fld,AccNm,[]);
      AccessHead = squareTerm(Lc,Fld,[mkDepends(Lc,[H],[FldTp])]);
      valis mkAccessorStmt(Lc,Q,Cx,AccessHead,mkLetDef(Lc,AcEqs,AccNm))
    }

  accessorEqns:(ast,ast,ast,cons[ast])=>cons[ast].
  accessorEqns(Cns,Fld,AccNm,SoFar) where (Lc,L,R)?=isBinary(Cns,"|") =>
    accessorEqns(R,Fld,AccNm,accessorEqns(L,Fld,AccNm,SoFar)).
  accessorEqns(Cns,Fld,AccNm,SoFar) where
      (Lc,CnNm,Els)?=isBrTerm(Cns) && isFieldOfFc(Els,Fld) => valof{
	Sorted = sort(Els,compEls);
	ConArgs = projectArgTypes(Sorted,Fld);
	
	Eqn = mkEquation(Lc,some(AccNm),.false,
	  rndTuple(Lc,[mkEnumCon(Lc,dollarName(CnNm),ConArgs)]),.none,nme(Lc,"X"));
	valis [Eqn,..SoFar]
  }
  accessorEqns(C,Fld,AccNm,Eqns) where (Lc,I) ?= isPrivate(C) =>
    accessorEqns(I,Fld,AccNm,Eqns).
  accessorEqns(C,Fld,AccNm,Eqns) default => Eqns.

  isFieldOfFc([F,..Els],Fld) where (_,Fld,_) ?= isTypeAnnotation(F) => .true.
  isFieldOfFc([_,..Els],Fld) => isFieldOfFc(Els,Fld).
  isFieldOfFc([],_) default => .false.

  projectArgTypes([],_) => [].
  projectArgTypes([A,..As],F) where
      (Lc,V,T) ?= isTypeAnnotation(A) && F== V =>
    [nme(Lc,"X"),..projectArgTypes(As,F)].
  projectArgTypes([A,..As],F) where (Lc,V,T) ?= isTypeAnnotation(A) =>
    [mkAnon(Lc),..projectArgTypes(As,F)].
  projectArgTypes([_,..As],F) => projectArgTypes(As,F).

  makeUpdater:(ast,string,ast,cons[ast],cons[ast],ast,visibility) => ast.
  makeUpdater(Annot,TpNm,Cns,Q,Cx,H,Vz) where (Lc,Fld,FldTp)?=isTypeAnnotation(Annot) => valof{
    AccNm = dollarName(Fld);
    UpEqs = updaterEqns(Cns,Fld,AccNm,[]);
    AccessHead = squareTerm(Lc,Fld,[mkDepends(Lc,[H],[FldTp])]);
    valis mkUpdaterStmt(Lc,Q,Cx,AccessHead,mkLetDef(Lc,UpEqs,AccNm))
  }

  updaterEqns:(ast,ast,ast,cons[ast])=>cons[ast].
  updaterEqns(Cns,Fld,AccNm,SoFar) where (Lc,L,R)?=isBinary(Cns,"|") =>
    updaterEqns(R,Fld,AccNm,updaterEqns(L,Fld,AccNm,SoFar)).
  updaterEqns(Cns,Fld,AccNm,SoFar) where
      (Lc,CnNm,Els)?=isBrTerm(Cns) && isFieldOfFc(Els,Fld) => valof{
	Sorted = sort(Els,compEls);
	UEqn = mkEquation(Lc,some(AccNm),.false,
	  rndTuple(Lc,[mkEnumCon(Lc,dollarName(CnNm),allArgs(Sorted,Fld,0,mkAnon(Lc))),nme(Lc,"XX")]),.none,
	  mkEnumCon(Lc,dollarName(CnNm),allArgs(Sorted,Fld,0,nme(Lc,"XX"))));
	valis [UEqn,..SoFar]
      }
  updaterEqns(C,Fld,AccNm,Eqns) where (Lc,I) ?= isPrivate(C) =>
    updaterEqns(I,Fld,AccNm,Eqns).
  updaterEqns(C,_,_,Eqns) default => Eqns.
  
  allArgs([],_,_,_) => [].
  allArgs([A,..As],F,Ix,Rep) where
      (Lc,V,T) ?= isTypeAnnotation(A) && F == V =>
    [Rep,..allArgs(As,F,Ix+1,Rep)].
  allArgs([A,..As],F,Ix,Rep) where (Lc,V,T) ?= isTypeAnnotation(A) =>
    [nme(Lc,"X$(Ix)"),..allArgs(As,F,Ix+1,Rep)].
  allArgs([_,..As],F,Ix,Rep) => allArgs(As,F,Ix,Rep).
}
