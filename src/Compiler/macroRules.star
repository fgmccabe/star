star.compiler.macro.rules{
  import star.
  import star.sort.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.misc.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.macro.grammar.
  import star.compiler.macro.infra.
  import star.compiler.opts.
  import star.compiler.wff.

  public macroRule ~> (ast,macroContext) => macroState.

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
  macros = {
    ":=" -> [(.actn,spliceAssignMacro), (.actn,indexAssignMacro)],
    "[]" -> [(.pattern,squarePtnMacro), (.expression,squareSequenceMacro)],
    "$[]" -> [(.expression,indexMacro),(.expression,sliceMacro),(.typeterm,generatorTypeMacro)],
    "<||>" -> [(.expression,quoteMacro)],
    "::" -> [(.expression,coercionMacro)],
    ":?" -> [(.expression,coercionMacro)],
    ":" -> [(.actn,forLoopMacro),(.expression,rangeMacro)],
    "*" -> [(.expression,multicatMacro)],
    "|:" -> [(.rule,caseRuleMacro)],
    "{}" -> [(.expression,comprehensionMacro),
      (.expression,totalizerMacro),
      (.expression,mapLiteralMacro)],
    "{!!}" -> [(.expression,iotaComprehensionMacro)],
    "{??}" -> [(.expression,testComprehensionMacro)],
    "__pkg__" -> [(.expression,pkgNameMacro)],
    "__loc__" -> [(.expression,locationMacro)],
    "-" -> [(.expression, uMinusMacro),(.pattern, uMinusMacro)],
    "?=" -> [(.expression, optionMatchMacro)],
    "!" -> [(.expression,binRefMacro)],
    "do" -> [(.actn,forInLoopMacro),(.expression,doMacro)],
    "valof" -> [(.expression,valofMacro)],
    "collect" -> [(.expression,collectMacro)],
    "implementation" -> [(.statement,implementationMacro)],
    "assert" -> [(.actn,assertMacro)],
    "show" -> [(.actn,showMacro)],
    "trace" -> [(.expression,traceMacro)],
    "generator\${}" -> [(.expression,generatorMacro)],
    "task\${}" -> [(.expression,taskMacro)],
    "yield" -> [(.actn,yieldMacro)],
    "async" -> [(.typeterm,asyncMacro)],
    "->" -> [(.expression,arrowMacro),(.pattern,arrowMacro)],
    "-->" -> [(.statement,grammarMacro),
      (.typeterm,grammarTypeMacro),
      (.expression,grammarCallMacro)],
    "..<" -> [(.expression,incRangeMacro)],
    "..>" -> [(.expression,decRangeMacro)]
  }.

  -- Convert assert C to assrt(C,"failed C",Loc)
  assertMacro(A,.actn) where (Lc,C) ?= isIntegrity(A) => valof{
    Assert = ternary(Lc,"assrt",C,.str(Lc,C::string),.str(Lc,disp(Lc)));
    valis .active(Assert)
  }
  assertMacro(_,_) default => .inactive.

  -- Convert show E to shwMsg(E,"E",Lc)
  showMacro(A,.actn) where (Lc,E) ?= isShow(A) => valof{
    Shw = ternary(Lc,"shwMsg",E,.str(Lc,E::string),.str(Lc,disp(Lc)));
    valis .active(Shw)
  }
  showMacro(_,.actn) default => .inactive.

  -- Convert trace E to traceCall("value of "E" is ",.true,E)
  traceMacro(T,.expression) where (Lc,Grd,Exp) ?= isTrace(T) => valof{
    Tr = ternary(Lc,"traceCall",.str(Lc,"#(showLocn(Lc))\: #(Exp::string)"),Grd,Exp);
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

  -- Convert P?=E to some(P).=E
  optionMatchMacro(A,.expression) where (Lc,L,R) ?= isOptionMatch(A) =>
    .active(mkMatch(Lc,mkOption(Lc,L),R)).
  optionMatchMacro(_,_) default => .inactive.

  mkLoc(Lc) where .locn(P,Line,Col,Off,Ln)?=Lc =>
    mkCon(Lc,"locn",
      [.str(Lc,P),.int(Lc,Line),.int(Lc,Col),.int(Lc,Off),.int(Lc,Ln)]).

  -- Handle __loc__ macro symbol
  locationMacro(A,.expression) where
      (Lc,"__loc__") ?= isName(A) =>
    .active(mkLoc(Lc)).
  
  -- Handle occurrences of __pkg__
  pkgNameMacro(A,.expression) where
      (Lc,"__pkg__") ?= isName(A) && .locn(Pkg,_,_,_,_)?=Lc =>
    .active(.str(Lc,Pkg)).

  -- Convert unary minus to a call to __minus
  uMinusMacro(A,_) where (Lc,R) ?= isUnary(A,"-") =>
    (.int(_,Ix) .= R ??
	.active(.int(Lc,-Ix)) ||
	.num(_,Dx) .= R ??
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
  genHedTest(Lc,H,T) => mkWherePtn(Lc,.tpl(Lc,"()",[H,T]),.nme(Lc,"_hdtl")).

  squareSequenceMacro(A,.expression) where
      (Lc,Els) ?= isSqTuple(A) =>
    .active(macroListEntries(Lc,Els,genNil,genCons)).
  squareSequenceMacro(_,_) => .inactive.

  genNil(Lc) => .nme(Lc,"_nil").
  genCons(Lc,H,T) => binary(Lc,"_cons",H,T).

  mapLiteralMacro(A,.expression) where
      (Lc,Els) ?= isMapLiteral(A) => .active(mkTypeAnnotation(Lc,macroListEntries(Lc,Els,genEmpty,genPut),squareApply(Lc,"map",[mkAnon(Lc),mkAnon(Lc)]))).
  mapLiteralMacro(_,_) default => .inactive.

  genEmpty(Lc) => .nme(Lc,"_empty").
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
	  genResult(mkOption(Lc,Bnd)),
	  .grounded(enum(Lc,"none")));
	valis .active(CC)
      }.
  iotaComprehensionMacro(_,_) default => .inactive.

  -- Convert { F <* El <* Zr | Cond }
  totalizerMacro(A,.expression) where
      (Lc,Fn,El,Zr,Body) ?= isTotalizerComprehension(A) =>
    .active(makeCondition(Body,passThru,genFold(Lc,Fn,El),.grounded(Zr))).
  totalizerMacro(_,_) default => .inactive.

  genFold(Lc,Fn,El) => let{
    chk(.grounded(X))=> roundTerm(Lc,Fn,[El,X]).
    chk(.lyfted(St)) => St.
  } in chk.

  -- Convert {? Cond ?}
  testComprehensionMacro(A,.expression) where
      (Lc,Cond) ?= isTestComprehension(A) => valof{
	CC = makeCondition(Cond,passThru,
	  genResult(enum(Lc,"true")),
	  .grounded(enum(Lc,"false")));
	valis .active(CC)
      }
  testComprehensionMacro(_,_) => .inactive.
	
  makeComprehension:(option[locn],ast,ast) => ast.
  makeComprehension(Lc,Bnd,Cond) => 
    makeCondition(Cond,passThru,(St)=>push(Lc,Bnd,St),null(Lc)).

  -- Condition processing

  lyfted[a] ::= .lyfted(a) | .grounded(a).

  implementation all e ~~ display[e] |= display[lyfted[e]] => {
    disp(.lyfted(X)) => "lifted $(X)".
    disp(.grounded(X)) => "grounded $(X)"
  }

  passThru(.grounded(X)) => X.
  passThru(.lyfted(X)) => X.

  push(Lc,Bnd,.grounded(St)) => binary(Lc,"_push",Bnd,St).
  push(_,_,.lyfted(St)) => St.

  null(Lc) => .grounded(.nme(Lc,"_null")).

  genResult(T) => let{
    chk(.grounded(_))=> T.
    chk(.lyfted(St)) => St.
  } in chk.	

  /*
  * Ptn in Src
  * becomes
  * (El,St) => (Ptn.=El ?? Succ(X,St) || Lift(St)
  * where Succ, Lift & Zed are parameters to the conversion
  */
  makeCondition:(ast,(lyfted[ast])=>ast,(lyfted[ast])=>ast,lyfted[ast]) => ast.
  makeCondition(A,Lift,Succ,Zed) where (Lc,Ptn,Src) ?= isSearch(A) => valof{
    El = genName(Lc,"El");
    St = genName(Lc,"St");

    Lam = mkLambda(Lc,.true,rndTuple(Lc,[El,St]),.none,
      mkConditional(Lc,mkMatch(Lc,Ptn,El),Succ(.grounded(St)),Lift(.grounded(St))));
    valis ternary(Lc,"_iter",Src,Lift(Zed),Lam)
  }
  makeCondition(A,Lift,Succ,Zed) where (Lc,L,R) ?= isConjunct(A) => 
    makeCondition(L,Lift,(Lf) => makeCondition(R,Lift,Succ,Lf),Zed).
  makeCondition(A,Lift,Succ,Zed) where (Lc,L,R) ?= isDisjunct(A) => valof{
    E1 =makeCondition(L,Lift,Succ,Zed);
    valis makeCondition(R,Lift,Succ,.lyfted(E1))
  }
  makeCondition(A,Lift,Succ,Zed) where (Lc,R) ?= isNegation(A) => valof{
    Negated = makeCondition(R,Lift,genResult(enum(Lc,"true")),
      .grounded(enum(Lc,"false")));
    St = genName(Lc,"St");
    SuccCase = Succ(Zed);
    FalseCase = Lift(Zed);
    valis mkConditional(Lc,Negated,FalseCase,SuccCase)
  }
  makeCondition(A,Lift,Succ,Zed) where (Lc,L,R) ?= isImplies(A) =>
    makeCondition(negated(Lc,mkConjunct(Lc,L,negated(Lc,R))),Lift,Succ,Zed).
  makeCondition(A,Lift,Succ,Zed) where (_,[I]) ?= isTuple(A) =>
    makeCondition(I,Lift,Succ,Zed).
  makeCondition(Other,Lift,Succ,Zed) =>
    mkConditional(locOf(Other),Other,Succ(Zed),Lift(Zed)).

  -- Convert E::T to _optval(_coerce(E)):T
  -- Convert E:?T to _coerce(E):option[T]

  coercionMacro(A,.expression) where (Lc,L,R) ?= isCoerce(A) =>
    .active(mkTypeAnnotation(Lc,unary(Lc,"_optval",unary(Lc,"_coerce",L)),R)).
  coercionMacro(A,.expression) where (Lc,L,R) ?= isOptCoerce(A) =>
    .active(mkTypeAnnotation(Lc,unary(Lc,"_coerce",L),sqUnary(Lc,"option",R))).
  coercionMacro(_,_) => .inactive.

  indexMacro(A,.expression) where (Lc,L,R) ?= isIndexTerm(A) =>
    ((_,Ky,Vl) ?= isBinary(R,"->") ??
	.active(ternary(Lc,"_put",L,Ky,Vl)) ||
	(_,Ky) ?= isNegation(R) ??
	  .active(binary(Lc,"_remove",L,Ky)) ||
	  .active(binary(Lc,"_index",L,R))).
  indexMacro(_,_) default => .inactive.

  -- C[I]:=R becomes C:=_put(C!,I,R)

  indexAssignMacro(A,.actn) where
      (Lc,L,R) ?= isAssignment(A) &&
      (LLc,S,I) ?= isIndexTerm(L) =>
    .active(mkAssignment(Lc,S,
	  ternary(Lc,"_put",refCell(Lc,S),I,R))).
  indexAssignMacro(_,_) default => .inactive.
  
  sliceMacro(A,.expression) where (Lc,Op,F,T) ?= isSlice(A) =>
    .active(ternary(Lc,"_slice",Op,F,T)).
  sliceMacro(_,_) default => .inactive.

  spliceAssignMacro(A,.expression) where
      (Lc,S,F,T,R) ?= isSplice(A) =>
    .active(mkAssignment(Lc,S,
	  roundTerm(Lc,.nme(Lc,"_splice"),
	    [refCell(Lc,S),F,T,R]))).
  spliceAssignMacro(_,_) default => .inactive.
  
  multicatMacro(A,.expression) where (Lc,E) ?= isUnary(A,"*") =>
    .active(unary(Lc,"_multicat",E)).
  multicatMacro(_,.expression) => .inactive.

  -- Convert <| E |> to a quote expression
  -- which relies on the quote contract
  quoteMacro(A,.expression) where (Lc,Trm) ?= isQuoted(A) =>
    .active(quoteAst(A)).
  quoteMacro(_,_) => .inactive.

  -- quoteAst creates an ast that checks to a quoted form of the ast itself
  quoteAst(E) where (_,X) ?= isUnary(E,"$") => X.
  quoteAst(.nme(Lc,Nm)) => binary(Lc,"_name",mkLoc(Lc),.str(Lc,Nm)).
  quoteAst(.qnm(Lc,Nm)) => binary(Lc,"_qnme",mkLoc(Lc),.str(Lc,Nm)).
  quoteAst(.int(Lc,Nx)) => binary(Lc,"_integer",mkLoc(Lc),.int(Lc,Nx)).
  quoteAst(.big(Lc,Bx)) => binary(Lc,"_biginteger",mkLoc(Lc),.big(Lc,Bx)).
  quoteAst(.num(Lc,Nx)) => binary(Lc,"_float",mkLoc(Lc),.num(Lc,Nx)).
  quoteAst(.chr(Lc,Cx)) => binary(Lc,"_char",mkLoc(Lc),.chr(Lc,Cx)).
  quoteAst(.str(Lc,Sx)) => binary(Lc,"_string",mkLoc(Lc),.str(Lc,Sx)).
  quoteAst(.tpl(Lc,Nm,Els)) => ternary(Lc,"_tuple",mkLoc(Lc),.str(Lc,Nm),
      macroListEntries(Lc,Els//quoteAst,genNil,genCons)).
  quoteAst(.app(Lc,Op,Arg)) => ternary(Lc,"_apply",mkLoc(Lc),quoteAst(Op),quoteAst(Arg)).

  /*
  for P : G do B
  becomes

  lb{ while .true do{
  case G resume ._next in {
    | _yld(P) => B
    | _yld(_) default => {}
    | ._all => break lb
  }
  }
  }

  */

  forLoopMacro(A,.actn) where (Lc,P,G,B) ?= isForDo(A) => valof{
    Lb = genId("Lb");

    /* Build ._all => break Lb */
    End = mkLambda(Lc,.false,enum(Lc,"_all"),.none,mkBreak(Lc,Lb));

    /* build _yld(P) => B */
    Yld = mkLambda(Lc,.false,mkCon(Lc,"_yld",[P]),.none,B);

    /* build _yld(_) default => {} */
    Deflt = mkLambda(Lc,.true,mkCon(Lc,"_yld",[mkAnon(Lc)]),.none,brTuple(Lc,[]));

    /* build case G resume ._next in .. */
    Resume = mkCaseExp(Lc,mkResume(Lc,G,enum(Lc,"_next")),[Yld,Deflt,End]);

    /* Build while .true loop */
    Loop = mkWhileDo(Lc,enum(Lc,"true"),brTuple(Lc,[Resume]));

  /* Build Lb{ while .true do .. } */
    Lbld = mkLbldAction(Lc,Lb,[Loop]);

    valis .active(Lbld)
  }
  forLoopMacro(_,.actn) default => .inactive.

/*
   Lb..<Up
   becomes
   .range(Lc,Up,one)
*/
  incRangeMacro(T,.expression) where (Lc,Lb,Up) ?= isBinary(T,"..<") =>
    .active(mkCon(Lc,"range",[Lb,Up,.nme(Lc,"one")])).
  incRangeMacro(_,_) default => .inactive.
  
/*
   Lb..>Up
   becomes
   .range(Lb,Up,-one)
*/
  decRangeMacro(T,.expression) where (Lc,Lb,Up) ?= isBinary(T,"..>") =>
    .active(mkCon(Lc,"range",[Lb,Up,unary(Lc,"-",.nme(Lc,"one"))])).
  decRangeMacro(_,_) default => .inactive.

/*
  Lb..<Up:Step
  becomes
  .range(Lb,Up,Step)
  */
  rangeMacro(T,.expression) where
      (Lc,L,Step) ?= isBinary(T,":") &&
	  (_,Lb,Up) ?= isBinary(L,"..<") =>
    .active(mkCon(Lc,"range",[Lb,Up,Step])).
  rangeMacro(_,.expression) => .inactive.
  
  /*
  for P in C do B
  becomes
  I .= _generate(C);
  lb {
    while .true do{
      case I resume ._next in {
	| _yld(P) => B
	| _yld(_) default => {}
	| ._all => break lb
      }
    }
  }
  */

  forInLoopMacro(A,.actn) where (Lc,P,C,B) ?= isForIn(A) => valof{
    I = genName(Lc,"I");
    Lb = genId("Lb");

    /* Build ._all => break Lb */
    End = mkLambda(Lc,.false,enum(Lc,"_all"),.none,mkBreak(Lc,Lb));

    /* build _yld(P) => B */
    Yld = mkLambda(Lc,.false,mkCon(Lc,"_yld",[P]),.none,B);

    /* build _yld(_) default => {} */
    Deflt = mkLambda(Lc,.true,mkCon(Lc,"_yld",[mkAnon(Lc)]),.none,brTuple(Lc,[]));

    /* build case I resume ._next in .. */
    Resume = mkCaseExp(Lc,mkResume(Lc,I,enum(Lc,"_next")),[Yld,Deflt,End]);

    /* Build while .true loop */
    Loop = mkWhileDo(Lc,enum(Lc,"true"),brTuple(Lc,[Resume]));

  /* Build Lb{while .true do .. }*/
    Lbld = mkLbldAction(Lc,Lb,[Loop]);

    /* Build call to _generate */
    IT = roundTerm(Lc,.nme(Lc,"_generate"),[C]);
    S1 = mkDefn(Lc,I,IT);

    valis .active(mkSequence(Lc,S1,Lbld))
  }
  forInLoopMacro(_,.actn) default => .inactive.

  /* Do notation ... converting to monad calls */
  doMacro(E,.expression) where (ELc,As) ?= isDo(E) => let{.
    monadActions([]) => .none.
    monadActions([Act,..Suff]) where (_,Blk) ?= isBrTuple(Act) =>
      monadActions(deSequence(Blk)++Suff).
    monadActions([Act,..Suff]) =>
      .some(monadAction(Act,monadActions(Suff))).

    monadAction(Act,.some(Rest)) where (Lc,Lhs,Rhs) ?= isLift(Act) => valof{
      Lam = mkLambda(Lc,.false,rndTuple(Lc,[Lhs]),.none,Rest);
      valis binary(Lc,">>=",Rhs,Lam)
    }
    monadAction(Act,.none) where (Lc,Lhs,Rhs) ?= isLift(Act) => valof{
      reportError("$(Act) may not be last action",Lc);
      valis Rhs
    }
    monadAction(Act,.some(Rest)) where (Lc,Lhs,Rhs) ?= isDefn(Act) => valof{
      Lam = mkLambda(Lc,.false,rndTuple(Lc,[Lhs]),.none,Rest);
      valis roundTerm(Lc,Lam,[Rhs]).
    }
    monadAction(Act,.none) where (Lc,_,_) ?= isDefn(Act) => valof{
      reportError("$(Act) may not be last action",Lc);
      valis Act
    }
    monadAction(Act,Rest) where (Lc,Rhs) ?= isValis(Act) =>
      combine(unary(Lc,"return",Rhs),Lc,Rest).
    monadAction(Act,Rest) where (Lc,_,_) ?= isRoundTerm(Act) =>
      combine(Act,Lc,Rest).
    monadAction(Act,Rest) where (Lc,C,Th,El) ?= isIfThenElse(Act) => valof{
      A1 = _optval(monadActions([Th]));
      A2 = _optval(monadActions([El]));
      valis combine(mkConditional(Lc,C,A1,A2),Lc,Rest).
    }

    combine(Act,_,.none) => Act.
    combine(Act,Lc,.some(Rest)) =>
      binary(Lc,">>=",Act,mkLambda(Lc,.false,rndTuple(Lc,[mkAnon(Lc)]),.none,Rest)).
  .} in .active(_optval(monadActions(As))).
  doMacro(_,.expression) default => .inactive.

  /* valof E where E is not an explicit action block ... */
  valofMacro(E,.expression) where (Lc,A) ?= isUnary(E,"valof") &&
      ~ _?= isBrTuple(A) =>
    .active(unary(Lc,"_valof",A)).
  valofMacro(_,_) default => .inactive.

  /* generator{A}
  becomes
  _fiber((this,_)=>valof{
      A*;
      valis .all
    })
  */
   
  generatorMacro(E,.expression) where
      (Lc,A) ?= isGenerator(E) => valof{
    All = mkValis(Lc,enum(Lc,"_all"));
	valis .active(unary(Lc,"_fiber",
	    mkLambda(Lc,.false,
	      rndTuple(Lc,[.nme(Lc,"this"),.nme(Lc,"_")]),
	      .none,
	  mkValof(Lc,[A,All]))))
      }.
  generatorMacro(_,_) default => .inactive.

  /* map generator[t] to 'generator'[t] */
  generatorTypeMacro(E,.typeterm) where
      (Lc,.nme(_,"generator"),[Y]) ?= isSquareTerm(E) =>
    .active(mkGeneratorType(Lc,Y)).
  generatorTypeMacro(_,_) default => .inactive.

  /* yield E
  becomes
  case this suspend ._yld(E) in {
    | ._next => {}
    | ._cancel => retire._all
  }
  */
  yieldMacro(A,.actn) where (Lc,E) ?= isUnary(A,"yield") => valof{
    This = .nme(Lc,"this");
    /* build ._next => {} */
    Nxt = mkLambda(Lc,.false,enum(Lc,"_next"),.none,brTuple(Lc,[]));

    /* build ._cancel => retire ._all */
    Cancel = mkLambda(Lc,.false,enum(Lc,"_cancel"),.none,mkRetire(Lc,This,enum(Lc,"_all")));

    /* Build suspend */
    valis .active(mkCaseExp(Lc,mkSuspend(Lc,This,mkCon(Lc,"_yld",[E])),[Nxt,Cancel]))
  }

  /* task { A }

  becomes a task:

  tsk(this, let{
      tk:async () => _ throws _.
      tk() => valof { A }
      } in ζ tk)

  where tsk is a library function defined in mbox.
  */

  taskMacro(E,.expression) where (Lc,A) ?= isTask(E) => valof{
    Tk = genName(Lc,"tk");
    Anon = .nme(Lc,"_");
    Empty = rndTuple(Lc,[]);
    -- Build type annotation:
    -- tk:async () => _ throws _.
    TkTp = mkTypeDeclaration(Lc,Tk,mkAsync(Lc,mkThrowingFunType(Lc,Empty,Anon,Anon)));

    -- Build function:
    -- tk() => valof { A }
    TkDf = mkEquation(Lc,.some(Tk),.true,Empty,.none,mkValof(Lc,[A]));

    -- Build let defn
    LetFn = mkLetDef(Lc,[TkTp,TkDf],mkSuppress(Lc,Tk));

    -- Build call to tsk
    Call = roundTerm(Lc,.nme(Lc,"tsk"),[.nme(Lc,"this"),LetFn]);
    if macroTracing! then
      showMsg("task: $(Call)");
    valis .active(Call)
  }
  taskMacro(_,_) default => .inactive.


  /* collect { A }

  becomes

    valof{
     $result := _null;
     A*;
     valis $result
     }
  */

  collectMacro(E,.expression) where (Lc,Acts) ?= isCollect(E) => let{.
    Res = genName(Lc,"reslt").

    replaceElemis(A) where (Elc,Vl) ?= isElemis(A) =>
      .active(mkAssignment(Elc,Res,binary(Elc,"_push",Vl,refCell(Elc,Res)))).
    replaceElemis(_) default => .inactive.
  .} in .active(mkValof(Lc,[
	mkAssignment(Lc,Res,.nme(Lc,"_null"))]++
	foldOverAction(Acts,replaceElemis)++
      [mkValis(Lc,refCell(Lc,Res))])).

  implementationMacro(A,.statement) where
      (Lc,Q,C,H,E) ?= isImplementationStmt(A) &&
      (_,Nm,_) ?= isSquareTerm(H) &&
      Ex ?= labelImplExp(E,Nm) => 
    .active(mkImplementationStmt(Lc,Q,C,H,Ex)).
  implementationMacro(_,_) default => .inactive.

  labelImplExp(T,Nm) where (Lc,Els) ?= isBrTuple(T) =>
    .some(braceTerm(Lc,dollarName(Nm),Els)).
  labelImplExp(T,Nm) where (Lc,Els) ?= isQBrTuple(T) =>
    .some(qbraceTerm(Lc,dollarName(Nm),Els)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ?= isLetDef(T) &&
    EE ?= labelImplExp(Exp,Nm) =>
    .some(mkLetDef(Lc,Els,EE)).
  labelImplExp(T,Nm) where (Lc,Els,Exp) ?= isLetRecDef(T) &&
      EE ?= labelImplExp(Exp,Nm) =>
    .some(mkLetRecDef(Lc,Els,EE)).
  labelImplExp(T,_) default => .none.

  arrowMacro(E,_) where (Lc,Lhs,Rhs) ?= isBinary(E,"->") =>
    .active(mkCon(Lc,"kv",[Lhs,Rhs])).
  arrowMacro(_,_) default => .inactive.

  caseRuleMacro(A,_) where
      (Lc,P,R) ?= isBinary(A,"|:") && (LLc,T,E) ?= isBinary(R,"=>") =>
    .active(binary(Lc,"=>",binary(Lc,"|:",P,T),E)).
  caseRuleMacro(A,_) default => .inactive.

  -- Convert async T to (this:task[_]) |= T.
  asyncMacro(A,.typeterm) where
      (Lc,R) ?= isAsync(A) =>
    .active(reConstrain([mkTypeAnnotation(Lc,.nme(Lc,"this"),
	  squareTerm(Lc,.nme(Lc,"task"),[.nme(Lc,"_")]))],
      R)).
  asyncMacro(_,_) default => .inactive.
}
