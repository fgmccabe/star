rdf.sparql.expreval.test{
  import star.
  import star.assert.
  import rdf.triple.
  import rdf.sparql.query.
  import rdf.sparql.solution.
  import rdf.sparql.expreval.

  /* Tests for rdf.sparql.expreval: the operator set (logic/comparison/arithmetic with
     int/float promotion), EBV, EXISTS/NOT EXISTS, and the implemented builtin subset. No
     graph or engine.star involved -- EXISTS/NOT EXISTS use a stub pattern evaluator instead
     of a real graph, since expreval.star only needs *some* (pattern)=>solutions callback,
     not rdf.sparql.engine specifically (see expreval.star's header comment on why). */

  tm:(term) => expression.
  tm(T) => .term(T).

  lit:(concept) => expression.
  lit(C) => .term(.literal(C)).

  vr:(string) => expression.
  vr(V) => .term(.var(V)).

  -- Stub pattern evaluators for EXISTS/NOT EXISTS -- one that "finds" a match compatible
  -- with anything, one that never matches.
  alwaysMatches:(pattern) => solutions throws string.
  alwaysMatches(_) => [emptyMapping].

  neverMatches:(pattern) => solutions throws string.
  neverMatches(_) => [].

  main:(){}.
  main(){
    Failures = ref 0;

    M0 = emptyMapping.
    Mx = emptyMapping["x"->.int(5)].

    -- .term / .bound
    checkEq(Failures,evalOk(neverMatches,M0,lit(.int(1))),.int(1),".term(.literal) evaluates to the literal itself");
    checkEq(Failures,evalOk(neverMatches,Mx,vr("x")),.int(5),".term(.var) looks the variable up in the mapping");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,vr("x")),"an unbound variable is an evaluation error");
    checkEq(Failures,evalOk(neverMatches,Mx,.bound("x")),.bool(.true),"BOUND(?x) is true when ?x is bound");
    checkEq(Failures,evalOk(neverMatches,M0,.bound("x")),.bool(.false),"BOUND(?x) is false when ?x is unbound");

    -- EBV
    checkEq(Failures,evalOk(neverMatches,M0,lit(.bool(.true))),.bool(.true),"ebv passes booleans through .not's double-negation check below");
    checkEq(Failures,evalOk(neverMatches,M0,.not(lit(.int(0)))),.bool(.true),"NOT(0) is true -- 0's EBV is false");
    checkEq(Failures,evalOk(neverMatches,M0,.not(lit(.text([.str("")])))),.bool(.true),"NOT(\"\") is true -- empty string's EBV is false");
    checkEq(Failures,evalOk(neverMatches,M0,.not(lit(.text([.str("x")])))),.bool(.false),"NOT(\"x\") is false -- non-empty string's EBV is true");

    -- && / || truth table, including error-masking
    checkEq(Failures,evalOk(neverMatches,M0,.and(lit(.bool(.true)),lit(.bool(.true)))),.bool(.true),"true && true");
    checkEq(Failures,evalOk(neverMatches,M0,.and(lit(.bool(.false)),vr("undefined"))),.bool(.false),
      "false && error masks the error -- result is false");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,.and(lit(.bool(.true)),vr("undefined"))),
      "true && error propagates the error");
    checkEq(Failures,evalOk(neverMatches,M0,.or(lit(.bool(.true)),vr("undefined"))),.bool(.true),
      "true || error masks the error -- result is true");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,.or(lit(.bool(.false)),vr("undefined"))),
      "false || error propagates the error");

    -- = / != with int/float cross-type promotion
    checkEq(Failures,evalOk(neverMatches,M0,.eq(lit(.int(1)),lit(.flt(1.0)))),.bool(.true),"1 = 1.0 (numeric cross-type equality)");
    checkEq(Failures,evalOk(neverMatches,M0,.ne(lit(.int(1)),lit(.int(2)))),.bool(.true),"1 != 2");
    checkEq(Failures,evalOk(neverMatches,M0,.eq(lit(.uri("a")),lit(.uri("a")))),.bool(.true),"same URI is equal to itself");

    -- < > <= >= with mixed int/float
    checkEq(Failures,evalOk(neverMatches,M0,.lt(lit(.int(1)),lit(.flt(1.5)))),.bool(.true),"1 < 1.5");
    checkEq(Failures,evalOk(neverMatches,M0,.gt(lit(.flt(2.5)),lit(.int(2)))),.bool(.true),"2.5 > 2");
    checkEq(Failures,evalOk(neverMatches,M0,.le(lit(.int(2)),lit(.int(2)))),.bool(.true),"2 <= 2");
    checkEq(Failures,evalOk(neverMatches,M0,.ge(lit(.int(2)),lit(.int(3)))),.bool(.false),"2 >= 3 is false");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,.lt(lit(.text([.str("a")])),lit(.int(1)))),
      "comparing a non-numeric operand is an error");

    -- IN / NOT IN
    checkEq(Failures,evalOk(neverMatches,M0,.isIn(lit(.int(2)),[lit(.int(1)),lit(.int(2)),lit(.int(3))])),.bool(.true),
      "2 IN (1,2,3)");
    checkEq(Failures,evalOk(neverMatches,M0,.notIn(lit(.int(5)),[lit(.int(1)),lit(.int(2))])),.bool(.true),
      "5 NOT IN (1,2)");

    -- arithmetic with int/float promotion
    checkEq(Failures,evalOk(neverMatches,M0,.add(lit(.int(2)),lit(.int(3)))),.int(5),"2 + 3 stays integer");
    checkEq(Failures,evalOk(neverMatches,M0,.add(lit(.int(2)),lit(.flt(0.5)))),.flt(2.5),"2 + 0.5 promotes to float");
    checkEq(Failures,evalOk(neverMatches,M0,.sub(lit(.int(5)),lit(.int(3)))),.int(2),"5 - 3");
    checkEq(Failures,evalOk(neverMatches,M0,.mul(lit(.int(4)),lit(.flt(1.5)))),.flt(6.0),"4 * 1.5 promotes to float");
    checkEq(Failures,evalOk(neverMatches,M0,.div(lit(.int(6)),lit(.int(4)))),.flt(1.5),
      "6 / 4 always yields a float, not truncated integer division");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,.div(lit(.int(1)),lit(.int(0)))),"1 / 0 is a division error");
    checkEq(Failures,evalOk(neverMatches,M0,.neg(lit(.int(3)))),.int(-3),"unary -3");
    checkEq(Failures,evalOk(neverMatches,M0,.pos(lit(.flt(3.5)))),.flt(3.5),"unary +3.5");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,.add(lit(.text([.str("x")])),lit(.int(1)))),
      "adding a non-numeric operand is an error");

    -- EXISTS / NOT EXISTS
    checkEq(Failures,evalOk(alwaysMatches,M0,.existsPattern(.nilPattern)),.bool(.true),
      "EXISTS is true when the (stubbed) pattern evaluator finds a compatible match");
    checkEq(Failures,evalOk(neverMatches,M0,.existsPattern(.nilPattern)),.bool(.false),
      "EXISTS is false when nothing matches");
    checkEq(Failures,evalOk(neverMatches,M0,.notExists(.nilPattern)),.bool(.true),
      "NOT EXISTS is true when nothing matches");

    -- builtins
    checkEq(Failures,evalOk(neverMatches,M0,.call("str",[lit(.int(42))])),.text([.str("42")]),"STR(42)");
    checkEq(Failures,evalOk(neverMatches,M0,.call("lang",[lit(.langText([.str("hi")],"en"))])),.text([.str("en")]),
      "LANG(\"hi\"@en)");
    checkEq(Failures,evalOk(neverMatches,M0,.call("isiri",[lit(.uri("http://example.org/"))])),.bool(.true),"isIRI(<uri>)");
    checkEq(Failures,evalOk(neverMatches,M0,.call("isiri",[lit(.int(1))])),.bool(.false),"isIRI(1) is false");
    checkEq(Failures,evalOk(neverMatches,M0,.call("isnumeric",[lit(.int(1))])),.bool(.true),"isNumeric(1)");
    checkEq(Failures,evalOk(neverMatches,M0,.call("isliteral",[lit(.text([.str("x")]))])),.bool(.true),"isLiteral(\"x\")");
    checkEq(Failures,evalOk(neverMatches,M0,.call("abs",[lit(.int(-7))])),.int(7),"ABS(-7)");
    checkEq(Failures,evalOk(neverMatches,M0,.call("strlen",[lit(.text([.str("hello")]))])),.int(5),"STRLEN(\"hello\")");
    checkEq(Failures,evalOk(neverMatches,M0,.call("contains",[lit(.text([.str("hello")])),lit(.text([.str("ell")]))])),
      .bool(.true),"CONTAINS(\"hello\",\"ell\")");
    checkEq(Failures,evalOk(neverMatches,M0,.call("strstarts",[lit(.text([.str("hello")])),lit(.text([.str("he")]))])),
      .bool(.true),"STRSTARTS(\"hello\",\"he\")");
    checkEq(Failures,evalOk(neverMatches,M0,.call("strends",[lit(.text([.str("hello")])),lit(.text([.str("lo")]))])),
      .bool(.true),"STRENDS(\"hello\",\"lo\")");
    checkEq(Failures,evalOk(neverMatches,M0,.call("sameterm",[lit(.int(1)),lit(.int(1))])),.bool(.true),"sameTerm(1,1)");
    checkEq(Failures,evalOk(neverMatches,M0,.call("sameterm",[lit(.int(1)),lit(.flt(1.0))])),.bool(.false),
      "sameTerm(1,1.0) is false -- sameTerm is not numeric-equality, unlike =");
    checkEq(Failures,evalOk(neverMatches,M0,.call("concat",[lit(.text([.str("foo")])),lit(.text([.str("bar")]))])),
      .text([.str("foobar")]),"CONCAT(\"foo\",\"bar\")");
    checkEq(Failures,evalOk(neverMatches,M0,.call("coalesce",[vr("undefined"),lit(.int(9))])),.int(9),
      "COALESCE skips the erroring first argument and returns the second");
    checkEq(Failures,evalOk(neverMatches,M0,.call("if",[lit(.bool(.true)),lit(.int(1)),lit(.int(2))])),.int(1),
      "IF(true,1,2)");
    checkThrows(Failures,() => evalExpr(neverMatches,M0,.call("regex",[lit(.text([.str("x")])),lit(.text([.str("x")]))])),
      "an unimplemented builtin (REGEX) raises a clear error, not a silent wrong answer");

    if Failures! == 0 then{
      logMsg(.info,"all expreval tests passed")
    } else{
      logMsg(.severe,"$(Failures!) expreval test(s) failed")
    };
    assert(Failures! == 0)
  }

  -- evalExpr is declared "throws string" -- wrap "expected to succeed" calls the same way
  -- enginetest.star wraps evalPattern, for the same reason.
  evalOk:((pattern)=>solutions throws string,mapping,expression) => concept.
  evalOk(EvalP,M,E) => valof{
    try{
      valis evalExpr(EvalP,M,E)
    } catch {
      _ do valis unexpectedThrow()
    }
  }

  unexpectedThrow:() => concept.
  unexpectedThrow() => valof{
    assert(.false);
    valis .bool(.false)
  }

  checkEq(Failures,Got,Expect,Descr) => valof{
    if Got==Expect then{
      logMsg(.info,"PASS: $(Descr)");
      valis ()
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL ($(Descr)): expected $(Expect), got $(Got)");
      valis ()
    }
  }

  checkThrows(Failures,Th,Descr) => valof{
    try{
      Th();
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL ($(Descr)): expected an error, but it succeeded");
      valis ()
    } catch {
      _ do {
        logMsg(.info,"PASS: $(Descr)");
        valis ()
      }
    }
  }
}
