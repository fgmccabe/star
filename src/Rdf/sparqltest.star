rdf.sparql.test{
  import star.
  import star.location.
  import rdf.lexer.
  import rdf.sparql.parser.
  import rdf.sparql.query.
  import rdf.triple.

  -- Smoke test for the SPARQL parser (rdf.sparql.parser): each query is
  -- tokenized and run through queryUnit, and the result is checked against
  -- the expected accept/reject outcome. Covers every query form, solution
  -- modifiers, property paths, aggregates, UNION/OPTIONAL, RDF-star
  -- (reified triples, annotations, reifiers), and expression precedence.

  public _main:(cons[string]) => integer.
  _main(_) => valof{
    Failures = ref 0;

    tryQuery(Failures,"SELECT ?s ?p ?o WHERE { ?s ?p ?o . }", .true);
    tryQuery(Failures,"SELECT DISTINCT ?name WHERE { ?person foaf:name ?name . FILTER(?age > 21) }", .true);
    tryQuery(Failures,"ASK { ?s a ?type }", .true);
    tryQuery(Failures,"SELECT ?s WHERE { { ?s ex:p ?o } UNION { ?s ex:r ?o } }", .true);
    tryQuery(Failures,"SELECT ?s WHERE { ?s ex:p ?o . OPTIONAL { ?o ex:q ?r } }", .true);
    tryQuery(Failures,"SELECT ?s WHERE { ?s (ex:p/ex:q)+ ?o }", .true);
    tryQuery(Failures,"SELECT (COUNT(DISTINCT ?s) AS ?c) WHERE { ?s ?p ?o } GROUP BY ?p HAVING (?c > 1)", .true);
    tryQuery(Failures,"SELECT ?s WHERE { << ?s ?p ?o >> ex:certainty 0.9 }", .true);
    tryQuery(Failures,"SELECT ?s WHERE { ?s ex:p ?o {| ex:meta ex:val |} }", .true);
    tryQuery(Failures,"SELECT ?s WHERE { ?s ex:p ?o ~ ex:someid }", .true);
    tryQuery(Failures,"SELECT ?s WHERE { ?s ?p ?o . FILTER(?o = 1 && (2+3)*4 -5 > 0 || !BOUND(?s)) }", .true);
    tryQuery(Failures,"CONSTRUCT { ?s ex:derived ?o } WHERE { ?s ex:p ?o }", .true);
    tryQuery(Failures,"DESCRIBE ?s WHERE { ?s a ex:Thing }", .true);
    tryQuery(Failures,"PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s a ex:Thing } ORDER BY DESC(?s) LIMIT 10 OFFSET 2", .true);
    tryQuery(Failures,"SELECT ?s WHERE { VALUES ?s { ex:a ex:b } ?s ?p ?o }", .true);
    tryQuery(Failures,"THIS IS NOT VALID SPARQL AT ALL", .false);
    tryQuery(Failures,"SELECT ?s WHERE { ?s ?p ?o UNION { ?s ex:r ?o } }", .false);

    -- AST-shape checks: recognition alone can't catch a wrong operator
    -- precedence fold, the OPTIONAL/MINUS accumulator combining with the
    -- wrong side, or CONSTRUCT WHERE's template/where-pattern sharing --
    -- these inspect the actual query.query value each parse produces.
    tryAst(Failures,"SELECT ?s WHERE { ?s ?p ?o }", checkSimpleSelect,
      "basic triple pattern with plain projection");
    tryAst(Failures,"SELECT ?s WHERE { ?s ex:p ?o . FILTER(2+3*4) }", checkPrecedence,
      "additive/multiplicative precedence (2+3*4, not (2+3)*4)");
    tryAst(Failures,"SELECT ?s WHERE { ?s ex:p ?o . FILTER(2 -3*4) }", checkSignedFactor,
      "spaced minus binds as subtraction, not a signed literal (2 -3*4 = 2 - (3*4))");
    tryAst(Failures,"SELECT ?s WHERE { ?s ex:p ?o . OPTIONAL { ?o ex:q ?r } }", checkOptionalAccum,
      "OPTIONAL takes the accumulated left pattern, not an empty one");
    tryAst(Failures,"SELECT ?s WHERE { ?s ex:p ?o . MINUS { ?o ex:q ?r } }", checkMinusAccum,
      "MINUS takes the accumulated left pattern, not an empty one");
    tryAst(Failures,"SELECT (COUNT(DISTINCT ?s) AS ?c) WHERE { ?s ?p ?o }", checkCountDistinct,
      "COUNT(DISTINCT ?s) AS ?c aggregate with distinct flag set");
    tryAst(Failures,"CONSTRUCT WHERE { ?s ?p ?o }", checkConstructWhereShorthand,
      "CONSTRUCT WHERE shorthand reuses one pattern as both template and where-clause");
    tryAst(Failures,"SELECT ?s WHERE { ?s ?p ?o } VALUES ?s { ex:a }", checkTopValues,
      "top-level trailing VALUES folds into the WHERE pattern");
    tryAst(Failures,"SELECT ?s WHERE { { ?s ex:p ?o } UNION { ?s ex:r ?o } }", checkUnion,
      "UNION combines both branches directly, no external left operand");
    tryAst(Failures,"SELECT (GROUP_CONCAT(?n; SEPARATOR=\",\") AS ?names) WHERE { ?s ex:n ?n }", checkGroupConcatSep,
      "GROUP_CONCAT's SEPARATOR string is captured");

    if Failures! == 0 then{
      logMsg(.info,"all sparql parser tests passed");
      valis 0
    } else{
      logMsg(.severe,"$(Failures!) sparql parser test(s) failed");
      valis 1
    }
  }

  tryQuery(Failures,Q,Expect) => valof{
    Toks = allTokens(.locn("t",1,1,0,0),Q::cons[char]);
    Got = (Rslt ?= (queryUnit --> Toks) ?? .true || .false);
    if Got==Expect then{
      logMsg(.info,"PASS ($(Got)): $(Q)");
      valis ()
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"MISMATCH (expected $(Expect), got $(Got)): $(Q)");
      valis ()
    }
  }

  tryAst(Failures,Q,Checker,Descr) => valof{
    Toks = allTokens(.locn("t",1,1,0,0),Q::cons[char]);
    if QAst ?= (queryUnit --> Toks) then{
      if Checker(QAst) then{
        logMsg(.info,"PASS (ast): $(Descr)");
        valis ()
      } else{
        Failures := Failures! + 1;
        logMsg(.severe,"AST MISMATCH ($(Descr)): $(Q) -- $(QAst)");
        valis ()
      }
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"PARSE FAILED ($(Descr)): $(Q)");
      valis ()
    }
  }

  checkSimpleSelect(.select(.noModifier,.vars([.plain("s")]),[],
    .basic(.var("s"),.simple(.var("p")),.var("o")),_)) => .true.
  checkSimpleSelect(_) default => .false.

  checkPrecedence(.select(_,_,_,.conj(.basic(_,_,_),
    .filter(.add(.term(.literal(.int(2))),.mul(.term(.literal(.int(3))),.term(.literal(.int(4))))))),_)) => .true.
  checkPrecedence(_) default => .false.

  -- additiveOp's clauses are tried in the same priority order as the real
  -- SPARQL BNF ('+', '-', then the signed-literal case) -- so "- 3 * 4" always
  -- matches the plain punc("-"),multiplicativeExpression alternative first
  -- (it parses "3*4" successfully as a multiplicativeExpression), and the
  -- signed-literal alternative never gets a chance. This matches real SPARQL
  -- too: without adjacency info (rdf.lexer doesn't track whether '-' touches
  -- the digit -- see numericLiteralNegative's comment), "2 -3*4" is
  -- indistinguishable from ordinary subtraction, which is what it parses as.
  checkSignedFactor(.select(_,_,_,.conj(.basic(_,_,_),
    .filter(.sub(.term(.literal(.int(2))),.mul(.term(.literal(.int(3))),.term(.literal(.int(4))))))),_)) => .true.
  checkSignedFactor(_) default => .false.

  -- ex:p/ex:q are IRI-valued predicates in a WHERE-clause (property-path)
  -- context, so they parse through verbPath as .path(.predicate(...)), not
  -- the plain .simple(...) that CONSTRUCT templates' verb produces (see
  -- checkConstructWhereShorthand below, and section 3's verb/verbPath split).
  checkOptionalAccum(.select(_,_,_,.optional(.basic(.var("s"),.path(.predicate(.literal(_))),.var("o")),
    .basic(.var("o"),.path(.predicate(.literal(_))),.var("r"))),_)) => .true.
  checkOptionalAccum(_) default => .false.

  checkMinusAccum(.select(_,_,_,.minus(.basic(.var("s"),.path(.predicate(.literal(_))),.var("o")),
    .basic(.var("o"),.path(.predicate(.literal(_))),.var("r"))),_)) => .true.
  checkMinusAccum(_) default => .false.

  checkCountDistinct(.select(_,.vars([.computed(.aggregate("count",.some(.term(.var("s"))),.true),"c")]),_,_,_)) => .true.
  checkCountDistinct(_) default => .false.

  -- Checks the template and where-pattern positions really did come from the
  -- same parse (see constructBody's second clause) by comparing the
  -- variable names on both sides -- pattern/term have no equality contract,
  -- so this compares the string names directly rather than the patterns
  -- themselves.
  checkConstructWhereShorthand(.construct(
    .basic(.var(S1),.simple(.var(P1)),.var(O1)),_,
    .basic(.var(S2),.simple(.var(P2)),.var(O2)),_)) => S1==S2 && P1==P2 && O1==O2.
  checkConstructWhereShorthand(_) default => .false.

  checkTopValues(.select(_,_,_,.conj(.basic(.var("s"),.simple(_),.var("o")),
    .values(.oneVar("s",[.some(.literal(_))]))),_)) => .true.
  checkTopValues(_) default => .false.

  checkUnion(.select(_,_,_,.union(.basic(_,_,_),.basic(_,_,_)),_)) => .true.
  checkUnion(_) default => .false.

  checkGroupConcatSep(.select(_,.vars([.computed(.groupConcat(.some(_),.false,.some(",")),"names")]),_,_,_)) => .true.
  checkGroupConcatSep(_) default => .false.
}
