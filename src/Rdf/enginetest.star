rdf.sparql.engine.test{
  import star.
  import star.assert.
  import rdf.triple.
  import rdf.graph.
  import rdf.sparql.query.
  import rdf.sparql.solution.
  import rdf.sparql.engine.

  /* Tests for rdf.sparql.engine's evalPattern -- BGP evaluation via join (.conj/.basic), the
     pure combinators (.union/.optional/.minus/.values/.nilPattern), and FILTER/BIND (backed
     by rdf.sparql.expreval). .graph/.subSelect/.service/.annotated are exercised only for
     their "not supported" error, since they're out of v1 scope entirely. */

  vr:(string) => term.
  vr(V) => .var(V).

  lit:(concept) => term.
  lit(C) => .literal(C).

  -- Named bp, not basic, to avoid colliding with query.star's .basic pattern constructor
  -- (the same class of clash as solution.star's minus/minusSol rename).
  bp:(term,term,term) => pattern.
  bp(S,P,O) => .basic(S,.simple(P),O).

  -- evalPattern is declared "throws string" -- a direct call outside a try/catch isn't
  -- allowed unless the caller also declares throws, so every "expected to succeed" call
  -- goes through this wrapper; a throw here means the test itself is broken, so it's a hard
  -- failure rather than a counted one.
  evalOk:(graph,pattern) => solutions.
  evalOk(G,P) => valof{
    try{
      valis evalPattern(G,P)
    } catch {
      _ do valis unexpectedThrow()
    }
  }

  unexpectedThrow:() => solutions.
  unexpectedThrow() => valof{
    assert(.false);
    valis []
  }

  main:(){}.
  main(){
    Failures = ref 0;

    Alice = .uri("alice"). Bob = .uri("bob"). Carol = .uri("carol").
    Knows = .uri("knows"). Age = .uri("age").

    G0 = nullGraph;
    G1 = addTriple(G0,.tr(Alice,Knows,Bob));
    G2 = addTriple(G1,.tr(Alice,Knows,Carol));
    G3 = addTriple(G2,.tr(Bob,Knows,Carol));
    G  = addTriple(G3,.tr(Alice,Age,.int(30)));

    -- .nilPattern is the join identity -- exactly one (empty) solution.
    checkCount(Failures,evalOk(G,.nilPattern),1,".nilPattern yields the single empty mapping");

    -- .conj/.basic: a two-triple BGP joined on the shared variable ?mid.
    Bgp = .conj(bp(vr("who"),lit(Knows),vr("mid")),bp(vr("mid"),lit(Knows),lit(Carol)));
    checkCount(Failures,evalOk(G,Bgp),1,
      "?who knows ?mid . ?mid knows carol -- BGP join on the shared ?mid variable (only alice-bob-carol chains)");

    -- .union: bag union of both branches, no dedup.
    Un = .union(bp(vr("s"),lit(Knows),lit(Bob)),bp(vr("s"),lit(Knows),lit(Bob)));
    checkCount(Failures,evalOk(G,Un),2,".union keeps duplicates across identical branches (bag union)");

    -- .optional: rows with no compatible match in the right side survive unmerged, rows
    -- with one get merged.
    Opt = .optional(bp(vr("s"),lit(Knows),vr("o")),bp(vr("o"),lit(Age),vr("a")));
    checkCount(Failures,evalOk(G,Opt),3,
      ".optional -- all 3 left rows survive (none of bob/carol have an age), none merged");

    -- .minus: no shared variables between the two sides is a no-op (domain-overlap gate --
    -- see solution.star's minusSol), unlike NOT EXISTS.
    MinusNoOverlap = .minus(bp(vr("s"),lit(Knows),vr("o")),bp(lit(Alice),lit(Age),vr("a")));
    checkCount(Failures,evalOk(G,MinusNoOverlap),3,
      ".minus is a no-op when the two sides share no variables");

    -- .minus with a genuine overlap removes only the matching rows: bob only knows carol,
    -- so the right side is the single row {o=carol}, which removes exactly the left rows
    -- with o=carol (alice-carol, bob-carol) and leaves alice-bob.
    MinusOverlap = .minus(bp(vr("s"),lit(Knows),vr("o")),bp(lit(Bob),lit(Knows),vr("o")));
    checkCount(Failures,evalOk(G,MinusOverlap),1,
      ".minus removes rows whose ?o overlaps and is compatible with the right side");

    -- .values: a VALUES block becomes solutions directly.
    Vals = .values(.oneVar("s",[.some(.literal(Alice)),.some(.literal(Bob))]));
    checkCount(Failures,evalOk(G,Vals),2,".values turns a data block into solutions directly");

    -- FILTER: ?s age ?a . FILTER(?a > 25) -- keeps only alice (the only one with an age, 30).
    FilterKeep = .conj(bp(vr("s"),lit(Age),vr("a")),.filter(.gt(.term(vr("a")),.term(lit(.int(25))))));
    checkCount(Failures,evalOk(G,FilterKeep),1,"FILTER(?a > 25) keeps the one row satisfying the condition");

    FilterDrop = .conj(bp(vr("s"),lit(Age),vr("a")),.filter(.gt(.term(vr("a")),.term(lit(.int(100))))));
    checkCount(Failures,evalOk(G,FilterDrop),0,"FILTER(?a > 100) drops every row when nothing satisfies it");

    -- A FILTER expression that errors (division by zero) is treated as "not true", not a
    -- hard failure of the whole query -- per the SPARQL error-tolerance rules.
    FilterErrors = .conj(bp(vr("s"),lit(Knows),vr("o")),
      .filter(.eq(.div(.term(lit(.int(1))),.term(lit(.int(0)))),.term(lit(.int(0))))));
    checkCount(Failures,evalOk(G,FilterErrors),0,
      "FILTER with a division-by-zero error is treated as not-true, not a hard failure");

    -- BIND: ?s age ?a . BIND(?a + 1 AS ?next) -- extends each row with the computed value.
    BindPattern = .conj(bp(vr("s"),lit(Age),vr("a")),.bind(.add(.term(vr("a")),.term(lit(.int(1)))),"next"));
    ExpectBind = [emptyMapping["s"->Alice]["a"->.int(30)]["next"->.int(31)]];
    checkSolutionsEq(Failures,evalOk(G,BindPattern),ExpectBind,
      "BIND(?a + 1 AS ?next) extends each row with the computed value");

    -- A BIND expression that errors leaves the variable unbound for that solution rather
    -- than dropping the solution entirely.
    BindErrors = .conj(bp(vr("s"),lit(Knows),vr("o")),
      .bind(.div(.term(lit(.int(1))),.term(lit(.int(0)))),"bad"));
    checkCount(Failures,evalOk(G,BindErrors),3,
      "BIND with an evaluation error keeps the solution, just leaves the variable unbound");

    -- Not-yet-supported forms raise a clear error rather than silently mismatching.
    checkThrows(Failures,() => evalPattern(G,.graph(lit(Alice),.nilPattern)),
      "GRAPH raises \"not supported\" (named graphs are out of scope)");

    if Failures! == 0 then{
      logMsg(.info,"all engine tests passed")
    } else{
      logMsg(.severe,"$(Failures!) engine test(s) failed")
    };
    assert(Failures! == 0)
  }

  checkCount(Failures,Got,Expect,Descr) => valof{
    N = size(Got);
    if N==Expect then{
      logMsg(.info,"PASS: $(Descr)");
      valis ()
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL ($(Descr)): expected $(Expect) solutions, got $(N)");
      valis ()
    }
  }

  checkSolutionsEq(Failures,Got,Expect,Descr) => valof{
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
