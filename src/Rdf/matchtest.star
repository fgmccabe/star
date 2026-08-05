rdf.sparql.match.test{
  import star.
  import star.assert.
  import rdf.triple.
  import rdf.graph.
  import rdf.sparql.query.
  import rdf.sparql.solution.
  import rdf.sparql.match.

  /* Tests for rdf.sparql.match's indexed triple-pattern matching, covering all 8
     bound/unbound S/P/O combinations against a small hand-built graph -- built via chained
     addTriple calls, not foldRight/(addTriple,nullGraph,[...]) over a list literal, since
     folding directly over an unannotated collection literal was found to hang
     star.compiler (see match.star's candidateSet comment). */

  vr:(string) => term.
  vr(V) => .var(V).

  lit:(concept) => term.
  lit(C) => .literal(C).

  main:(){}.
  main(){
    Failures = ref 0;

    Alice = .uri("alice"). Bob = .uri("bob"). Carol = .uri("carol"). Nobody = .uri("nobody").
    Knows = .uri("knows"). Age = .uri("age"). RdfType = .uri("type"). Person = .uri("Person").

    G0 = nullGraph;
    G1 = addTriple(G0,.tr(Alice,Knows,Bob));
    G2 = addTriple(G1,.tr(Alice,Knows,Carol));
    G3 = addTriple(G2,.tr(Bob,Knows,Carol));
    G4 = addTriple(G3,.tr(Alice,Age,.int(30)));
    G5 = addTriple(G4,.tr(Alice,RdfType,Person));
    G  = addTriple(G5,.tr(Bob,RdfType,Person));

    -- S,P,O all unbound -- no index narrowing at all, falls back to a full scan.
    checkCount(Failures,matchBasic(G,vr("s"),.simple(vr("p")),vr("o")),6,
      "?s ?p ?o -- unbound/unbound/unbound matches every triple (full scan)");

    -- One position bound (symbolic, present) -- narrows via that position's index alone.
    checkCount(Failures,matchBasic(G,lit(Alice),.simple(vr("p")),vr("o")),4,
      "alice ?p ?o -- bound subject narrows via the subjects index");
    checkCount(Failures,matchBasic(G,vr("s"),.simple(lit(Knows)),vr("o")),3,
      "?s knows ?o -- bound predicate narrows via the predicates index");
    checkCount(Failures,matchBasic(G,vr("s"),.simple(vr("p")),lit(Carol)),2,
      "?s ?p carol -- bound object narrows via the objects index");

    -- Two positions bound -- intersecting two index sets.
    checkCount(Failures,matchBasic(G,lit(Alice),.simple(lit(Knows)),vr("o")),2,
      "alice knows ?o -- subject+predicate intersect");
    checkCount(Failures,matchBasic(G,lit(Alice),.simple(vr("p")),lit(Bob)),1,
      "alice ?p bob -- subject+object intersect (no predicates-index contribution)");
    checkCount(Failures,matchBasic(G,vr("s"),.simple(lit(Knows)),lit(Carol)),2,
      "?s knows carol -- predicate+object intersect");

    -- All three bound -- a ground triple pattern, present vs. absent.
    checkSolutionsEq(Failures,matchBasic(G,lit(Alice),.simple(lit(Knows)),lit(Bob)),[emptyMapping],
      "alice knows bob -- fully ground, present triple yields exactly the empty mapping");
    checkCount(Failures,matchBasic(G,lit(Alice),.simple(lit(Knows)),lit(Alice)),0,
      "alice knows alice -- fully ground, absent triple yields no solutions");

    -- Bound-and-symbolic but absent from the graph entirely -- must narrow to zero
    -- candidates, not silently fall back to a full scan (which would wrongly match).
    checkCount(Failures,matchBasic(G,lit(Nobody),.simple(vr("p")),vr("o")),0,
      "nobody ?p ?o -- bound subject absent from the index narrows to zero, no full-scan fallback");

    -- Bound but unindexed (a literal value, not a symbolic concept) -- the predicate index
    -- narrows to one candidate, then the literal object is checked by direct equality.
    checkCount(Failures,matchBasic(G,vr("s"),.simple(lit(Age)),lit(.int(30))),1,
      "?s age 30 -- literal object position filtered by equality, not indexed");
    checkCount(Failures,matchBasic(G,vr("s"),.simple(lit(Age)),lit(.int(99))),0,
      "?s age 99 -- literal object mismatch yields no solutions");

    -- Repeated variable: both occurrences must agree -- no self-knows triple exists, so
    -- this must match nothing (not silently skip the consistency check).
    checkCount(Failures,matchBasic(G,vr("x"),.simple(lit(Knows)),vr("x")),0,
      "?x knows ?x -- repeated variable requires both occurrences to agree");

    if Failures! == 0 then{
      logMsg(.info,"all match tests passed")
    } else{
      logMsg(.severe,"$(Failures!) match test(s) failed")
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
}
