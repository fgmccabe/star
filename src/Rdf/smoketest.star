rdf.sparql.smoketest{
  import star.
  import star.assert.
  import star.location.
  import rdf.lexer.
  import rdf.parser.
  import rdf.triple.
  import rdf.graph.
  import rdf.sparql.query.
  import rdf.sparql.solution.
  import rdf.sparql.parser.
  import rdf.sparql.engine.

  /* End-to-end smoke test, per the plan's verification step: parse real N3 data (inline,
     not loaded from a file, so this test is self-contained and portable) via the same
     lexer/parseGraph pipeline rdf.graph.parseN3 uses internally, then run real SPARQL
     queries (including PREFIX, GROUP BY/aggregate, and OPTIONAL) through the full pipeline
     -- rdf.sparql.parser (parse + Phase 0 prefix resolution) into rdf.sparql.engine
     (evalPattern/runSelect/runAsk) -- and check the results, not just that each stage
     compiles/runs in isolation as the per-module test suites already do. */

  main:(){}.
  main(){
    Failures = ref 0;

    if Grph ?= parseSmokeGraph() then{
      runQuery(Failures,Grph,
        "PREFIX ex: <http://example.org/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> " ++
        "SELECT ?name WHERE { ex:alice foaf:name ?name }",
        [emptyMapping["name"->.text([.str("Alice")])]],
        "PREFIX-qualified ground-subject query resolves and matches real parsed N3 data");

      runQuery(Failures,Grph,
        "PREFIX foaf: <http://xmlns.com/foaf/0.1/> " ++
        "SELECT ?s ?age WHERE { ?s foaf:age ?age } ORDER BY ?age",
        [emptyMapping["s"->.uri("http://example.org/bob")]["age"->.int(25)],
          emptyMapping["s"->.uri("http://example.org/alice")]["age"->.int(30)],
          emptyMapping["s"->.uri("http://example.org/carol")]["age"->.int(40)]],
        "SELECT with ORDER BY across the whole parse->resolve->evaluate pipeline");

      runQuery(Failures,Grph,
        "PREFIX foaf: <http://xmlns.com/foaf/0.1/> " ++
        "SELECT ?s (COUNT(?o) AS ?c) WHERE { ?s foaf:knows ?o } GROUP BY ?s HAVING(?c > 1)",
        [emptyMapping["s"->.uri("http://example.org/alice")]["c"->.int(2)]],
        "GROUP BY + aggregate + HAVING through the full pipeline");

      runQuery(Failures,Grph,
        "PREFIX ex: <http://example.org/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> " ++
        "SELECT ?s ?age WHERE { ?s foaf:knows ex:carol . OPTIONAL { ?s foaf:age ?age } }",
        [emptyMapping["s"->.uri("http://example.org/alice")]["age"->.int(30)],
          emptyMapping["s"->.uri("http://example.org/bob")]["age"->.int(25)]],
        "OPTIONAL through the full pipeline");

      runAskQuery(Failures,Grph,
        "PREFIX ex: <http://example.org/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> " ++
        "ASK { ex:alice foaf:knows ex:carol }",
        .true,"ASK true through the full pipeline");
      runAskQuery(Failures,Grph,
        "PREFIX ex: <http://example.org/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> " ++
        "ASK { ex:carol foaf:knows ex:alice }",
        .false,"ASK false through the full pipeline")
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL: could not load the smoke-test N3 fixture")
    };

    if Failures! == 0 then{
      logMsg(.info,"all smoke tests passed")
    } else{
      logMsg(.severe,"$(Failures!) smoke test(s) failed")
    };
    assert(Failures! == 0)
  }

  -- The lexer only recognises a statement-terminating "." when it's followed by a newline
  -- or tab (see lexer.star's nxxTok(`.`,...) clauses) -- a trailing plain space doesn't
  -- form the ". " token rdfTriple/prefix expect, so every statement here ends with \n.
  smokeN3:() => string.
  smokeN3() =>
    "@prefix ex: <http://example.org/>.\n" ++
    "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n" ++
    "ex:alice foaf:name \"Alice\"; foaf:age 30; foaf:knows ex:bob, ex:carol.\n" ++
    "ex:bob foaf:name \"Bob\"; foaf:age 25; foaf:knows ex:carol.\n" ++
    "ex:carol foaf:name \"Carol\"; foaf:age 40.\n".

  parseSmokeGraph:() => option[graph].
  parseSmokeGraph() => valof{
    Toks = allTokens(startLoc("smoketest"),smokeN3()::cons[char]);
    if Trpls ?= (parseGraph() --> Toks) then
      valis .some(foldRight((Tr,Gx) => addTriple(Gx,Tr),nullGraph,Trpls))
    else
      valis .none
  }

  runQuery(Failures,Grph,Q,Expect,Descr) => valof{
    Toks = allTokens(.locn("t",1,1,0,0),Q::cons[char]);
    if Qry ?= (queryUnit --> Toks) then{
      try{
        Got = runQueryOf(Grph,Qry);
        if sameBag(Got,Expect) then{
          logMsg(.info,"PASS: $(Descr)");
          valis ()
        } else{
          Failures := Failures! + 1;
          logMsg(.severe,"FAIL ($(Descr)): expected $(Expect), got $(Got)");
          valis ()
        }
      } catch {
        Msg do {
          Failures := Failures! + 1;
          logMsg(.severe,"FAIL ($(Descr)): evaluation error: $(Msg)");
          valis ()
        }
      }
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL ($(Descr)): query failed to parse: $(Q)");
      valis ()
    }
  }

  runQueryOf:(graph,query) => solutions throws string.
  runQueryOf(G,.select(Mod,Proj,_,P,Mods)) => runSelect(G,Mod,Proj,P,Mods).
  runQueryOf(_,_) default => throw "smoketest only drives SELECT via runQuery".

  runAskQuery(Failures,Grph,Q,Expect,Descr) => valof{
    Toks = allTokens(.locn("t",1,1,0,0),Q::cons[char]);
    if Qry ?= (queryUnit --> Toks) then{
      try{
        Got = runAskOf(Grph,Qry);
        if Got==Expect then{
          logMsg(.info,"PASS: $(Descr)");
          valis ()
        } else{
          Failures := Failures! + 1;
          logMsg(.severe,"FAIL ($(Descr)): expected $(Expect), got $(Got)");
          valis ()
        }
      } catch {
        Msg do {
          Failures := Failures! + 1;
          logMsg(.severe,"FAIL ($(Descr)): evaluation error: $(Msg)");
          valis ()
        }
      }
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL ($(Descr)): query failed to parse: $(Q)");
      valis ()
    }
  }

  runAskOf:(graph,query) => boolean throws string.
  runAskOf(G,.ask(_,P,_)) => runAsk(G,P).
  runAskOf(_,_) default => throw "smoketest only drives ASK via runAskQuery".

  -- solutions are a multiset -- compare as bags, not list order.
  sameBag:(solutions,solutions) => boolean.
  sameBag(Got,Expect) => size(Got)==size(Expect) && containsAllRows(Got,Expect).

  containsAllRows:(solutions,solutions) => boolean.
  containsAllRows([],_) => .true.
  containsAllRows([M,..Rest],Expect) where Rem ?= removeOneRow(M,Expect) => containsAllRows(Rest,Rem).
  containsAllRows(_,_) default => .false.

  removeOneRow:(mapping,solutions) => option[solutions].
  removeOneRow(_,[]) => .none.
  removeOneRow(M,[M2,..Rest]) where M==M2 => .some(Rest).
  removeOneRow(M,[M2,..Rest]) where Rem ?= removeOneRow(M,Rest) => .some([M2,..Rem]).
  removeOneRow(_,_) default => .none.
}
