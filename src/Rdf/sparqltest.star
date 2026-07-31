rdf.sparql.test{
  import star.
  import star.location.
  import rdf.lexer.
  import rdf.sparql.parser.

  -- Smoke test for the SPARQL recognizer (rdf.sparql.parser): each query is
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

    if Failures! == 0 then{
      logMsg(.info,"all sparql recognizer tests passed");
      valis 0
    } else{
      logMsg(.severe,"$(Failures!) sparql recognizer test(s) failed");
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
}
