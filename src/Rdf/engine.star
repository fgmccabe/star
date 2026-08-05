rdf.sparql.engine{
  import star.
  import rdf.triple.
  import rdf.graph.
  import rdf.sparql.query.
  import rdf.sparql.solution.
  import rdf.sparql.match.
  import rdf.sparql.expreval.

  /* The graph-pattern evaluator: walks a (already prefix-resolved, see rdf.sparql.prologue)
     pattern against a graph, producing its multiset of solution mappings, per the SPARQL
     algebra (https://www.w3.org/TR/sparql12-query/#sparqlAlgebra). BGPs (.basic/.conj) are
     matched via rdf.sparql.match and combined with join; .union/.optional/.minus/.values are
     pure combinators needing no expression evaluation.

     FILTER/BIND have no sub-pattern of their own in query.star's AST -- .filter(E)/.bind(E,V)
     are conjuncts that apply to whatever they're accumulated onto, per the parser's
     left-associative .conj chaining (the same "already the accumulated pattern" convention
     .optional/.minus rely on -- see sparqltest.star's checkOptionalAccum). So .conj(A,.filter(E))
     and .conj(A,.bind(E,V)) are special-cased ahead of the generic .conj(A,B)=>join(...)
     clause, matching the SPARQL algebra's own translation (a FILTER applies to the whole
     accumulated group via Filter(expr,Join(...)), not to one arbitrary conjunct via a plain
     Join) rather than treating .filter/.bind as "just another pattern to join". A bare
     top-level .filter/.bind (no preceding conjunct) applies to the join identity instead.

     .graph/.subSelect/.service(non-silent)/.annotated are out of v1 scope entirely (see the
     phased plan): GRAPH because rdf.graph models only a single unnamed graph, subqueries
     because they're deferred purely for sequencing, and SERVICE because no HTTP client
     exists here. SERVICE SILENT is the one exception -- per its SPARQL contract it must
     still yield an (empty) pattern result rather than failing the query. */

  public evalPattern:(graph,pattern) => solutions throws string.
  evalPattern(_,.nilPattern) => unitSolutions.
  evalPattern(G,.basic(S,P,O)) => matchBasic(G,S,P,O).
  evalPattern(G,.conj(A,.filter(E))) => filterSol((M) => filterPasses(G,M,E),evalPattern(G,A)).
  evalPattern(G,.conj(A,.bind(E,V))) => extendSol(G,evalPattern(G,A),E,V).
  evalPattern(G,.conj(A,B)) => join(evalPattern(G,A),evalPattern(G,B)).
  evalPattern(G,.union(A,B)) => unionSol(evalPattern(G,A),evalPattern(G,B)).
  evalPattern(G,.optional(A,B)) => leftJoin(evalPattern(G,A),evalPattern(G,B),(_)=>.true).
  evalPattern(G,.minus(A,B)) => minusSol(evalPattern(G,A),evalPattern(G,B)).
  evalPattern(_,.values(D)) => valuesToSolutions(D).
  evalPattern(G,.filter(E)) => filterSol((M) => filterPasses(G,M,E),unitSolutions).
  evalPattern(G,.bind(E,V)) => extendSol(G,unitSolutions,E,V).
  evalPattern(_,.service(_,_,.true)) => unitSolutions.
  /* GRAPH/subSelect/SERVICE(non-silent)/RDF-star annotations all fall through to this one
     catch-all rather than getting their own clauses -- .graph(_,_) as a pattern here was
     found to collide with rdf.graph's own graph type/record constructor (both named
     "graph" and both in scope), the same class of name clash as solution.star's
     minus/.minus rename; a single non-constructor-specific clause sidesteps it. */
  evalPattern(_,_) default =>
    throw "this pattern form is not supported yet (GRAPH, subqueries, non-silent SERVICE, and RDF-star annotations are all out of v1 scope)".

  -- FILTER: keep M where E's Effective Boolean Value is true; any evaluation error means
  -- "not true", never a hard failure of the whole query (per the SPARQL error-tolerance
  -- rules -- see expreval.star's header comment).
  filterPasses:(graph,mapping,expression) => boolean.
  filterPasses(G,M,E) => valof{
    try{
      valis ebv(evalExpr((P) => evalPattern(G,P),M,E))
    } catch {
      _ do valis .false
    }
  }

  -- BIND: extend each mapping with V bound to E's value. Per the SPARQL Extend semantics,
  -- an evaluation error leaves V unbound for that solution rather than dropping it; rebinding
  -- an already-bound variable is a (static, query-authoring) SPARQL error that we don't
  -- validate for here, so it's treated the same way -- leave the mapping unchanged.
  extendSol:(graph,solutions,expression,string) => solutions.
  extendSol(_,[],_,_) => [].
  extendSol(G,[M,..Rest],E,V) => [extendOne(G,M,E,V),..extendSol(G,Rest,E,V)].

  extendOne:(graph,mapping,expression,string) => mapping.
  extendOne(_,M,_,V) where _ ?= M[V] => M.
  extendOne(G,M,E,V) => valof{
    try{
      valis extend(M,V,evalExpr((P) => evalPattern(G,P),M,E))
    } catch {
      _ do valis M
    }
  }

  valuesToSolutions:(dataBlock) => solutions.
  valuesToSolutions(.oneVar(V,Vals)) => oneVarRows(V,Vals).
  valuesToSolutions(.full(Vars,Rows)) => fullRows(Vars,Rows).

  oneVarRows:(string,cons[option[term]]) => solutions.
  oneVarRows(_,[]) => [].
  oneVarRows(V,[.none,..Rest]) => [emptyMapping,..oneVarRows(V,Rest)].
  oneVarRows(V,[.some(.literal(C)),..Rest]) => [emptyMapping[V->C],..oneVarRows(V,Rest)].

  fullRows:(cons[string],cons[cons[option[term]]]) => solutions.
  fullRows(_,[]) => [].
  fullRows(Vars,[Row,..Rest]) => [rowToMapping(Vars,Row,emptyMapping),..fullRows(Vars,Rest)].

  rowToMapping:(cons[string],cons[option[term]],mapping) => mapping.
  rowToMapping([],_,M) => M.
  rowToMapping(_,[],M) => M.
  rowToMapping([_,..Vs],[.none,..Vals],M) => rowToMapping(Vs,Vals,M).
  rowToMapping([V,..Vs],[.some(.literal(C)),..Vals],M) => rowToMapping(Vs,Vals,M[V->C]).
}
