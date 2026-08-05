rdf.sparql.engine{
  import star.
  import rdf.triple.
  import rdf.graph.
  import rdf.sparql.query.
  import rdf.sparql.solution.
  import rdf.sparql.match.

  /* The graph-pattern evaluator: walks a (already prefix-resolved, see rdf.sparql.prologue)
     pattern against a graph, producing its multiset of solution mappings, per the SPARQL
     algebra (https://www.w3.org/TR/sparql12-query/#sparqlAlgebra). BGPs (.basic/.conj) are
     matched via rdf.sparql.match and combined with join; .union/.optional/.minus/.values are
     pure combinators needing no expression evaluation. .filter/.bind need expreval.star
     (not yet built -- a later phase) and raise a clear "not yet supported" error for now,
     same as .graph/.subSelect/.service, which are out of v1 scope entirely (see the phased
     plan): GRAPH because rdf.graph models only a single unnamed graph, subqueries because
     they're deferred purely for sequencing, and SERVICE because no HTTP client exists here.
     SERVICE SILENT is the one exception -- per its SPARQL contract it must still yield an
     (empty) pattern result rather than failing the query. */

  public evalPattern:(graph,pattern) => solutions throws string.
  evalPattern(_,.nilPattern) => unitSolutions.
  evalPattern(G,.basic(S,P,O)) => matchBasic(G,S,P,O).
  evalPattern(G,.conj(A,B)) => join(evalPattern(G,A),evalPattern(G,B)).
  evalPattern(G,.union(A,B)) => unionSol(evalPattern(G,A),evalPattern(G,B)).
  evalPattern(G,.optional(A,B)) => leftJoin(evalPattern(G,A),evalPattern(G,B),(_)=>.true).
  evalPattern(G,.minus(A,B)) => minusSol(evalPattern(G,A),evalPattern(G,B)).
  evalPattern(_,.values(D)) => valuesToSolutions(D).
  evalPattern(_,.filter(_)) => throw "FILTER is not supported yet (expression evaluation is a later phase)".
  evalPattern(_,.bind(_,_)) => throw "BIND is not supported yet (expression evaluation is a later phase)".
  evalPattern(_,.service(_,_,.true)) => unitSolutions.
  /* GRAPH/subSelect/SERVICE(non-silent)/RDF-star annotations all fall through to this one
     catch-all rather than getting their own clauses -- .graph(_,_) as a pattern here was
     found to collide with rdf.graph's own graph type/record constructor (both named
     "graph" and both in scope), the same class of name clash as solution.star's
     minus/.minus rename; a single non-constructor-specific clause sidesteps it. */
  evalPattern(_,_) default =>
    throw "this pattern form is not supported yet (GRAPH, subqueries, non-silent SERVICE, and RDF-star annotations are all out of v1 scope)".

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
