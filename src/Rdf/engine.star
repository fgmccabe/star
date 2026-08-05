rdf.sparql.engine{
  import star.
  import star.sort.
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

  /* ===== Solution modifiers (Phase e) and SELECT/ASK result forms (Phase g) =====

     Evaluation order, per the plan (https://www.w3.org/TR/sparql12-query/#solutionModifiers):
     pattern eval -> GROUP BY -> HAVING -> SELECT-expression eval -> ORDER BY -> Project ->
     DISTINCT/REDUCED -> OFFSET -> LIMIT.

     One simplification from the spec's strict order: SELECT-expression evaluation actually
     runs *before* HAVING here (not after, as the section list above might suggest), so a
     HAVING condition can reference a SELECT alias -- e.g. "SELECT (COUNT(?s) AS ?c) ...
     HAVING(?c > 1)", which is what this project's own sparqltest.star already expects, and
     which many real engines support even though it's not the strict per-clause spec order.
     Aggregates are only recognised at the *top level* of a SELECT-computed-var or a HAVING
     condition (e.g. "COUNT(?s) AS ?c" or a bare "HAVING(COUNT(?s))"); an aggregate nested
     inside a larger expression (e.g. "COUNT(?s)+1 AS ?c") isn't rewritten and falls through
     to evalExpr, which doesn't know how to evaluate a raw .aggregate/.groupConcat node
     against a single mapping -- a documented v1 simplification, not a silent gap, given how
     much larger correctly threading aggregate context through every expression constructor
     would be.

     GROUP BY's implicit single group (no explicit GROUP BY, but an aggregate is used
     somewhere) is handled explicitly, including the zero-solution case: COUNT(*) with zero
     matching solutions and no GROUP BY still produces one row with count 0, not zero rows
     (see the plan's Risk #3). Ungrouped queries (no GROUP BY, no aggregate) are represented
     as one singleton group per solution, so the same per-group code path handles both. */

  public runSelect:(graph,selectModifier,projection,pattern,solutionMods) => solutions throws string.
  runSelect(G,Mod,Proj,P,Mods) => valof{
    Sols = evalPattern(G,P);
    UsesAgg = usesAggregate(Proj,Mods.having);
    Groups = makeGroups(G,Sols,Mods.grouping,UsesAgg);
    Rows = groupsToRows(G,Groups,Proj,Mods.having);
    Ordered = sortRows(G,Rows,Mods.ordering);
    Projected = Ordered//((R) => projectRow(R,Proj));
    Deduped = applySelectMod(Mod,Projected);
    valis applyOffsetLimit(Deduped,Mods.offset,Mods.limit)
  }

  -- ASK: did the pattern produce at least one solution.
  public runAsk:(graph,pattern) => boolean throws string.
  runAsk(G,P) => size(evalPattern(G,P)) > 0.

  usesAggregate:(projection,cons[expression]) => boolean.
  usesAggregate(.selectAll,Having) => havingHasAgg(Having).
  usesAggregate(.vars(SVs),Having) => selectHasAgg(SVs) || havingHasAgg(Having).

  selectHasAgg:(cons[selectVar]) => boolean.
  selectHasAgg([]) => .false.
  selectHasAgg([.plain(_),..Rest]) => selectHasAgg(Rest).
  selectHasAgg([.computed(E,_),..Rest]) => isAggExpr(E) || selectHasAgg(Rest).

  havingHasAgg:(cons[expression]) => boolean.
  havingHasAgg([]) => .false.
  havingHasAgg([E,..Rest]) => isAggExpr(E) || havingHasAgg(Rest).

  isAggExpr:(expression) => boolean.
  isAggExpr(.aggregate(_,_,_)) => .true.
  isAggExpr(.groupConcat(_,_,_)) => .true.
  isAggExpr(_) default => .false.

  makeGroups:(graph,solutions,cons[groupCondition],boolean) => cons[solutions] throws string.
  makeGroups(_,Sols,[],.false) => wrapSingletons(Sols).
  makeGroups(_,Sols,[],.true) => [Sols].
  makeGroups(G,Sols,GCs,_) => groupByKey(G,Sols,GCs).

  wrapSingletons:(solutions) => cons[solutions].
  wrapSingletons([]) => [].
  wrapSingletons([M,..Rest]) => [[M],..wrapSingletons(Rest)].

  groupKey:(graph,mapping,cons[groupCondition]) => cons[concept] throws string.
  groupKey(_,_,[]) => [].
  groupKey(G,M,[.groupExpr(E,_),..Gs]) =>
    [evalExpr((P) => evalPattern(G,P),M,E),..groupKey(G,M,Gs)].

  -- Groups by carrying (key,members) pairs through a linear scan (no hashable[cons[concept]]
  -- instance is assumed to exist, so this can't use a map -- fine at the scale a test/small
  -- graph needs; a real perf pass would index this).
  groupByKey:(graph,solutions,cons[groupCondition]) => cons[solutions] throws string.
  groupByKey(G,Sols,GCs) => valof{
    Pairs = buildGroupPairs(G,Sols,GCs);
    valis pairsToGroups(Pairs)
  }

  buildGroupPairs:(graph,solutions,cons[groupCondition]) => cons[(cons[concept],solutions)] throws string.
  buildGroupPairs(_,[],_) => [].
  buildGroupPairs(G,[M,..Rest],GCs) => valof{
    K = groupKey(G,M,GCs);
    Tail = buildGroupPairs(G,Rest,GCs);
    valis addToGroupPairs(K,M,Tail)
  }

  addToGroupPairs:(cons[concept],mapping,cons[(cons[concept],solutions)]) => cons[(cons[concept],solutions)].
  addToGroupPairs(K,M,[]) => [(K,[M])].
  addToGroupPairs(K,M,[(K2,Grp),..Rest]) where K==K2 => [(K2,[M,..Grp]),..Rest].
  addToGroupPairs(K,M,[Pair,..Rest]) => [Pair,..addToGroupPairs(K,M,Rest)].

  pairsToGroups:(cons[(cons[concept],solutions)]) => cons[solutions].
  pairsToGroups([]) => [].
  pairsToGroups([(_,Grp),..Rest]) => [Grp,..pairsToGroups(Rest)].

  groupsToRows:(graph,cons[solutions],projection,cons[expression]) => solutions throws string.
  groupsToRows(_,[],_,_) => [].
  groupsToRows(G,[Grp,..Rest],Proj,Having) => valof{
    Rep = firstOfGroup(Grp);
    Extended = extendWithSelectVars(G,Grp,Rep,Proj);
    RestRows = groupsToRows(G,Rest,Proj,Having);
    if passesHaving(G,Extended,Having) then
      valis [Extended,..RestRows]
    else
      valis RestRows
  }

  firstOfGroup:(solutions) => mapping.
  firstOfGroup([]) => emptyMapping.
  firstOfGroup([M,.._]) => M.

  extendWithSelectVars:(graph,solutions,mapping,projection) => mapping throws string.
  extendWithSelectVars(_,_,Rep,.selectAll) => Rep.
  extendWithSelectVars(G,Grp,Rep,.vars(SVs)) => extendVars(G,Grp,Rep,SVs).

  extendVars:(graph,solutions,mapping,cons[selectVar]) => mapping throws string.
  extendVars(_,_,Rep,[]) => Rep.
  extendVars(G,Grp,Rep,[.plain(_),..Rest]) => extendVars(G,Grp,Rep,Rest).
  extendVars(G,Grp,Rep,[.computed(E,Alias),..Rest]) => valof{
    V = evalComputedVar(G,Grp,Rep,E);
    valis extendVars(G,Grp,Rep[Alias->V],Rest)
  }

  evalComputedVar:(graph,solutions,mapping,expression) => concept throws string.
  evalComputedVar(G,Grp,_,.aggregate(Nm,Arg,Dist)) => evalAggregateFn((P) => evalPattern(G,P),Grp,Nm,Arg,Dist).
  evalComputedVar(G,Grp,_,.groupConcat(Arg,Dist,Sep)) => evalGroupConcatFn((P) => evalPattern(G,P),Grp,Arg,Dist,Sep).
  evalComputedVar(G,_,Rep,E) => evalExpr((P) => evalPattern(G,P),Rep,E).

  evalAggregateFn:((pattern)=>solutions throws string,solutions,string,option[expression],boolean) => concept throws string.
  evalAggregateFn(_,Grp,"count",.none,_) => .int(size(Grp)).
  evalAggregateFn(EvalP,Grp,"count",.some(E),Dist) => .int(size(aggValues(EvalP,Grp,E,Dist))).
  evalAggregateFn(EvalP,Grp,"sum",.some(E),Dist) => sumVals(aggValues(EvalP,Grp,E,Dist)).
  evalAggregateFn(EvalP,Grp,"avg",.some(E),Dist) => avgVals(aggValues(EvalP,Grp,E,Dist)).
  evalAggregateFn(EvalP,Grp,"min",.some(E),Dist) => minVals(aggValues(EvalP,Grp,E,Dist)).
  evalAggregateFn(EvalP,Grp,"max",.some(E),Dist) => maxVals(aggValues(EvalP,Grp,E,Dist)).
  evalAggregateFn(EvalP,Grp,"sample",.some(E),_) => sampleVal(EvalP,Grp,E).
  evalAggregateFn(_,_,Nm,_,_) default => throw "$(Nm)() aggregate is not supported".

  -- Evaluates E against every mapping in the group, skipping rows where it errors (mirrors
  -- FILTER's error tolerance); DISTINCT dedups by plain concept equality.
  aggValues:((pattern)=>solutions throws string,solutions,expression,boolean) => cons[concept] throws string.
  aggValues(_,[],_,_) => [].
  aggValues(EvalP,[M,..Rest],E,Dist) => valof{
    RestVals = aggValues(EvalP,Rest,E,Dist);
    try{
      V = evalExpr(EvalP,M,E);
      valis addAggVal(V,Dist,RestVals)
    } catch {
      _ do valis RestVals
    }
  }

  addAggVal:(concept,boolean,cons[concept]) => cons[concept].
  addAggVal(V,.false,Vs) => [V,..Vs].
  addAggVal(V,.true,Vs) => (isMemberEq(V,Vs) ?? Vs || [V,..Vs]).

  isMemberEq:(concept,cons[concept]) => boolean.
  isMemberEq(_,[]) => .false.
  isMemberEq(V,[W,..Ws]) => V==W || isMemberEq(V,Ws).

  sumVals:(cons[concept]) => concept throws string.
  sumVals([]) => .int(0).
  sumVals([V,..Vs]) => numAdd(V,sumVals(Vs)).

  avgVals:(cons[concept]) => concept throws string.
  avgVals([]) => .int(0).
  avgVals(Vs) => numDiv(sumVals(Vs),.int(size(Vs))).

  minVals:(cons[concept]) => concept throws string.
  minVals([]) => throw "MIN() of an empty group".
  minVals([V]) => V.
  minVals([V,..Vs]) => valof{
    Rest = minVals(Vs);
    valis (numCompare(V,Rest) =< 0 ?? V || Rest)
  }

  maxVals:(cons[concept]) => concept throws string.
  maxVals([]) => throw "MAX() of an empty group".
  maxVals([V]) => V.
  maxVals([V,..Vs]) => valof{
    Rest = maxVals(Vs);
    valis (numCompare(V,Rest) >= 0 ?? V || Rest)
  }

  sampleVal:((pattern)=>solutions throws string,solutions,expression) => concept throws string.
  sampleVal(_,[],_) => throw "SAMPLE() of an empty group".
  sampleVal(EvalP,[M,.._],E) => evalExpr(EvalP,M,E).

  evalGroupConcatFn:((pattern)=>solutions throws string,solutions,option[expression],boolean,option[string]) => concept throws string.
  evalGroupConcatFn(_,_,.none,_,_) => .text([.str("")]).
  evalGroupConcatFn(EvalP,Grp,.some(E),Dist,Sep) =>
    .text([.str(joinStrs(aggTextValues(EvalP,Grp,E,Dist),sepOf(Sep)))]).

  sepOf:(option[string]) => string.
  sepOf(.some(S)) => S.
  sepOf(.none) default => " ".

  aggTextValues:((pattern)=>solutions throws string,solutions,expression,boolean) => cons[string] throws string.
  aggTextValues(_,[],_,_) => [].
  aggTextValues(EvalP,[M,..Rest],E,Dist) => valof{
    RestVals = aggTextValues(EvalP,Rest,E,Dist);
    try{
      V = lexicalForm(evalExpr(EvalP,M,E));
      valis addAggText(V,Dist,RestVals)
    } catch {
      _ do valis RestVals
    }
  }

  addAggText:(string,boolean,cons[string]) => cons[string].
  addAggText(S,.false,Ss) => [S,..Ss].
  addAggText(S,.true,Ss) => (strMember(S,Ss) ?? Ss || [S,..Ss]).

  strMember:(string,cons[string]) => boolean.
  strMember(_,[]) => .false.
  strMember(S,[T,..Ts]) => S==T || strMember(S,Ts).

  joinStrs:(cons[string],string) => string.
  joinStrs([],_) => "".
  joinStrs([S],_) => S.
  joinStrs([S,..Ss],Sep) => S++Sep++joinStrs(Ss,Sep).

  -- HAVING: all conditions must have EBV true, evaluated against the extended mapping (so a
  -- condition can reference a SELECT alias -- see this section's header comment). An
  -- evaluation error means "not true", same as FILTER.
  passesHaving:(graph,mapping,cons[expression]) => boolean.
  passesHaving(_,_,[]) => .true.
  passesHaving(G,M,[E,..Es]) => havingOk(G,M,E) && passesHaving(G,M,Es).

  havingOk:(graph,mapping,expression) => boolean.
  havingOk(G,M,E) => valof{
    try{
      valis ebv(evalExpr((P) => evalPattern(G,P),M,E))
    } catch {
      _ do valis .false
    }
  }

  -- ORDER BY: stable merge sort (star.sort) over the extended mappings, so it can reference
  -- either original variables or SELECT aliases, same as HAVING. A comparison error (e.g.
  -- ordering by an unbound variable) is treated as "equal" rather than failing the sort.
  sortRows:(graph,solutions,cons[orderCondition]) => solutions.
  sortRows(_,Sols,[]) => Sols.
  sortRows(G,Sols,OCs) => sort(Sols,(M1,M2) => orderLess(G,M1,M2,OCs)).

  orderLess:(graph,mapping,mapping,cons[orderCondition]) => boolean.
  orderLess(_,_,_,[]) => .false.
  orderLess(G,M1,M2,[OC,..Rest]) => valof{
    C = compareOrderCond(G,M1,M2,OC);
    if C < 0 then
      valis .true
    else if C > 0 then
      valis .false
    else
      valis orderLess(G,M1,M2,Rest)
  }

  compareOrderCond:(graph,mapping,mapping,orderCondition) => integer.
  compareOrderCond(G,M1,M2,.asc(E)) => compareExprValues(G,M1,M2,E).
  compareOrderCond(G,M1,M2,.desc(E)) => -compareExprValues(G,M1,M2,E).

  compareExprValues:(graph,mapping,mapping,expression) => integer.
  compareExprValues(G,M1,M2,E) => valof{
    try{
      V1 = evalExpr((P) => evalPattern(G,P),M1,E);
      V2 = evalExpr((P) => evalPattern(G,P),M2,E);
      valis compareConcepts(V1,V2)
    } catch {
      _ do valis 0
    }
  }

  -- Same-type comparisons only (int/int, float/float, string/string, bool/bool, uri/uri) --
  -- no cross-type numeric promotion here, unlike expreval.star's numCompare; a documented
  -- simplification, since ORDER BY only needs *a* consistent ordering, not SPARQL's full
  -- comparison-operator semantics. Mismatched types sort as equal (stable, harmless).
  compareConcepts:(concept,concept) => integer.
  compareConcepts(.int(A),.int(B)) => cmp3(A,B).
  compareConcepts(.flt(A),.flt(B)) => cmp3(A,B).
  compareConcepts(.bool(.false),.bool(.true)) => -1.
  compareConcepts(.bool(.true),.bool(.false)) => 1.
  compareConcepts(.bool(_),.bool(_)) => 0.
  compareConcepts(.uri(A),.uri(B)) => cmp3(A,B).
  compareConcepts(.text([.str(A)]),.text([.str(B)])) => cmp3(A,B).
  compareConcepts(_,_) default => 0.

  cmp3:all t ~~ comp[t] |= (t,t) => integer.
  cmp3(A,B) where A<B => -1.
  cmp3(A,B) where A>B => 1.
  cmp3(_,_) => 0.

  -- Project: narrow each extended mapping down to just the requested SELECT vars (or keep
  -- everything for SELECT *). A variable/alias with no binding in a given row is simply
  -- omitted, not an error -- normal for e.g. an OPTIONAL that didn't match.
  projectRow:(mapping,projection) => mapping.
  projectRow(M,.selectAll) => M.
  projectRow(M,.vars(SVs)) => projectVars(M,SVs,emptyMapping).

  projectVars:(mapping,cons[selectVar],mapping) => mapping.
  projectVars(_,[],Acc) => Acc.
  projectVars(M,[.plain(V),..Rest],Acc) => projectVars(M,Rest,copyVar(M,V,Acc)).
  projectVars(M,[.computed(_,Alias),..Rest],Acc) => projectVars(M,Rest,copyVar(M,Alias,Acc)).

  copyVar:(mapping,string,mapping) => mapping.
  copyVar(M,V,Acc) where C ?= M[V] => Acc[V->C].
  copyVar(_,_,Acc) default => Acc.

  -- DISTINCT/REDUCED. REDUCED only *permits* removing duplicates (it isn't required to
  -- remove all of them); adjacent-only dedup is a valid, cheap implementation of that.
  applySelectMod:(selectModifier,solutions) => solutions.
  applySelectMod(.noModifier,Sols) => Sols.
  applySelectMod(.reduced,Sols) => dedupAdjacent(Sols).
  applySelectMod(.distinct,Sols) => dedupAll(Sols).

  dedupAll:(solutions) => solutions.
  dedupAll([]) => [].
  dedupAll([M,..Rest]) => [M,..dedupAll(removeEq(M,Rest))].

  removeEq:(mapping,solutions) => solutions.
  removeEq(_,[]) => [].
  removeEq(M,[M2,..Rest]) where M==M2 => removeEq(M,Rest).
  removeEq(M,[M2,..Rest]) => [M2,..removeEq(M,Rest)].

  dedupAdjacent:(solutions) => solutions.
  dedupAdjacent([]) => [].
  dedupAdjacent([M]) => [M].
  dedupAdjacent([M1,M2,..Rest]) where M1==M2 => dedupAdjacent([M1,..Rest]).
  dedupAdjacent([M1,..Rest]) => [M1,..dedupAdjacent(Rest)].

  applyOffsetLimit:(solutions,option[integer],option[integer]) => solutions.
  applyOffsetLimit(Sols,Offset,Limit) => takeLimit(dropOffset(Sols,Offset),Limit).

  dropOffset:(solutions,option[integer]) => solutions.
  dropOffset(Sols,.none) => Sols.
  dropOffset(Sols,.some(N)) => dropN(Sols,N).

  dropN:(solutions,integer) => solutions.
  dropN(Sols,N) where N =< 0 => Sols.
  dropN([],_) => [].
  dropN([_,..Rest],N) => dropN(Rest,N-1).

  takeLimit:(solutions,option[integer]) => solutions.
  takeLimit(Sols,.none) => Sols.
  takeLimit(Sols,.some(N)) => takeN(Sols,N).

  takeN:(solutions,integer) => solutions.
  takeN(_,N) where N =< 0 => [].
  takeN([],_) => [].
  takeN([M,..Rest],N) => [M,..takeN(Rest,N-1)].
}
